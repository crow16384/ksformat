#' Utilities for ksformat Package
#'
#' Internal helper functions for format creation and manipulation.
#'
#' @name utilities
#' @keywords internal
NULL


# ===========================================================================
# Value type constants and helpers
# ===========================================================================

#' Recognised value types for ks_format objects.
#' These types store native R objects instead of character labels.
#' @keywords internal
#' @noRd
.value_types <- c("Date", "POSIXct", "logical")

#' Check whether a type string is a value type
#' @keywords internal
#' @noRd
.is_value_type <- function(type) {
  type %in% .value_types
}

#' Return a typed NA for the given value type
#' @keywords internal
#' @noRd
.typed_na <- function(type) {
  switch(type,
    Date    = as.Date(NA_real_, origin = "1970-01-01"),
    POSIXct = as.POSIXct(NA_real_, origin = "1970-01-01"),
    logical = NA,
    NA
  )
}

#' Extract values from a ks_format mapping list as a typed vector
#'
#' Uses \code{do.call(c, ...)} which preserves Date/POSIXct class.
#' Returns NULL for empty mappings.
#' @keywords internal
#' @noRd
.typed_map_values <- function(mappings) {
  if (length(mappings) == 0L) return(NULL)
  do.call(c, unname(mappings))
}

#' Parse a Date range key like "2020-01-01,2025-12-31,TRUE,FALSE"
#'
#' @param key Character string to parse.
#' @param date_format Optional strptime format for parsing date bounds.
#' @return A list with components low, high (Date), inc_low, inc_high (logical),
#'   or NULL if the key does not look like a date range.
#' @keywords internal
#' @noRd
.parse_date_range_key <- function(key, date_format = NULL) {
  parts <- strsplit(key, ",", fixed = TRUE)[[1]]
  if (length(parts) < 2L || length(parts) > 4L) return(NULL)

  low_str  <- trimws(parts[1])
  high_str <- trimws(parts[2])

  # Parse bounds
  parse_bound <- function(s, is_low) {
    su <- toupper(s)
    if (su == "LOW")  return(as.Date(-Inf, origin = "1970-01-01"))
    if (su == "HIGH") return(as.Date(Inf,  origin = "1970-01-01"))
    if (!is.null(date_format)) {
      d <- as.Date(s, format = date_format)
    } else {
      d <- suppressWarnings(as.Date(s))
    }
    if (is.na(d)) return(NULL)
    d
  }

  low  <- parse_bound(low_str,  TRUE)
  high <- parse_bound(high_str, FALSE)
  if (is.null(low) || is.null(high)) return(NULL)

  inc_low  <- TRUE
  inc_high <- FALSE
  if (length(parts) >= 3L) inc_low  <- toupper(trimws(parts[3])) == "TRUE"
  if (length(parts) >= 4L) inc_high <- toupper(trimws(parts[4])) == "TRUE"

  list(low = low, high = high, inc_low = inc_low, inc_high = inc_high)
}

#' Convert a typed value to a display string
#' @keywords internal
#' @noRd
.typed_value_to_string <- function(value, type, date_format = NULL) {
  if (is.na(value)) return("NA")
  switch(type,
    Date    = if (!is.null(date_format)) format(value, date_format) else as.character(value),
    POSIXct = if (!is.null(date_format)) format(value, date_format) else as.character(value),
    logical = as.character(value),
    as.character(value)
  )
}

#' Parse a string value into a native typed object
#' @keywords internal
#' @noRd
.parse_typed_value <- function(value_str, type, date_format = NULL) {
  switch(type,
    Date = {
      if (!is.null(date_format)) as.Date(value_str, format = date_format)
      else as.Date(value_str)
    },
    POSIXct = {
      if (!is.null(date_format)) as.POSIXct(value_str, format = date_format)
      else as.POSIXct(value_str)
    },
    logical = as.logical(value_str),
    value_str
  )
}


# ===========================================================================
# Expression label helpers
# ===========================================================================

#' Check if a label string is an R expression
#'
#' A label is treated as an expression if it references
#' \code{.x1}, \code{.x2}, etc.
#'
#' @param label Character string (the label/value side of a mapping)
#' @return Logical
#' @keywords internal
#' @noRd
.is_expr_label <- function(label) {
  grepl("\\.x\\d+", label)
}


#' Check if a label has the eval attribute
#'
#' @param label Character string (the label/value side of a mapping)
#' @return Logical
#' @keywords internal
#' @noRd
.has_eval_attr <- function(label) {
  isTRUE(attr(label, "eval"))
}


#' Evaluate an expression label with extra arguments
#'
#' Parses the expression string and evaluates it in an environment
#' containing \code{.x1}, \code{.x2}, etc. subsetted to the given indices.
#'
#' @param expr_str Character. The expression string.
#' @param extra_args List. Positional arguments from \code{fput(x, fmt, ...)}.
#' @param indices Integer vector. Indices in the original vector that matched.
#' @return Character vector of evaluated results.
#' @keywords internal
#' @noRd
.eval_expr_label <- function(expr_str, extra_args, indices,
                             parent_env = baseenv()) {
  # Build .x1, .x2, ... bindings via list2env (single C-level call)
  n_args <- length(extra_args)
  if (n_args > 0L) {
    xn_vals <- vector("list", n_args)
    xn_names <- paste0(".x", seq_len(n_args))
    for (j in seq_len(n_args)) {
      av <- extra_args[[j]]
      xn_vals[[j]] <- if (length(av) == 1L) av else av[indices]
    }
    env <- list2env(setNames(xn_vals, xn_names), parent = parent_env)
  } else {
    env <- new.env(parent = parent_env)
  }

  # Use cached parsed expression to avoid repeat parse() calls
  parsed <- if (exists(expr_str, envir = .expr_parse_cache, inherits = FALSE)) {
    .expr_parse_cache[[expr_str]]
  } else {
    p <- tryCatch(
      parse(text = expr_str),
      error = function(e) {
        cli_warn(c(
          "Expression parse failed: {conditionMessage(e)}",
          "i" = "Expression: {expr_str}"
        ))
        NULL
      }
    )
    if (!is.null(p)) assign(expr_str, p, envir = .expr_parse_cache)
    p
  }
  if (is.null(parsed)) return(rep(NA_character_, length(indices)))

  result <- tryCatch(
    as.character(eval(parsed, envir = env)),
    error = function(e) {
      cli_warn(c(
        "Expression evaluation failed: {conditionMessage(e)}",
        "i" = "Expression: {expr_str}"
      ))
      rep(NA_character_, length(indices))
    }
  )

  # Ensure result length matches indices
  if (length(result) == 0L) {
    cli_warn("Expression returned empty result, using NA. Expression: {expr_str}")
    result <- rep(NA_character_, length(indices))
  } else if (length(result) != length(indices)) {
    if (length(result) == 1L) {
      result <- rep(result, length(indices))
    } else {
      cli_warn("Expression returned {length(result)} values, expected {length(indices)}. Recycling/truncating.")
      result <- rep_len(result, length(indices))
    }
  }

  result
}


#' Check if Value is Missing
#'
#' Element-wise check for missing values including NA and NaN.
#' Optionally treats empty strings as missing.
#'
#' @param x Value to check
#'
#' @return Logical vector. NULL input returns \code{logical(0)}.
#'
#' @export
#'
#' @examples
#' is_missing(NA)          # TRUE
#' is_missing(NaN)         # TRUE
#' is_missing("")          # TRUE
#' is_missing("text")      # FALSE
#' is_missing(c(1, NA, NaN)) # FALSE TRUE TRUE
is_missing <- function(x) {
  if (is.null(x)) return(logical(0))
  out <- is.na(x)
  if (is.numeric(x)) return(out | is.nan(x))
  out | !nzchar(x)
}

#' Create Range Specification
#'
#' Helper function to create range specifications for numeric formats.
#'
#' @param low Numeric. Lower bound of the range.
#' @param high Numeric. Upper bound of the range.
#' @param label Character. Label for values in this range.
#' @param inc_low Logical. If \code{TRUE} (default), the lower bound is inclusive
#'   (>=). If \code{FALSE}, exclusive (>).
#' @param inc_high Logical. If \code{TRUE}, the upper bound is inclusive (<=).
#'   If \code{FALSE} (default), exclusive (<).
#'
#' @return A \code{range_spec} object (list with low, high, label, inc_low, inc_high).
#'
#' @details
#' By default, ranges are half-open: \code{[low, high)} — the lower bound is
#' included and the upper bound is excluded. This matches 'SAS' PROC FORMAT
#' range semantics and prevents overlap between adjacent ranges.
#'
#' @export
#'
#' @examples
#' range_spec(0, 18, "Child")          # [0, 18)
#' range_spec(18, 65, "Adult")         # [18, 65)
#' range_spec(65, Inf, "Senior", inc_high = TRUE)  # [65, Inf]
range_spec <- function(low, high, label, inc_low = TRUE, inc_high = FALSE) {
  if (!is.numeric(low) || !is.numeric(high)) {
    cli_abort("Range bounds must be numeric.")
  }

  if (low > high) {
    cli_abort("Lower bound must be less than or equal to upper bound.")
  }

  structure(
    list(
      low = low,
      high = high,
      label = as.character(label),
      inc_low = inc_low,
      inc_high = inc_high
    ),
    class = "range_spec"
  )
}

#' Check if Value Falls in Range
#'
#' @param x Numeric value
#' @param range_spec Range specification
#'
#' @return Logical
#' @keywords internal
#' @noRd
in_range <- function(x, range_spec) {
  if (!inherits(range_spec, "range_spec")) {
    return(FALSE)
  }
  if (length(x) == 1L && is.na(x)) return(FALSE)

  low_ok <- if (range_spec$inc_low) x >= range_spec$low else x > range_spec$low
  high_ok <- if (range_spec$inc_high) x <= range_spec$high else x < range_spec$high
  low_ok & high_ok
}


#' Parse a range key string into components
#'
#' Parses range keys stored in format mappings. Supports two formats:
#' \itemize{
#'   \item 4-part: \code{"low,high,inc_low,inc_high"} (e.g. \code{"0,18,TRUE,FALSE"})
#'   \item 2-part (legacy): \code{"low,high"} (e.g. \code{"0,18"}) — defaults to \code{[low, high)}
#' }
#'
#' @param key Character. The mapping key to parse.
#' @return A list with \code{low}, \code{high}, \code{inc_low}, \code{inc_high},
#'   or \code{NULL} if the key is not a valid range.
#' @keywords internal
#' @noRd
.parse_range_key <- function(key) {
  parts <- strsplit(key, ",")[[1]]

  if (length(parts) == 4) {
    low <- suppressWarnings(as.numeric(parts[1]))
    high <- suppressWarnings(as.numeric(parts[2]))
    inc_low <- as.logical(parts[3])
    inc_high <- as.logical(parts[4])
    if (!is.na(low) && !is.na(high) && !is.na(inc_low) && !is.na(inc_high)) {
      return(list(low = low, high = high, inc_low = inc_low, inc_high = inc_high))
    }
  }

  if (length(parts) == 2) {
    low <- suppressWarnings(as.numeric(parts[1]))
    high <- suppressWarnings(as.numeric(parts[2]))
    if (!is.na(low) && !is.na(high)) {
      return(list(low = low, high = high, inc_low = TRUE, inc_high = FALSE))
    }
  }

  NULL
}

#' Parse Cache for Expression Labels
#'
#' Caches parsed R expressions (from \code{parse()}) keyed by the expression
#' string to avoid repeated parsing on successive \code{fput()} calls.
#'
#' @keywords internal
.expr_parse_cache <- new.env(hash = TRUE, parent = emptyenv())

#' Format Library (Global Environment for Storing Formats)
#'
#' @keywords internal
.format_library <- new.env(parent = emptyenv())

#' Register Format in Library (Internal)
#'
#' Stores a format definition in the global format library.
#'
#' @param format A ks_format or ks_invalue object
#' @param name Character. Name to store format under (uses format$name if NULL)
#' @param overwrite Logical. Allow overwriting existing format. Default TRUE.
#'
#' @return Invisibly returns the format object
#' @keywords internal
#' @noRd
.format_register <- function(format, name = NULL, overwrite = TRUE) {
  if (!inherits(format, c("ks_format", "ks_invalue"))) {
    cli_abort("{.arg format} must be a {.cls ks_format} or {.cls ks_invalue} object.")
  }

  if (is.null(name)) {
    name <- format$name
  }

  if (is.null(name) || is.na(name) || !nzchar(name)) {
    return(invisible(format))
  }

  if (exists(name, envir = .format_library) && !overwrite) {
    cli_abort("Format {.val {name}} already exists. Use {.code overwrite = TRUE} to replace.")
  }

  assign(name, format, envir = .format_library)
  invisible(format)
}

#' Retrieve Format from Library (Internal)
#'
#' @param name Character. Name of the format to retrieve
#' @return A ks_format or ks_invalue object
#' @keywords internal
#' @noRd
.format_get <- function(name) {
  if (!exists(name, envir = .format_library)) {
    # Case-insensitive lookup: try matching available names
    available <- ls(envir = .format_library)
    ci_match <- available[tolower(available) == tolower(name)]
    if (length(ci_match) == 1L) {
      return(get(ci_match, envir = .format_library))
    }
    if (length(ci_match) > 1L) {
      cli_abort(c(
        "Ambiguous format name {.val {name}}: multiple case-insensitive matches found.",
        "i" = "Matches: {.val {ci_match}}"
      ))
    }

    # Fallback: check if it's a built-in SAS datetime format
    if (.is_sas_datetime_format(name)) {
      fmt <- fnew_date(pattern = name, name = name)
      return(fmt)
    }

    if (length(available) == 0) {
      cli_abort(c(
        "Format {.val {name}} not found.",
        "i" = "Format library is empty."
      ))
    }
    cli_abort(c(
      "Format {.val {name}} not found.",
      "i" = "Available: {.val {available}}"
    ))
  }
  get(name, envir = .format_library)
}

#' Retrieve a Format from the Library
#'
#' Returns a format or invalue object by name. Used when you need the object
#' (e.g. for \code{\link{fput_df}} or \code{\link{fexport}}) rather than
#' applying by name with \code{\link{fput}}, \code{\link{fputn}}, or
#' \code{\link{fputc}}.
#'
#' @param name Character. Name of a registered format or invalue.
#' @return A \code{ks_format} or \code{ks_invalue} object.
#' @export
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' sex_fmt <- format_get("sex")
#' fput_df(data.frame(sex = c("M", "F")), sex = sex_fmt)
#' fclear()
format_get <- function(name) {
  .format_get(name)
}

#' Print Format(s) from Library
#'
#' Displays format information from the global format library.
#' When called without arguments, lists all registered format names.
#' When called with a name, displays the full definition of that format.
#'
#' @param name Character. Optional name of a specific format to display.
#'   If \code{NULL} (default), lists all registered formats.
#'
#' @return Invisible \code{NULL}. This function is for display only.
#'
#' @export
#'
#' @seealso \code{\link{flist}} for a programmatic alternative that returns
#'   a character vector of registered names.
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' flist()          # character vector of names
#' fprint()         # list all formats
#' fprint("sex")    # show specific format
#' fclear()
fprint <- function(name = NULL) {
  if (is.null(name)) {
    fnames <- ls(envir = .format_library)
    if (length(fnames) == 0) {
      cat("Format library is empty\n")
    } else {
      cat("Registered formats:\n")
      objs <- mget(fnames, envir = .format_library)
      for (nm in fnames) {
        obj <- objs[[nm]]
        type_str <- if (inherits(obj, "ks_format")) {
          if (obj$type %in% c("date", "time", "datetime")) {
            sas_info <- if (!is.null(obj$sas_name)) paste0(", ", obj$sas_name, ".") else ""
            paste0("VALUE (", obj$type, sas_info, ")")
          } else {
            paste0("VALUE (", obj$type, ")")
          }
        } else if (inherits(obj, "ks_invalue")) {
          paste0("INVALUE (", obj$target_type, ")")
        } else {
          "unknown"
        }
        n_mappings <- length(obj$mappings)
        cat("  ", nm, " - ", type_str, ", ", n_mappings, " mapping(s)\n", sep = "")
      }
    }
  } else {
    if (!exists(name, envir = .format_library)) {
      cli_abort(c(
        "Format {.val {name}} not found.",
        "i" = "Use {.fn fprint} to see available formats."
      ))
    }
    obj <- get(name, envir = .format_library)
    print(obj)
  }
  invisible(NULL)
}

#' List Format Names from Library
#'
#' Returns a character vector of all format and invalue names currently
#' registered in the global format library.
#'
#' @return A character vector of registered format names, sorted alphabetically.
#'   Returns \code{character(0)} if the library is empty.
#'
#' @export
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' flist()
#' fclear()
flist <- function() {
  sort(ls(envir = .format_library))
}

#' Remove Format(s) from Library
#'
#' Removes one or all formats from the global format library.
#' When called without arguments, clears all formats.
#' When called with a name, removes only that format.
#'
#' @param name Character. Optional name of a specific format to remove.
#'   If \code{NULL} (default), removes all formats.
#'
#' @return Invisible \code{NULL}
#'
#' @export
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' fclear("sex")   # remove one format
#' fclear()        # remove all formats
fclear <- function(name = NULL) {
  if (is.null(name)) {
    nms <- ls(envir = .format_library)
    if (length(nms) > 0) {
      rm(list = nms, envir = .format_library)
    }
    cli_inform("All formats cleared from library.")
  } else {
    if (!exists(name, envir = .format_library)) {
      cli_warn("Format {.val {name}} not found in library.")
      return(invisible(NULL))
    }
    rm(list = name, envir = .format_library)
    cli_inform("Format {.val {name}} removed from library.")
  }
  invisible(NULL)
}

#' Validate Format Object (Internal)
#'
#' Validates the structure and content of a format object during
#' creation or parsing. Provides clear error descriptions for the user.
#'
#' @param format_obj A ks_format or ks_invalue object to validate
#' @return Invisibly returns the format object if valid
#' @keywords internal
#' @noRd
.format_validate <- function(format_obj) {
  if (inherits(format_obj, "ks_format")) {
    name_str <- if (!is.null(format_obj$name)) {
      paste0(" '", format_obj$name, "'")
    } else {
      ""
    }

    # Check that all labels are character strings (not for value types)
    if (length(format_obj$mappings) > 0 && !.is_value_type(format_obj$type)) {
      labels <- unlist(format_obj$mappings)
      non_char <- !vapply(labels, is.character, logical(1L))
      if (any(non_char)) {
        bad_idx <- which(non_char)
        bad_keys <- names(format_obj$mappings)[bad_idx]
        cli_abort(c(
          "Format{name_str}: All labels must be character strings.",
          "x" = "Non-character labels found for keys: {.val {bad_keys}}"
        ))
      }
    }

    # Check for duplicate keys
    keys <- names(format_obj$mappings)
    if (anyDuplicated(keys)) {
      dupes <- unique(keys[duplicated(keys)])
      cli_warn("Format{name_str}: Duplicate keys: {.val {dupes}}")
    }

    # For numeric type, validate that keys are valid numbers or ranges
    if (format_obj$type == "numeric" && length(format_obj$mappings) > 0) {
      for (key in keys) {
        parsed <- .parse_range_key(key)
        if (is.null(parsed) && is.na(suppressWarnings(as.numeric(key)))) {
          cli_warn("Format{name_str}: Key {.val {key}} is not a valid numeric value or range.")
        }
      }
    }

  } else if (inherits(format_obj, "ks_invalue")) {
    name_str <- if (!is.null(format_obj$name)) {
      paste0(" '", format_obj$name, "'")
    } else {
      ""
    }

    # Check for duplicate keys
    if (length(format_obj$mappings) > 0) {
      keys <- names(format_obj$mappings)
      if (anyDuplicated(keys)) {
        dupes <- unique(keys[duplicated(keys)])
        cli_warn("Invalue{name_str}: Duplicate keys: {.val {dupes}}")
      }
    }
  }

  invisible(format_obj)
}


# ===========================================================================
# Named vector expansion helper
# ===========================================================================

#' Expand Named Vectors in \code{list(...)} Output
#'
#' Internal helper that detects unnamed elements in a \code{list(...)} result
#' that are named atomic vectors or named lists, and expands them into
#' individual key-value pairs. This allows \code{fnew()}, \code{finput()}, and
#' \code{fnew_bid()} to accept named vectors alongside individual \code{...}
#' arguments.
#'
#' @param dots_list List. The result of \code{list(...)}.
#' @param reverse Logical. If \code{TRUE}, swap names and values for expanded
#'   entries (except \code{.missing} and \code{.other} directives).
#'   Used by \code{fnew()} for character/numeric types where the named-vector
#'   convention \code{c(Label = "Code")} is reversed relative to the internal
#'   \code{"Code" = "Label"} representation. Set to \code{FALSE} for value
#'   types (\code{Date}, \code{POSIXct}, \code{logical}) where names are
#'   already the input keys and values are native R objects. See
#'   \code{fnew()} details and \code{vignette("usage_examples")} Example 21.
#'   Default \code{FALSE}.
#' @return A flat named list with the same structure as a standard
#'   \code{list(...)} output.
#' @keywords internal
#' @noRd
.expand_named_vectors <- function(dots_list, reverse = FALSE) {
  if (length(dots_list) == 0L) return(dots_list)

  outer_names <- names(dots_list)
  result <- list()

  for (i in seq_along(dots_list)) {
    nm <- if (!is.null(outer_names)) outer_names[i] else ""
    elem <- dots_list[[i]]

    # Unnamed element that is a named vector/list -> expand
    if (is.na(nm) || !nzchar(nm)) {
      elem_names <- names(elem)

      if (is.null(elem_names) || !all(nzchar(elem_names))) {
        cli_abort("Unnamed argument at position {i} must be a fully named vector or list.")
      }

      for (j in seq_along(elem)) {
        key <- elem_names[j]
        val <- elem[[j]]

        if (reverse && !key %in% c(".missing", ".other")) {
          # Swap: name becomes value, value becomes name
          entry <- stats::setNames(list(key), as.character(val))
        } else {
          entry <- stats::setNames(list(val), key)
        }
        result <- c(result, entry)
      }
    } else {
      # Regular named scalar argument -> keep as-is
      entry <- stats::setNames(list(elem), nm)
      result <- c(result, entry)
    }
  }

  result
}
