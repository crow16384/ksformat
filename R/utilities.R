#' Utilities for ksformat Package
#'
#' Internal helper functions for format creation and manipulation.
#'
#' @name utilities
#' @keywords internal
NULL


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
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
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

    # Check that all labels are character strings
    if (length(format_obj$mappings) > 0) {
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
