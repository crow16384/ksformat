#' Utilities for ksformat Package
#'
#' Internal helper functions for format creation and manipulation.
#'
#' @name utilities
#' @keywords internal
NULL

#' Check if Value is Missing
#'
#' Element-wise check for missing values including NA and NaN.
#' Optionally treats empty strings as missing.
#'
#' @param x Value to check
#' @param include_empty Logical. If \code{TRUE}, treat empty strings as missing.
#'   Default is \code{FALSE}.
#'
#' @return Logical vector. NULL input returns \code{logical(0)}.
#'
#' @export
#'
#' @examples
#' is_missing(NA)          # TRUE
#' is_missing(NaN)         # TRUE
#' is_missing("")          # FALSE
#' is_missing("", include_empty = TRUE)  # TRUE
#' is_missing("text")      # FALSE
is_missing <- function(x, include_empty = FALSE) {
  if (is.null(x)) return(logical(0))
  result <- is.na(x)
  if (include_empty) {
    result <- result | (!result & as.character(x) == "")
  }
  result
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
#' included and the upper bound is excluded. This matches SAS PROC FORMAT
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
    stop("Range bounds must be numeric")
  }

  if (low > high) {
    stop("Lower bound must be less than or equal to upper bound")
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

  low_ok <- ifelse(range_spec$inc_low, x >= range_spec$low, x > range_spec$low)
  high_ok <- ifelse(range_spec$inc_high, x <= range_spec$high, x < range_spec$high)

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
    stop("format must be a ks_format or ks_invalue object")
  }

  if (is.null(name)) {
    name <- format$name
  }

  if (is.null(name) || name == "") {
    return(invisible(format))
  }

  if (exists(name, envir = .format_library) && !overwrite) {
    stop("Format '", name, "' already exists. Use overwrite = TRUE to replace.")
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
    # Fallback: check if it's a built-in SAS datetime format
    if (.is_sas_datetime_format(name)) {
      fmt <- fnew_date(pattern = name, name = name)
      return(fmt)
    }
    available <- ls(envir = .format_library)
    if (length(available) == 0) {
      stop("Format '", name, "' not found. Format library is empty.")
    }
    stop("Format '", name, "' not found. Available: ",
         paste(available, collapse = ", "))
  }
  get(name, envir = .format_library)
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
      for (nm in fnames) {
        obj <- get(nm, envir = .format_library)
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
      stop("Format '", name, "' not found. Use fprint() to see available formats.")
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
    message("All formats cleared from library")
  } else {
    if (!exists(name, envir = .format_library)) {
      warning("Format '", name, "' not found in library")
      return(invisible(NULL))
    }
    rm(list = name, envir = .format_library)
    message("Format '", name, "' removed from library")
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
      non_char <- !sapply(labels, is.character)
      if (any(non_char)) {
        bad_idx <- which(non_char)
        bad_keys <- names(format_obj$mappings)[bad_idx]
        stop("Format", name_str, ": All labels must be character strings. ",
             "Non-character labels found for keys: ",
             paste(bad_keys, collapse = ", "))
      }
    }

    # Check for duplicate keys
    keys <- names(format_obj$mappings)
    if (anyDuplicated(keys)) {
      dupes <- unique(keys[duplicated(keys)])
      warning("Format", name_str, ": Duplicate keys: ",
              paste(dupes, collapse = ", "))
    }

    # For numeric type, validate that keys are valid numbers or ranges
    if (format_obj$type == "numeric" && length(format_obj$mappings) > 0) {
      for (key in keys) {
        parsed <- .parse_range_key(key)
        if (is.null(parsed) && is.na(suppressWarnings(as.numeric(key)))) {
          warning("Format", name_str, ": Key '", key,
                  "' is not a valid numeric value or range")
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
        warning("Invalue", name_str, ": Duplicate keys: ",
                paste(dupes, collapse = ", "))
      }
    }
  }

  invisible(format_obj)
}
