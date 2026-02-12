#' Utilities for ksformat Package
#'
#' Internal helper functions for format creation and manipulation.
#'
#' @name utilities
#' @keywords internal
NULL

#' Check if Value is Missing
#'
#' Comprehensive check for missing values including NA, NULL, NaN, and optionally empty strings.
#'
#' @param x Value to check
#' @param include_empty Logical. If TRUE, treat empty strings as missing
#'
#' @return Logical vector
#'
#' @export
#'
#' @examples
#' is_missing_value(NA)          # TRUE
#' is_missing_value(NULL)        # TRUE (length 0)
#' is_missing_value(NaN)         # TRUE
#' is_missing_value("")          # FALSE
#' is_missing_value("", TRUE)    # TRUE
is_missing_value <- function(x, include_empty = FALSE) {
  if (is.null(x)) {
    return(logical(0))
  }

  result <- is.na(x) | is.nan(x)

  if (include_empty && is.character(x)) {
    result <- result | (x == "")
  }

  return(result)
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

  low_ok <- if (range_spec$inc_low) x >= range_spec$low else x > range_spec$low
  high_ok <- if (range_spec$inc_high) x <= range_spec$high else x < range_spec$high

  return(low_ok & high_ok)
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

  return(NULL)
}

#' Format Library (Global Environment for Storing Formats)
#'
#' @keywords internal
.format_library <- new.env(parent = emptyenv())

#' Register Format in Library
#'
#' Stores a format definition in the global format library for later retrieval.
#'
#' @param format A ks_format or ks_invalue object
#' @param name Character. Name to store format under (uses format$name if NULL)
#' @param overwrite Logical. Allow overwriting existing format
#'
#' @return Invisibly returns the format object
#'
#' @export
#'
#' @examples
#' sex_fmt <- format_create("M" = "Male", "F" = "Female", name = "sex")
#' format_register(sex_fmt)
#'
#' # Later retrieve it
#' fmt <- format_get("sex")
format_register <- function(format, name = NULL, overwrite = FALSE) {
  if (!inherits(format, c("ks_format", "ks_invalue"))) {
    stop("format must be a ks_format or ks_invalue object")
  }

  if (is.null(name)) {
    name <- format$name
  }

  if (is.null(name) || name == "") {
    stop("Format must have a name to be registered")
  }

  if (exists(name, envir = .format_library) && !overwrite) {
    stop("Format '", name, "' already exists. Use overwrite = TRUE to replace.")
  }

  assign(name, format, envir = .format_library)

  message("Format '", name, "' registered successfully")
  invisible(format)
}

#' Retrieve Format from Library
#'
#' Gets a format definition from the global format library.
#'
#' @param name Character. Name of the format to retrieve
#'
#' @return A ks_format or ks_invalue object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sex_fmt <- format_get("sex")
#' }
format_get <- function(name) {
  if (!exists(name, envir = .format_library)) {
    stop("Format '", name, "' not found in library. Available formats: ",
         paste(format_list(), collapse = ", "))
  }
  
  return(get(name, envir = .format_library))
}

#' List Available Formats in Library
#'
#' Returns names of all formats stored in the global format library.
#'
#' @return Character vector of format names
#'
#' @export
#'
#' @examples
#' format_list()
format_list <- function() {
  return(ls(envir = .format_library))
}

#' Remove Format from Library
#'
#' Removes a format from the global format library.
#'
#' @param name Character. Name of format to remove
#'
#' @return Logical indicating success
#'
#' @export
#'
#' @examples
#' \dontrun{
#' format_remove("sex")
#' }
format_remove <- function(name) {
  if (!exists(name, envir = .format_library)) {
    warning("Format '", name, "' not found in library")
    return(FALSE)
  }

  rm(list = name, envir = .format_library)
  message("Format '", name, "' removed from library")
  return(TRUE)
}

#' Clear All Formats from Library
#'
#' Removes all formats from the global format library.
#'
#' @return Invisible NULL
#'
#' @export
format_clear <- function() {
  rm(list = ls(envir = .format_library), envir = .format_library)
  message("All formats cleared from library")
  invisible(NULL)
}

#' Validate Vector Against Format
#'
#' Checks which values in a vector match format definitions.
#'
#' @param x Vector to validate
#' @param format A ks_format object
#'
#' @return Data frame with columns: value, matched, label
#'
#' @export
#'
#' @examples
#' sex_fmt <- format_create("M" = "Male", "F" = "Female")
#' format_validate(c("M", "F", "X", NA), sex_fmt)
format_validate <- function(x, format) {
  if (!inherits(format, "ks_format")) {
    stop("format must be a ks_format object")
  }

  formatted <- format_apply(x, format)

  # Check which values were actually matched vs using .other label
  matched <- rep(FALSE, length(x))

  # Pre-parse range keys for numeric formats
  range_entries <- list()
  if (format$type == "numeric") {
    for (i in seq_along(format$mappings)) {
      key <- names(format$mappings)[i]
      parsed <- .parse_range_key(key)
      if (!is.null(parsed)) {
        range_entries <- c(range_entries, list(parsed))
      }
    }
  }

  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      matched[i] <- !is.null(format$missing_label)
    } else {
      # Check exact match
      if (as.character(x[i]) %in% names(format$mappings)) {
        matched[i] <- TRUE
      } else if (format$type == "numeric" && is.numeric(x[i])) {
        # Check range match
        for (re in range_entries) {
          low_ok <- if (re$inc_low) x[i] >= re$low else x[i] > re$low
          high_ok <- if (re$inc_high) x[i] <= re$high else x[i] < re$high
          if (low_ok && high_ok) {
            matched[i] <- TRUE
            break
          }
        }
      }
    }
  }

  return(data.frame(
    value = x,
    matched = matched,
    label = formatted,
    stringsAsFactors = FALSE
  ))
}
