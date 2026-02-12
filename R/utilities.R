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
#' @param low Numeric. Lower bound (inclusive)
#' @param high Numeric. Upper bound (inclusive)
#' @param label Character. Label for values in this range
#'
#' @return List with range specification
#'
#' @export
#'
#' @examples
#' range_spec(0, 18, "Child")
#' range_spec(18, 65, "Adult")
#' range_spec(65, Inf, "Senior")
range_spec <- function(low, high, label) {
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
      label = as.character(label)
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
  
  return(x >= range_spec$low & x <= range_spec$high)
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
  
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      matched[i] <- !is.null(format$missing_label)
    } else {
      matched[i] <- as.character(x[i]) %in% names(format$mappings)
    }
  }
  
  return(data.frame(
    value = x,
    matched = matched,
    label = formatted,
    stringsAsFactors = FALSE
  ))
}
