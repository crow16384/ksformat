#' Apply Format to Data (like SAS PUT function)
#'
#' Applies a format definition to a vector of values, returning formatted labels.
#' Properly handles NA, NULL, NaN, and other missing values.
#'
#' @param x Vector of values to format
#' @param format A \code{ks_format} object or a character string naming a format
#'   in the global format library.
#' @param keep_na Logical. If TRUE, preserve NA in output instead of applying
#'   missing label.
#'
#' @return Character vector with formatted labels
#'
#' @details
#' The function handles missing values in the following order:
#' \enumerate{
#'   \item NA, NULL, NaN -> Uses format's missing_label if defined
#'   \item Exact matches -> Uses defined value-label mapping
#'   \item Range matches (for numeric) -> Uses range label
#'   \item No match -> Uses format's other_label or returns original value
#' }
#'
#' @export
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")
#' fput(c("M", "F", NA, "X"), "sex")
#' # Returns: "Male" "Female" "Unknown" "X"
#' fclear()
fput <- function(x, format, keep_na = FALSE) {
  # Resolve format by name if string provided
  if (is.character(format) && length(format) == 1) {
    format <- .format_get(format)
  }

  if (!inherits(format, "ks_format")) {
    stop("format must be a ks_format object or a registered format name")
  }

  # Handle NULL input
  if (is.null(x)) {
    return(character(0))
  }

  # Initialize result vector
  result <- rep(NA_character_, length(x))

  # Identify missing values (NA, NaN)
  is_miss <- is.na(x) | is.nan(x)

  # Apply missing label if defined
  if (!is.null(format$missing_label) && !keep_na) {
    result[is_miss] <- format$missing_label
  } else if (keep_na) {
    result[is_miss] <- NA_character_
  }

  # Pre-parse range keys for numeric formats
  range_entries <- list()
  if (format$type == "numeric") {
    for (i in seq_along(format$mappings)) {
      key <- names(format$mappings)[i]
      parsed <- .parse_range_key(key)
      if (!is.null(parsed)) {
        range_entries <- c(range_entries, list(list(
          idx = i,
          low = parsed$low,
          high = parsed$high,
          inc_low = parsed$inc_low,
          inc_high = parsed$inc_high,
          label = format$mappings[[i]]
        )))
      }
    }
  }

  # Process non-missing values
  non_missing_idx <- which(!is_miss)

  for (idx in non_missing_idx) {
    value <- x[idx]
    matched <- FALSE

    # Try exact match first
    for (i in seq_along(format$mappings)) {
      key <- names(format$mappings)[i]

      if (as.character(value) == key) {
        result[idx] <- format$mappings[[i]]
        matched <- TRUE
        break
      }
    }

    # Try range match for numeric values
    if (!matched && format$type == "numeric" && is.numeric(value)) {
      for (re in range_entries) {
        low_ok <- if (re$inc_low) value >= re$low else value > re$low
        high_ok <- if (re$inc_high) value <= re$high else value < re$high
        if (low_ok && high_ok) {
          result[idx] <- re$label
          matched <- TRUE
          break
        }
      }
    }

    # Apply other label if no match
    if (!matched) {
      if (!is.null(format$other_label)) {
        result[idx] <- format$other_label
      } else {
        result[idx] <- as.character(value)
      }
    }
  }

  return(result)
}

#' Apply Numeric Format by Name (like SAS PUTN)
#'
#' Looks up a numeric VALUE format by name from the global format library
#' and applies it to the input vector.
#'
#' @param x Numeric vector of values to format
#' @param format_name Character. Name of a registered numeric format.
#'
#' @return Character vector with formatted labels
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fparse(text = '
#' VALUE age (numeric)
#'   [0, 18) = "Child"
#'   [18, 65) = "Adult"
#'   [65, HIGH] = "Senior"
#' ;
#' ')
#' fputn(c(5, 25, 70), "age")
#' }
fputn <- function(x, format_name) {
  format_obj <- .format_get(format_name)
  if (!inherits(format_obj, "ks_format")) {
    stop("'", format_name, "' is not a VALUE format (ks_format)")
  }
  if (format_obj$type != "numeric") {
    warning("Format '", format_name, "' is type '", format_obj$type,
            "', not 'numeric'")
  }
  fput(x, format_obj)
}

#' Apply Character Format by Name (like SAS PUTC)
#'
#' Looks up a character VALUE format by name from the global format library
#' and applies it to the input vector.
#'
#' @param x Character vector of values to format
#' @param format_name Character. Name of a registered character format.
#'
#' @return Character vector with formatted labels
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' fputc(c("M", "F"), "sex")
#' }
fputc <- function(x, format_name) {
  format_obj <- .format_get(format_name)
  if (!inherits(format_obj, "ks_format")) {
    stop("'", format_name, "' is not a VALUE format (ks_format)")
  }
  if (format_obj$type != "character") {
    warning("Format '", format_name, "' is type '", format_obj$type,
            "', not 'character'")
  }
  fput(x, format_obj)
}

#' Apply Format to Data Frame Columns
#'
#' Applies formats to one or more columns in a data frame.
#'
#' @param data Data frame
#' @param ... Named format specifications: \code{column_name = format_object_or_name}
#' @param suffix Character. Suffix to add to formatted column names (default: "_fmt")
#' @param replace Logical. If TRUE, replace original columns; if FALSE, create new columns
#'
#' @return Data frame with formatted columns
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   sex = c("M", "F", "M", NA),
#'   status = c("A", "I", "A", "P")
#' )
#'
#' fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex_fmt")
#' fnew("A" = "Active", "I" = "Inactive", "P" = "Pending", name = "status_fmt")
#'
#' sex_f <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
#' status_f <- fnew("A" = "Active", "I" = "Inactive", "P" = "Pending")
#' format_apply_df(df, sex = sex_f, status = status_f)
#' fclear()
format_apply_df <- function(data, ..., suffix = "_fmt", replace = FALSE) {
  formats <- list(...)

  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  result <- data

  for (col_name in names(formats)) {
    if (!col_name %in% names(data)) {
      warning("Column '", col_name, "' not found in data frame")
      next
    }

    format_obj <- formats[[col_name]]
    formatted_values <- fput(data[[col_name]], format_obj)

    if (replace) {
      result[[col_name]] <- formatted_values
    } else {
      new_col_name <- paste0(col_name, suffix)
      result[[new_col_name]] <- formatted_values
    }
  }

  return(result)
}
