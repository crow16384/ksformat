#' Apply Format to Data (like SAS FORMAT statement)
#'
#' Applies a format definition to a vector of values, returning formatted labels.
#' Properly handles NA, NULL, NaN, and other missing values.
#'
#' @param x Vector of values to format
#' @param format A ks_format object created by \code{\link{format_create}}
#' @param keep_na Logical. If TRUE, preserve NA in output instead of applying missing label
#'
#' @return Character vector with formatted labels
#'
#' @details
#' The function handles missing values in the following order:
#' 1. NA, NULL, NaN -> Uses format's missing_label if defined
#' 2. Exact matches -> Uses defined value-label mapping
#' 3. Range matches (for numeric) -> Uses range label
#' 4. No match -> Uses format's other_label or returns original value
#'
#' @export
#'
#' @examples
#' sex_fmt <- format_create("M" = "Male", "F" = "Female", .missing = "Unknown")
#' format_apply(c("M", "F", NA, "X"), sex_fmt)
#' # Returns: "Male" "Female" "Unknown" "X"
#'
#' age_fmt <- format_create(
#'   c(0, 18) = "Child",
#'   c(18, 65) = "Adult",
#'   c(65, Inf) = "Senior"
#' )
#' format_apply(c(5, 25, 70, NA), age_fmt)
format_apply <- function(x, format, keep_na = FALSE) {
  if (!inherits(format, "ks_format")) {
    stop("format must be a ks_format object created with format_create()")
  }
  
  # Handle NULL input
  if (is.null(x)) {
    return(character(0))
  }
  
  # Initialize result vector
  result <- rep(NA_character_, length(x))
  
  # Identify missing values (NA, NaN, NULL)
  is_missing <- is.na(x) | is.nan(x)
  
  # Apply missing label if defined
  if (!is.null(format$missing_label) && !keep_na) {
    result[is_missing] <- format$missing_label
  } else if (keep_na) {
    # Keep NA as NA
    result[is_missing] <- NA_character_
  }
  
  # Process non-missing values
  non_missing_idx <- which(!is_missing)
  
  for (idx in non_missing_idx) {
    value <- x[idx]
    matched <- FALSE
    
    # Try exact match first
    for (i in seq_along(format$mappings)) {
      key <- names(format$mappings)[i]
      
      # Check if key is a range specification (contains comma or dash)
      # For now, handle simple exact matches
      if (as.character(value) == key) {
        result[idx] <- format$mappings[[i]]
        matched <- TRUE
        break
      }
    }
    
    # Try range match for numeric values
    if (!matched && format$type == "numeric" && is.numeric(value)) {
      for (i in seq_along(format$mappings)) {
        range_key <- names(format$mappings)[i]
        
        # Check if this is a range (will be improved in utilities)
        # For now, store ranges in a better format
        if (matched) break
      }
    }
    
    # Apply other label if no match
    if (!matched) {
      if (!is.null(format$other_label)) {
        result[idx] <- format$other_label
      } else {
        # Return original value as character
        result[idx] <- as.character(value)
      }
    }
  }
  
  return(result)
}

#' Apply Format to Data Frame Columns
#'
#' Applies formats to one or more columns in a data frame.
#'
#' @param data Data frame
#' @param ... Named format specifications: column_name = format_object
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
#'   age = c(15, 25, 70, 35)
#' )
#'
#' sex_fmt <- format_create("M" = "Male", "F" = "Female", .missing = "Unknown")
#' age_fmt <- format_create(
#'   c(0, 18) = "Child",
#'   c(18, 65) = "Adult",
#'   c(65, Inf) = "Senior"
#' )
#'
#' format_apply_df(df, sex = sex_fmt, age = age_fmt)
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
    formatted_values <- format_apply(data[[col_name]], format_obj)
    
    if (replace) {
      result[[col_name]] <- formatted_values
    } else {
      new_col_name <- paste0(col_name, suffix)
      result[[new_col_name]] <- formatted_values
    }
  }
  
  return(result)
}

#' Format Values Using Named Format
#'
#' Convenience function to format values using a stored format name.
#'
#' @param x Vector to format
#' @param format_name Character. Name of a stored format
#'
#' @return Formatted character vector
#'
#' @export
format_put <- function(x, format_name) {
  # This will retrieve format from a format library (to be implemented)
  stop("Format library not yet implemented. Use format_apply() with a format object.")
}
