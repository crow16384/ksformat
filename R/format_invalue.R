#' Create Invalue Format (Reverse Formatting like SAS INVALUE)
#'
#' Creates an invalue format that converts formatted labels back to original values.
#' This is similar to SAS PROC FORMAT with INVALUE statement.
#'
#' @param ... Named arguments defining label-value mappings (reverse of format_create)
#' @param name Character. Optional name for the invalue format
#' @param target_type Character. Type to convert to: "numeric", "character", "integer", "logical"
#' @param missing_value Value to use for missing inputs (default: NA)
#'
#' @return An object of class "ks_invalue" containing the invalue definition
#'
#' @export
#'
#' @examples
#' # Convert text labels back to codes
#' sex_inv <- format_invalue(
#'   "Male" = "M",
#'   "Female" = "F",
#'   "Unknown" = NA
#' )
#'
#' # Convert age groups to numeric midpoints
#' age_inv <- format_invalue(
#'   "Child" = 10,
#'   "Adult" = 40,
#'   "Senior" = 75,
#'   target_type = "numeric"
#' )
format_invalue <- function(..., name = NULL, target_type = "auto", missing_value = NA) {
  mappings <- list(...)

  if (length(mappings) == 0) {
    stop("At least one label-value mapping must be provided")
  }

  # Determine target type
  if (target_type == "auto") {
    target_type <- detect_invalue_type(mappings)
  }

  # Create invalue object
  invalue_obj <- structure(
    list(
      name = name,
      target_type = target_type,
      mappings = mappings,
      missing_value = missing_value,
      created = Sys.time()
    ),
    class = "ks_invalue"
  )

  return(invalue_obj)
}

#' Detect Invalue Target Type
#'
#' @param mappings List of label-value mappings
#' @return Character: target type
#' @keywords internal
detect_invalue_type <- function(mappings) {
  values <- unlist(mappings)

  if (all(sapply(values, is.logical))) {
    return("logical")
  }

  if (all(sapply(values, is.numeric))) {
    if (all(sapply(values, function(x) x == as.integer(x)))) {
      return("integer")
    }
    return("numeric")
  }

  return("character")
}

#' Apply Invalue Format (Reverse Formatting)
#'
#' Applies an invalue format to convert formatted labels back to original values.
#'
#' @param x Character vector of labels to convert
#' @param invalue A ks_invalue object created by \code{\link{format_invalue}}
#' @param na_if Character vector. Additional values to treat as NA
#'
#' @return Vector with original values (type depends on invalue$target_type)
#'
#' @export
#'
#' @examples
#' sex_inv <- format_invalue("Male" = "M", "Female" = "F", "Unknown" = NA)
#' invalue_apply(c("Male", "Female", "Unknown", NA), sex_inv)
#' # Returns: "M" "F" NA NA
invalue_apply <- function(x, invalue, na_if = NULL) {
  if (!inherits(invalue, "ks_invalue")) {
    stop("invalue must be a ks_invalue object created with format_invalue()")
  }

  # Handle NULL input
  if (is.null(x)) {
    return(vector(invalue$target_type, 0))
  }

  # Initialize result vector with appropriate type
  result <- vector(invalue$target_type, length(x))
  result[] <- invalue$missing_value

  # Identify missing values
  is_missing <- is.na(x)

  # Add na_if values to missing
  if (!is.null(na_if)) {
    is_missing <- is_missing | x %in% na_if
  }

  # Process non-missing values
  non_missing_idx <- which(!is_missing)

  for (idx in non_missing_idx) {
    label <- as.character(x[idx])

    # Look up label in mappings
    if (label %in% names(invalue$mappings)) {
      value <- invalue$mappings[[label]]

      # Handle NA in mapping
      if (is.na(value)) {
        result[idx] <- invalue$missing_value
      } else {
        # Convert to target type
        result[idx] <- switch(
          invalue$target_type,
          "numeric" = as.numeric(value),
          "integer" = as.integer(value),
          "character" = as.character(value),
          "logical" = as.logical(value),
          value
        )
      }
    } else {
      # No mapping found - keep as missing or try to convert
      if (invalue$target_type == "numeric" || invalue$target_type == "integer") {
        # Try to convert directly
        converted <- suppressWarnings(as.numeric(label))
        if (!is.na(converted)) {
          result[idx] <- converted
        }
      }
    }
  }

  return(result)
}

#' Create Bidirectional Format
#'
#' Creates both a format and its corresponding invalue for bidirectional conversion.
#'
#' @param ... Named arguments for format mappings
#' @param name Character. Base name for both formats
#' @param type Character. Format type
#'
#' @return List with 'format' and 'invalue' components
#'
#' @export
#'
#' @examples
#' sex_bi <- format_bidirectional(
#'   "M" = "Male",
#'   "F" = "Female",
#'   name = "sex"
#' )
#'
#' # Format: M -> Male
#' format_apply("M", sex_bi$format)
#'
#' # Invalue: Male -> M
#' invalue_apply("Male", sex_bi$invalue)
format_bidirectional <- function(..., name = NULL, type = "auto") {
  mappings <- list(...)

  # Create format (value -> label)
  format_obj <- do.call(format_create, c(mappings, list(name = name, type = type)))

  # Create invalue (label -> value)
  # Reverse the mappings
  reversed_mappings <- lapply(names(mappings), function(key) {
    value <- mappings[[key]]
    stats::setNames(list(key), value)
  })
  reversed_mappings <- do.call(c, reversed_mappings)

  invalue_obj <- do.call(
    format_invalue,
    c(reversed_mappings, list(name = paste0(name, "_inv"), target_type = type))
  )

  return(list(
    format = format_obj,
    invalue = invalue_obj
  ))
}

#' Print Invalue Object
#'
#' @param x A ks_invalue object
#' @param ... Additional arguments (unused)
#' @export
print.ks_invalue <- function(x, ...) {
  cat("KS Invalue:", if (!is.null(x$name)) x$name else "(unnamed)", "\n")
  cat("Target Type:", x$target_type, "\n")
  cat("Mappings:\n")

  for (i in seq_along(x$mappings)) {
    key <- names(x$mappings)[i]
    value <- x$mappings[[i]]
    cat("  ", key, " => ", value, "\n", sep = "")
  }

  if (!is.na(x$missing_value)) {
    cat("  Missing value: ", x$missing_value, "\n", sep = "")
  }

  invisible(x)
}
