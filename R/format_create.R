#' Create a Format Definition (like SAS PROC FORMAT)
#'
#' Creates a format object that maps values to labels, similar to SAS PROC FORMAT.
#' Supports discrete value mapping, ranges, and special handling of missing values.
#'
#' @param ... Named arguments defining value-label mappings. Can be:
#'   - Discrete values: `"M" = "Male", "F" = "Female"`
#'   - Ranges: `c(0, 18) = "Child", c(18, 65) = "Adult"`
#'   - Special values: `.missing = "Missing"`, `.other = "Other"`
#' @param name Character. Optional name for the format (like SAS format name)
#' @param type Character. Type of format: "character" or "numeric"
#' @param default Character. Default label for unmatched values (overrides .other)
#'
#' @return An object of class "ks_format" containing the format definition
#'
#' @details
#' Special directives:
#' - `.missing`: Label for NA, NULL, NaN values
#' - `.other`: Label for values not matching any rule
#' - Ranges are specified as c(low, high) and are inclusive on both ends
#' - Use -Inf or Inf for open-ended ranges
#'
#' @export
#'
#' @examples
#' # Discrete value format
#' sex_fmt <- format_create(
#'   "M" = "Male",
#'   "F" = "Female",
#'   .missing = "Unknown"
#' )
#' 
#' # For range-based formatting, use string keys:
#' \dontrun{
#' age_fmt <- format_create(
#'   "0-18" = "Child",
#'   "18-65" = "Adult",
#'   "65+" = "Senior",
#'   .missing = "Age Unknown"
#' )
#' }
format_create <- function(..., name = NULL, type = "auto", default = NULL) {
  mappings <- list(...)
  
  if (length(mappings) == 0) {
    stop("At least one value-label mapping must be provided")
  }
  
  # Extract special directives
  missing_label <- mappings[[".missing"]]
  other_label <- mappings[[".other"]]
  
  # Remove special directives from mappings
  mappings[[".missing"]] <- NULL
  mappings[[".other"]] <- NULL
  
  # Override .other with default if provided
  if (!is.null(default)) {
    other_label <- default
  }
  
  # Determine format type
  if (type == "auto") {
    type <- detect_format_type(names(mappings), mappings)
  }
  
  # Validate mappings
  validate_mappings(mappings, type)
  
  # Create format object
  format_obj <- structure(
    list(
      name = name,
      type = type,
      mappings = mappings,
      missing_label = missing_label,
      other_label = other_label,
      created = Sys.time()
    ),
    class = "ks_format"
  )
  
  return(format_obj)
}

#' Detect Format Type
#'
#' @param names Character vector of mapping names
#' @param mappings List of mappings
#' @return Character: "character" or "numeric"
#' @keywords internal
detect_format_type <- function(names, mappings) {
  # Check if any mapping is a range (vector of length 2)
  has_ranges <- any(sapply(mappings, function(x) FALSE)) # Check keys, not values
  
  # Check names
  if (any(!is.na(suppressWarnings(as.numeric(names))))) {
    return("numeric")
  }
  
  # Check if any mapping key is numeric
  for (key in names(mappings)) {
    if (!is.na(suppressWarnings(as.numeric(key)))) {
      return("numeric")
    }
  }
  
  # Check for vector keys (ranges)
  for (i in seq_along(mappings)) {
    key_expr <- names(mappings)[i]
    if (is.null(key_expr) || key_expr == "") {
      # This might be a range passed as vector
      return("numeric")
    }
  }
  
  return("character")
}

#' Validate Format Mappings
#'
#' @param mappings List of mappings
#' @param type Format type
#' @keywords internal
validate_mappings <- function(mappings, type) {
  if (length(mappings) == 0) {
    return(invisible(TRUE))
  }
  
  # Check that all mapping values are character
  labels <- unlist(mappings)
  if (!all(sapply(labels, is.character))) {
    stop("All labels must be character strings")
  }
  
  invisible(TRUE)
}

#' Print Format Object
#'
#' @param x A ks_format object
#' @param ... Additional arguments (unused)
#' @export
print.ks_format <- function(x, ...) {
  cat("KS Format:", if (!is.null(x$name)) x$name else "(unnamed)", "\n")
  cat("Type:", x$type, "\n")
  cat("Mappings:\n")

  for (i in seq_along(x$mappings)) {
    key <- names(x$mappings)[i]
    value <- x$mappings[[i]]

    # Try to display range keys in interval notation
    parsed <- .parse_range_key(key)
    if (!is.null(parsed)) {
      left_bracket <- if (parsed$inc_low) "[" else "("
      right_bracket <- if (parsed$inc_high) "]" else ")"
      low_str <- if (is.infinite(parsed$low) && parsed$low < 0) "LOW" else as.character(parsed$low)
      high_str <- if (is.infinite(parsed$high) && parsed$high > 0) "HIGH" else as.character(parsed$high)
      cat("  ", left_bracket, low_str, ", ", high_str, right_bracket,
          " => ", value, "\n", sep = "")
    } else {
      cat("  ", key, " => ", value, "\n", sep = "")
    }
  }

  if (!is.null(x$missing_label)) {
    cat("  .missing => ", x$missing_label, "\n", sep = "")
  }

  if (!is.null(x$other_label)) {
    cat("  .other => ", x$other_label, "\n", sep = "")
  }

  invisible(x)
}
