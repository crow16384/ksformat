#' Create Invalue Format (Reverse Formatting like SAS INVALUE)
#'
#' Creates an invalue format that converts formatted labels back to values.
#' This is similar to SAS PROC FORMAT with INVALUE statement.
#' The invalue is automatically stored in the global format library if \code{name}
#' is provided.
#'
#' @param ... Named arguments defining label-value mappings (reverse of \code{\link{fnew}}).
#'   Example: \code{"Male" = 1, "Female" = 2}.
#' @param name Character. Optional name for the invalue format. If provided,
#'   the invalue is automatically registered in the global format library.
#' @param target_type Character. Type to convert to: \code{"numeric"} (default),
#'   \code{"integer"}, \code{"character"}, or \code{"logical"}.
#'   INVALUE formats produce numeric output by default; character-to-character
#'   conversion should use a regular VALUE format (\code{\link{fnew}}) instead.
#' @param missing_value Value to use for missing inputs (default: \code{NA})
#'
#' @return An object of class \code{"ks_invalue"} containing the invalue definition.
#'   The object is also stored in the format library if \code{name} is given.
#'
#' @export
#'
#' @examples
#' # Convert text labels to numeric codes
#' finput(
#'   "Male" = 1,
#'   "Female" = 2,
#'   name = "sex_inv"
#' )
#'
#' invalue_apply(c("Male", "Female", "Unknown"), "sex_inv")
#' # Returns: 1 2 NA
#' fclear()
finput <- function(..., name = NULL, target_type = "numeric", missing_value = NA) {
  mappings <- list(...)

  if (length(mappings) == 0) {
    stop("At least one label-value mapping must be provided")
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

  # Validate
  .format_validate(invalue_obj)

  # Auto-register in library if named
  .format_register(invalue_obj)

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

  return("numeric")
}

#' Apply Invalue Format (Reverse Formatting)
#'
#' Applies an invalue format to convert formatted labels back to values.
#'
#' @param x Character vector of labels to convert
#' @param invalue A \code{ks_invalue} object or a character string naming an
#'   invalue format in the global format library.
#' @param na_if Character vector. Additional values to treat as NA
#'
#' @return Vector with values (type depends on invalue's \code{target_type})
#'
#' @export
#'
#' @examples
#' inv <- finput("Male" = 1, "Female" = 2, name = "sex_inv")
#' invalue_apply(c("Male", "Female", "Unknown"), inv)
#' # Returns: 1 2 NA
#' fclear()
invalue_apply <- function(x, invalue, na_if = NULL) {
  # Resolve invalue by name if string provided
  if (is.character(invalue) && length(invalue) == 1) {
    invalue <- .format_get(invalue)
  }

  if (!inherits(invalue, "ks_invalue")) {
    stop("invalue must be a ks_invalue object or a registered invalue name")
  }

  # Handle NULL input
  if (is.null(x)) {
    return(vector(invalue$target_type, 0))
  }

  # Initialize result vector with appropriate type
  result <- vector(invalue$target_type, length(x))
  result[] <- invalue$missing_value

  # Identify missing values
  is_miss <- is.na(x)

  # Add na_if values to missing
  if (!is.null(na_if)) {
    is_miss <- is_miss | x %in% na_if
  }

  # Process non-missing values
  non_missing_idx <- which(!is_miss)

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
      # No mapping found - try to convert directly for numeric types
      if (invalue$target_type == "numeric" || invalue$target_type == "integer") {
        converted <- suppressWarnings(as.numeric(label))
        if (!is.na(converted)) {
          result[idx] <- converted
        }
      }
    }
  }

  return(result)
}

#' Apply Numeric Invalue by Name (like SAS INPUTN)
#'
#' Looks up a numeric INVALUE format by name from the global format library
#' and applies it to convert labels to numeric values.
#'
#' @param x Character vector of labels to convert
#' @param invalue_name Character. Name of a registered INVALUE format.
#'
#' @return Numeric vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' finput("Male" = 1, "Female" = 2, name = "sex_inv")
#' finputn(c("Male", "Female"), "sex_inv")
#' }
finputn <- function(x, invalue_name) {
  inv_obj <- .format_get(invalue_name)
  if (!inherits(inv_obj, "ks_invalue")) {
    stop("'", invalue_name, "' is not an INVALUE format (ks_invalue)")
  }
  result <- invalue_apply(x, inv_obj)
  as.numeric(result)
}

#' Apply Character Invalue by Name (like SAS INPUTC)
#'
#' Looks up an INVALUE format by name from the global format library
#' and applies it to convert labels to character values.
#'
#' @param x Character vector of labels to convert
#' @param invalue_name Character. Name of a registered INVALUE format.
#'
#' @return Character vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' finput("Male" = "M", "Female" = "F", name = "sex_inv",
#'        target_type = "character")
#' finputc(c("Male", "Female"), "sex_inv")
#' }
finputc <- function(x, invalue_name) {
  inv_obj <- .format_get(invalue_name)
  if (!inherits(inv_obj, "ks_invalue")) {
    stop("'", invalue_name, "' is not an INVALUE format (ks_invalue)")
  }
  result <- invalue_apply(x, inv_obj)
  as.character(result)
}

#' Create Bidirectional Format
#'
#' Creates both a format and its corresponding invalue for bidirectional conversion.
#' Both are automatically stored in the global format library if \code{name}
#' is provided.
#'
#' @param ... Named arguments for format mappings
#' @param name Character. Base name for both formats. The invalue will be
#'   named \code{paste0(name, "_inv")}.
#' @param type Character. Format type
#'
#' @return List with \code{format} (ks_format) and \code{invalue} (ks_invalue)
#'   components.
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
#' fput("M", sex_bi$format)
#'
#' # Invalue: Male -> M
#' invalue_apply("Male", sex_bi$invalue)
#' fclear()
format_bidirectional <- function(..., name = NULL, type = "auto") {
  mappings <- list(...)

  # Create format (value -> label)
  format_obj <- do.call(fnew, c(mappings, list(name = name, type = type)))

  # Create invalue (label -> value)
  # Reverse the mappings
  reversed_mappings <- lapply(names(mappings), function(key) {
    value <- mappings[[key]]
    stats::setNames(list(key), value)
  })
  reversed_mappings <- do.call(c, reversed_mappings)

  inv_name <- if (!is.null(name)) paste0(name, "_inv") else NULL
  invalue_obj <- do.call(
    finput,
    c(reversed_mappings, list(
      name = inv_name,
      target_type = "character"
    ))
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
