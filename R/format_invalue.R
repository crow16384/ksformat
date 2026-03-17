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
#' # Apply using finputn (numeric invalue by name)
#' finputn(c("Male", "Female", "Unknown"), "sex_inv")
#' # [1]  1  2 NA
#' fclear()
finput <- function(..., name = NULL, target_type = "numeric", missing_value = NA) {
  target_type <- match.arg(target_type, c("numeric", "integer", "character", "logical"))
  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      cli_abort("{.arg name} must be a single non-empty character string.")
    }
  }

  mappings <- list(...)

  if (length(mappings) == 0L) {
    cli_abort("At least one label-value mapping must be provided.")
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

  invalue_obj
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
#' @keywords internal
#'
#' @examples
#' inv <- finput("Male" = 1, "Female" = 2, name = "sex_inv")
#' ksformat:::.invalue_apply(c("Male", "Female", "Unknown"), inv)
#' # Returns: 1 2 NA
#' fclear()
.invalue_apply <- function(x, invalue, na_if = NULL) {
  # Resolve invalue by name if string provided
  if (is.character(invalue) && length(invalue) == 1L) {
    invalue <- .format_get(invalue)
  }

  if (!inherits(invalue, "ks_invalue")) {
    cli_abort("{.arg invalue} must be a {.cls ks_invalue} object or a registered invalue name.")
  }

  # Handle NULL input
  if (is.null(x)) {
    return(vector(invalue$target_type, 0L))
  }

  n <- length(x)
  result <- vector(invalue$target_type, n)
  result[] <- invalue$missing_value

  # Identify missing values
  is_miss <- is.na(x)
  if (!is.null(na_if)) {
    is_miss <- is_miss | x %in% na_if
  }

  non_miss <- which(!is_miss)
  if (length(non_miss) == 0L) return(result)

  # Vectorized lookup using match()
  labels <- as.character(x[non_miss])
  map_keys <- names(invalue$mappings)
  pos <- match(labels, map_keys)
  found <- !is.na(pos)

  if (any(found)) {
    values <- unlist(invalue$mappings[pos[found]], use.names = FALSE)
    not_na_vals <- !is.na(values)

    converter <- switch(
      invalue$target_type,
      "numeric" = as.numeric,
      "integer" = as.integer,
      "character" = as.character,
      "logical" = as.logical,
      identity
    )

    target_idx <- non_miss[found]
    if (any(not_na_vals)) {
      result[target_idx[not_na_vals]] <- converter(values[not_na_vals])
    }
  }

  # Unfound: try direct numeric conversion for numeric/integer types
  not_found <- non_miss[!found]
  if (length(not_found) > 0L &&
      invalue$target_type %in% c("numeric", "integer")) {
    converted <- suppressWarnings(as.numeric(labels[!found]))
    valid <- !is.na(converted)
    if (any(valid)) {
      result[not_found[valid]] <- converted[valid]
    }
  }

  result
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
#' # Create numeric invalue and apply
#' finput(
#'   "Male" = 1,
#'   "Female" = 2,
#'   name = "sex_inv"
#' )
#' finputn(c("Male", "Female", "Male", "Unknown", "Female"), "sex_inv")
#' # [1]  1  2  1 NA  2
#' fclear()
#'
#' # Parse invalue from text and apply
#' fparse(text = '
#' INVALUE race_inv
#'   "White" = 1
#'   "Black" = 2
#'   "Asian" = 3
#' ;
#' ')
#' finputn(c("White", "Black"), "race_inv")
#' # [1] 1 2
#' fclear()
finputn <- function(x, invalue_name) {
  inv_obj <- .format_get(invalue_name)
  if (!inherits(inv_obj, "ks_invalue")) {
    cli_abort("{.val {invalue_name}} is not an INVALUE format ({.cls ks_invalue}).")
  }
  result <- .invalue_apply(x, inv_obj)
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
#' # Bidirectional: use finputc for reverse direction
#' fnew_bid(
#'   "A" = "Active",
#'   "I" = "Inactive",
#'   "P" = "Pending",
#'   name = "status"
#' )
#'
#' # Forward: code -> label
#' fputc(c("A", "I", "P"), "status")
#' # [1] "Active" "Inactive" "Pending"
#'
#' # Reverse: label -> code
#' finputc(c("Active", "Pending", "Inactive"), "status_inv")
#' # [1] "A" "P" "I"
#' fclear()
finputc <- function(x, invalue_name) {
  inv_obj <- .format_get(invalue_name)
  if (!inherits(inv_obj, "ks_invalue")) {
    cli_abort("{.val {invalue_name}} is not an INVALUE format ({.cls ks_invalue}).")
  }
  result <- .invalue_apply(x, inv_obj)
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
#' # Bidirectional status format
#' status_bi <- fnew_bid(
#'   "A" = "Active",
#'   "I" = "Inactive",
#'   "P" = "Pending",
#'   name = "status"
#' )
#'
#' # Forward: code -> label
#' fputc(c("A", "I", "P", "A"), "status")
#' # [1] "Active" "Inactive" "Pending" "Active"
#'
#' # Reverse: label -> code
#' finputc(c("Active", "Pending", "Inactive"), "status_inv")
#' # [1] "A" "P" "I"
#' fclear()
fnew_bid <- function(..., name = NULL, type = "auto") {
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

  list(format = format_obj, invalue = invalue_obj)
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

  if (!is.null(x$missing_value) && !identical(x$missing_value, NA)) {
    cat("  Missing value: ", x$missing_value, "\n", sep = "")
  }

  invisible(x)
}
