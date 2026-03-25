#' Create a Format Definition (like 'SAS' PROC FORMAT)
#'
#' Creates a format object that maps values to labels, similar to 'SAS' PROC FORMAT.
#' Supports discrete value mapping, ranges, and special handling of missing values.
#' The format is automatically stored in the global format library if \code{name}
#' is provided.
#'
#' @param ... Named arguments defining value-label mappings. Can be:
#'   \itemize{
#'     \item Discrete values: \code{"M" = "Male", "F" = "Female"}
#'     \item Special values: \code{.missing = "Missing"}, \code{.other = "Other"}
#'   }
#' @param name Character. Optional name for the format. If provided, the format
#'   is automatically registered in the global format library.
#' @param type Character. Type of format: \code{"character"}, \code{"numeric"},
#'   or \code{"auto"} (default) for auto-detection.
#' @param default Character. Default label for unmatched values (overrides .other)
#' @param multilabel Logical. If \code{TRUE}, the format supports overlapping
#'   ranges where a single value can match multiple labels. Used with
#'   \code{\link{fput_all}} to retrieve all matching labels. Default \code{FALSE}.
#' @param ignore_case Logical. If \code{TRUE}, key matching for character formats
#'   is case-insensitive. Default \code{FALSE}.
#'
#' @return An object of class \code{"ks_format"} containing the format definition.
#'   The object is also stored in the format library if \code{name} is given.
#'
#' @details
#' Special directives:
#' \itemize{
#'   \item \code{.missing}: Label for NA, NULL, NaN values
#'   \item \code{.other}: Label for values not matching any rule
#' }
#'
#' \strong{Expression labels:} If a label contains \code{.x1}, \code{.x2}, etc.,
#' it is treated as an R expression that is evaluated at apply-time. Extra arguments
#' are passed positionally via \code{...} in \code{\link{fput}}:
#' \preformatted{
#' stat_fmt <- fnew("n" = "sprintf('\%s', .x1)",
#'                  "pct" = "sprintf('\%.1f\%\%', .x1 * 100)")
#' fput(c("n", "pct"), stat_fmt, c(42, 0.15))
#' # Returns: "42" "15.0\%"
#' }
#'
#' @export
#'
#' @examples
#' # Discrete value format (auto-stored as "sex")
#' fnew(
#'   "M" = "Male",
#'   "F" = "Female",
#'   .missing = "Unknown",
#'   .other = "Other Gender",
#'   name = "sex"
#' )
#'
#' # Apply immediately
#' fput(c("M", "F", NA, "X"), "sex")
#' # [1] "Male" "Female" "Unknown" "Other Gender"
#' fclear()
#'
#' # Multilabel format: a value can match multiple labels
#' fnew(
#'   "0,5,TRUE,TRUE"   = "Infant",
#'   "6,11,TRUE,TRUE"  = "Child",
#'   "12,17,TRUE,TRUE" = "Adolescent",
#'   "0,17,TRUE,TRUE"  = "Pediatric",
#'   "18,64,TRUE,TRUE" = "Adult",
#'   "65,Inf,TRUE,TRUE" = "Elderly",
#'   "18,Inf,TRUE,TRUE" = "Non-Pediatric",
#'   name = "age_categories",
#'   type = "numeric",
#'   multilabel = TRUE
#' )
#'
#' # fput returns first match; fput_all returns all matches
#' fput(c(3, 14, 25, 70), "age_categories")
#' fput_all(c(3, 14, 25, 70), "age_categories")
#' fclear()
fnew <- function(..., name = NULL, type = "auto", default = NULL,
                 multilabel = FALSE, ignore_case = FALSE) {
  type <- match.arg(type, c("auto", "character", "numeric"))
  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      cli_abort("{.arg name} must be a single non-empty character string.")
    }
  }
  if (!is.null(default) && (!is.character(default) || length(default) != 1L)) {
    cli_abort("{.arg default} must be a single character string.")
  }
  if (!is.logical(multilabel) || length(multilabel) != 1L) {
    cli_abort("{.arg multilabel} must be TRUE or FALSE.")
  }
  if (!is.logical(ignore_case) || length(ignore_case) != 1L) {
    cli_abort("{.arg ignore_case} must be TRUE or FALSE.")
  }

  mappings <- list(...)

  if (length(mappings) == 0L) {
    cli_abort("At least one value-label mapping must be provided.")
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
  if (identical(type, "auto")) {
    type <- detect_format_type(names(mappings))
  }

  # Create format object
  format_obj <- structure(
    list(
      name = name,
      type = type,
      mappings = mappings,
      missing_label = missing_label,
      other_label = other_label,
      multilabel = multilabel,
      ignore_case = ignore_case,
      created = Sys.time()
    ),
    class = "ks_format"
  )

  # Validate format structure
  .format_validate(format_obj)

  # Auto-register in library if named
  .format_register(format_obj)

  format_obj
}


#' Mark a Label for Expression Evaluation
#'
#' Marks a format label string so it will be evaluated as an R expression
#' at apply-time (\code{\link{fput}}), even when it does not contain
#' \code{.x1}, \code{.x2}, etc. placeholders.
#'
#' @param expr Character string. The R expression to evaluate.
#' @return The same character string with an \code{"eval"} attribute set to
#'   \code{TRUE}.
#'
#' @details
#' This is useful when a label should call a function that does not need
#' positional \code{.xN} arguments.
#' The expression is evaluated in the caller's environment of
#' \code{\link{fput}}, so user-defined functions are accessible.
#'
#' Labels containing \code{.x1}, \code{.x2}, etc. are still evaluated
#' automatically without needing \code{e()}.
#'
#' @export
#' @examples
#' # Mark an expression for evaluation at apply-time
#' fmt <- fnew(
#'   "timestamp" = e("format(Sys.time(), '%Y-%m-%d')"),
#'   "static"    = "Hello",
#'   name = "demo_eval"
#' )
#' fput(c("timestamp", "static"), fmt)
#' fclear()
e <- function(expr) {
  if (!is.character(expr) || length(expr) != 1L) {
    cli_abort("{.arg expr} must be a single character string.")
  }
  attr(expr, "eval") <- TRUE
  expr
}


#' Detect Format Type
#'
#' @param keys Character vector of mapping key names
#' @return Character: "character" or "numeric"
#' @keywords internal
detect_format_type <- function(keys) {
  if (length(keys) == 0L) return("character")

  non_empty <- keys[!is.na(keys) & nzchar(keys)]

  if (length(non_empty) == 0L) {
    cli_warn("All mapping keys are empty or NA; defaulting to {.val numeric} type.")
    return("numeric")
  }

  if (all(!is.na(suppressWarnings(as.numeric(non_empty))))) {
    return("numeric")
  }

  if (any(grepl(",", keys, fixed = TRUE))) {
    return("numeric")
  }

  if (any(keys == "" | is.na(keys))) {
    return("numeric")
  }

  "character"
}


#' Print Format Object
#'
#' @param x A ks_format object
#' @param ... Additional arguments (unused)
#' @return The input \code{x}, returned invisibly.
#' @export
print.ks_format <- function(x, ...) {
  flags <- character(0)
  if (isTRUE(x$multilabel)) flags <- c(flags, "multilabel")
  if (isTRUE(x$ignore_case)) flags <- c(flags, "nocase")
  flags_str <- if (length(flags) > 0) paste0(" (", paste(flags, collapse = ", "), ")") else ""
  cat("KS Format:", if (!is.null(x$name)) x$name else "(unnamed)",
      flags_str, "\n", sep = "")
  cat("Type:", x$type, "\n")

  # Date/time formats show pattern instead of mappings
  if (x$type %in% c("date", "time", "datetime")) {
    pattern_str <- x$dt_pattern
    if (!is.null(x$sas_name)) {
      pattern_str <- paste0(pattern_str, " (", x$sas_name, ".)")
    }
    cat("Pattern:", pattern_str, "\n")
  } else {
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
  }

  if (!is.null(x$missing_label)) {
    cat("  .missing => ", x$missing_label, "\n", sep = "")
  }

  if (!is.null(x$other_label)) {
    cat("  .other => ", x$other_label, "\n", sep = "")
  }

  invisible(x)
}
