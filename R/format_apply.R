#' Apply Format to Data (like SAS PUT function)
#'
#' Applies a format definition to a vector of values, returning formatted labels.
#' Properly handles NA, NULL, NaN, and other missing values.
#'
#' @param x Vector of values to format
#' @param format A \code{ks_format} object or a character string naming a format
#'   in the global format library.
#' @param ... Additional arguments for expression labels. Positional arguments
#'   are mapped to \code{.x1}, \code{.x2}, etc. inside expression labels.
#'   Can be vectors of the same length as \code{x} or scalars (recycled).
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
#' \strong{Expression labels:} If a label string contains \code{.x1}, \code{.x2},
#' etc., it is evaluated as an R expression at apply-time. Extra data is passed
#' as positional arguments:
#'
#' \preformatted{
#' stat_fmt <- fnew("n" = "sprintf('\%s', .x1)",
#'                  "pct" = "sprintf('\%.1f\%\%', .x1 * 100)")
#' fput(c("n", "pct"), stat_fmt, c(42, 0.15))
#' # Returns: "42" "15.0\%"
#' }
#'
#' \strong{Case-insensitive matching:} When a format has \code{ignore_case = TRUE},
#' key matching is case-insensitive for character formats.
#'
#' @export
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")
#' fput(c("M", "F", NA, "X"), "sex")
#' # Returns: "Male" "Female" "Unknown" "X"
#' fclear()
fput <- function(x, format, ..., keep_na = FALSE) {
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

  # Delegate to datetime formatter for date/time/datetime types
  if (format$type %in% c("date", "time", "datetime")) {
    return(.apply_datetime_format(x, format, keep_na = keep_na))
  }

  # Capture extra arguments for expression labels
  extra_args <- list(...)

  # Flags
  nocase <- isTRUE(format$ignore_case)

  # Initialize result vector
  result <- rep(NA_character_, length(x))

  # Track expression-label assignments: list of expr_str -> indices
  expr_map <- list()

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
      val_str <- as.character(value)

      match_ok <- if (nocase) {
        tolower(val_str) == tolower(key)
      } else {
        val_str == key
      }

      if (match_ok) {
        label <- format$mappings[[i]]
        if (.is_expr_label(label)) {
          # Defer expression evaluation: record index
          if (is.null(expr_map[[label]])) expr_map[[label]] <- integer(0)
          expr_map[[label]] <- c(expr_map[[label]], idx)
        } else {
          result[idx] <- label
        }
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
          label <- re$label
          if (.is_expr_label(label)) {
            if (is.null(expr_map[[label]])) expr_map[[label]] <- integer(0)
            expr_map[[label]] <- c(expr_map[[label]], idx)
          } else {
            result[idx] <- label
          }
          matched <- TRUE
          break
        }
      }
    }

    # Apply other label if no match
    if (!matched) {
      if (!is.null(format$other_label)) {
        if (.is_expr_label(format$other_label)) {
          label <- format$other_label
          if (is.null(expr_map[[label]])) expr_map[[label]] <- integer(0)
          expr_map[[label]] <- c(expr_map[[label]], idx)
        } else {
          result[idx] <- format$other_label
        }
      } else {
        result[idx] <- as.character(value)
      }
    }
  }

  # Evaluate deferred expression labels
  if (length(expr_map) > 0) {
    for (expr_str in names(expr_map)) {
      indices <- expr_map[[expr_str]]
      result[indices] <- .eval_expr_label(expr_str, extra_args, indices)
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
#' @param ... Additional arguments passed to \code{\link{fput}} for expression
#'   labels (mapped to \code{.x1}, \code{.x2}, etc.).
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
fputn <- function(x, format_name, ...) {
  format_obj <- .format_get(format_name)
  if (!inherits(format_obj, "ks_format")) {
    stop("'", format_name, "' is not a VALUE format (ks_format)")
  }
  if (!format_obj$type %in% c("numeric", "date", "time", "datetime")) {
    warning("Format '", format_name, "' is type '", format_obj$type,
            "', not 'numeric'")
  }
  fput(x, format_obj, ...)
}

#' Apply Character Format by Name (like SAS PUTC)
#'
#' Looks up a character VALUE format by name from the global format library
#' and applies it to the input vector.
#'
#' @param x Character vector of values to format
#' @param format_name Character. Name of a registered character format.
#' @param ... Additional arguments passed to \code{\link{fput}} for expression
#'   labels (mapped to \code{.x1}, \code{.x2}, etc.).
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
fputc <- function(x, format_name, ...) {
  format_obj <- .format_get(format_name)
  if (!inherits(format_obj, "ks_format")) {
    stop("'", format_name, "' is not a VALUE format (ks_format)")
  }
  if (!format_obj$type %in% c("character", "date", "time", "datetime")) {
    warning("Format '", format_name, "' is type '", format_obj$type,
            "', not 'character'")
  }
  fput(x, format_obj, ...)
}

#' Apply Format and Return All Matches (Multilabel)
#'
#' For multilabel formats, returns all matching labels for each input value.
#' Regular \code{\link{fput}} returns only the first match; this function
#' returns all matches as a list of character vectors.
#'
#' @param x Vector of values to format
#' @param format A \code{ks_format} object or a character string naming a format
#'   in the global format library.
#' @param ... Additional arguments for expression labels (mapped to \code{.x1},
#'   \code{.x2}, etc.).
#' @param keep_na Logical. If TRUE, preserve NA in output.
#'
#' @return A list of character vectors. Each element contains all matching labels
#'   for the corresponding input value.
#'
#' @export
#'
#' @examples
#' age_ml <- fnew(
#'   "0,5,TRUE,TRUE" = "Infant",
#'   "6,11,TRUE,TRUE" = "Child",
#'   "12,17,TRUE,TRUE" = "Teen",
#'   "0,17,TRUE,TRUE" = "Minor",
#'   "18,64,TRUE,TRUE" = "Adult",
#'   "65,Inf,TRUE,TRUE" = "Senior",
#'   name = "age_ml", type = "numeric", multilabel = TRUE
#' )
#'
#' fput_all(c(3, 15, 25), age_ml)
#' # [[1]] "Infant" "Minor"
#' # [[2]] "Teen" "Minor"
#' # [[3]] "Adult"
#' fclear()
fput_all <- function(x, format, ..., keep_na = FALSE) {
  # Resolve format by name if string provided
  if (is.character(format) && length(format) == 1) {
    format <- .format_get(format)
  }

  if (!inherits(format, "ks_format")) {
    stop("format must be a ks_format object or a registered format name")
  }

  # Handle NULL input
  if (is.null(x)) {
    return(list())
  }

  # Date/time formats delegate to normal fput (no multilabel concept)
  if (format$type %in% c("date", "time", "datetime")) {
    result <- .apply_datetime_format(x, format, keep_na = keep_na)
    return(as.list(result))
  }

  # Capture extra arguments for expression labels
  extra_args <- list(...)

  # Flags
  nocase <- isTRUE(format$ignore_case)

  n <- length(x)
  result <- vector("list", n)

  # Identify missing values
  is_miss <- is.na(x) | is.nan(x)

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

  # Track expression labels: list of expr_str -> list(indices, positions_in_result)
  expr_map <- list()

  for (idx in seq_len(n)) {
    # Handle missing
    if (is_miss[idx]) {
      if (!keep_na && !is.null(format$missing_label)) {
        result[[idx]] <- format$missing_label
      } else {
        result[[idx]] <- NA_character_
      }
      next
    }

    value <- x[idx]
    labels <- character(0)
    expr_labels <- character(0)

    # Collect ALL exact matches
    for (i in seq_along(format$mappings)) {
      key <- names(format$mappings)[i]

      val_str <- as.character(value)
      match_ok <- if (nocase) tolower(val_str) == tolower(key) else val_str == key

      if (match_ok) {
        label <- format$mappings[[i]]
        if (.is_expr_label(label)) {
          expr_labels <- c(expr_labels, label)
        } else {
          labels <- c(labels, label)
        }
      }
    }

    # Collect ALL range matches for numeric values
    if (format$type == "numeric" && is.numeric(value)) {
      for (re in range_entries) {
        low_ok <- if (re$inc_low) value >= re$low else value > re$low
        high_ok <- if (re$inc_high) value <= re$high else value < re$high
        if (low_ok && high_ok) {
          if (.is_expr_label(re$label)) {
            expr_labels <- c(expr_labels, re$label)
          } else {
            labels <- c(labels, re$label)
          }
        }
      }
    }

    # No match: use other_label or original value
    if (length(labels) == 0 && length(expr_labels) == 0) {
      if (!is.null(format$other_label)) {
        if (.is_expr_label(format$other_label)) {
          expr_labels <- c(expr_labels, format$other_label)
        } else {
          labels <- format$other_label
        }
      } else {
        labels <- as.character(value)
      }
    }

    # Record expression labels for deferred evaluation
    for (el in expr_labels) {
      if (is.null(expr_map[[el]])) expr_map[[el]] <- integer(0)
      expr_map[[el]] <- c(expr_map[[el]], idx)
    }

    result[[idx]] <- labels
  }

  # Evaluate deferred expression labels and append to results
  if (length(expr_map) > 0) {
    for (expr_str in names(expr_map)) {
      indices <- expr_map[[expr_str]]
      evaled <- .eval_expr_label(expr_str, extra_args, indices)
      for (k in seq_along(indices)) {
        result[[indices[k]]] <- c(result[[indices[k]]], evaled[k])
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
