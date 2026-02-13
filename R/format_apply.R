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
  if (is.character(format) && length(format) == 1L) {
    format <- .format_get(format)
  }

  if (!inherits(format, "ks_format")) {
    cli_abort("{.arg format} must be a {.cls ks_format} object or a registered format name.")
  }

  # Handle NULL input
  if (is.null(x)) {
    return(character(0))
  }

  # Delegate to datetime formatter for date/time/datetime types
  if (format$type %in% c("date", "time", "datetime")) {
    return(.apply_datetime_format(x, format, keep_na = keep_na))
  }

  extra_args <- list(...)
  nocase <- isTRUE(format$ignore_case)

  n <- length(x)
  result <- rep(NA_character_, n)

  # Identify missing values (NA, NaN)
  is_miss <- is.na(x) | is.nan(x)

  # Apply missing label
  if (!is.null(format$missing_label) && !keep_na) {
    result[is_miss] <- format$missing_label
  }

  non_miss <- which(!is_miss)
  if (length(non_miss) == 0L) return(result)

  # Vectorized mapping lookup
  map_keys <- names(format$mappings)
  map_labels <- unlist(format$mappings, use.names = FALSE)

  # Phase 1: Vectorized exact match using match()
  val_str <- as.character(x[non_miss])
  pos <- if (nocase) {
    match(tolower(val_str), tolower(map_keys))
  } else {
    match(val_str, map_keys)
  }

  found <- !is.na(pos)
  matched <- logical(n)
  expr_map <- list()

  if (any(found)) {
    found_labels <- map_labels[pos[found]]
    found_nm <- non_miss[found]
    is_expr <- grepl("\\.x\\d+", found_labels)

    # Assign static labels in bulk
    static <- !is_expr
    if (any(static)) {
      result[found_nm[static]] <- found_labels[static]
      matched[found_nm[static]] <- TRUE
    }

    # Defer expression labels
    if (any(is_expr)) {
      for (j in which(is_expr)) {
        lbl <- found_labels[j]
        expr_map[[lbl]] <- c(expr_map[[lbl]], found_nm[j])
      }
      matched[found_nm[is_expr]] <- TRUE
    }
  }

  # Phase 2: Vectorized range match for numeric formats
  if (format$type == "numeric") {
    # Pre-parse range entries
    range_entries <- list()
    for (i in seq_along(map_keys)) {
      parsed <- .parse_range_key(map_keys[i])
      if (!is.null(parsed)) {
        range_entries[[length(range_entries) + 1L]] <- list(
          low = parsed$low, high = parsed$high,
          inc_low = parsed$inc_low, inc_high = parsed$inc_high,
          label = map_labels[i]
        )
      }
    }

    if (length(range_entries) > 0L) {
      unmatched_nm <- non_miss[!matched[non_miss]]
      if (length(unmatched_nm) > 0L) {
        vals <- suppressWarnings(as.numeric(x[unmatched_nm]))
        valid_num <- !is.na(vals)

        for (re in range_entries) {
          still_free <- !matched[unmatched_nm] & valid_num
          if (!any(still_free)) break

          idx <- which(still_free)
          v <- vals[idx]
          low_ok <- if (re$inc_low) v >= re$low else v > re$low
          high_ok <- if (re$inc_high) v <= re$high else v < re$high
          in_rng <- low_ok & high_ok

          if (any(in_rng)) {
            target <- unmatched_nm[idx[in_rng]]
            if (grepl("\\.x\\d+", re$label)) {
              expr_map[[re$label]] <- c(expr_map[[re$label]], target)
            } else {
              result[target] <- re$label
            }
            matched[target] <- TRUE
          }
        }
      }
    }
  }

  # Phase 3: Unmatched -> .other or original value
  unmatched_final <- non_miss[!matched[non_miss]]
  if (length(unmatched_final) > 0L) {
    if (!is.null(format$other_label)) {
      if (grepl("\\.x\\d+", format$other_label)) {
        expr_map[[format$other_label]] <- c(
          expr_map[[format$other_label]], unmatched_final
        )
      } else {
        result[unmatched_final] <- format$other_label
      }
    } else {
      result[unmatched_final] <- as.character(x[unmatched_final])
    }
  }

  # Phase 4: Evaluate deferred expression labels
  if (length(expr_map) > 0L) {
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
    cli_abort("{.val {format_name}} is not a VALUE format ({.cls ks_format}).")
  }
  if (!format_obj$type %in% c("numeric", "date", "time", "datetime")) {
    cli_warn("Format {.val {format_name}} is type {.val {format_obj$type}}, not {.val numeric}.")
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
    cli_abort("{.val {format_name}} is not a VALUE format ({.cls ks_format}).")
  }
  if (!format_obj$type %in% c("character", "date", "time", "datetime")) {
    cli_warn("Format {.val {format_name}} is type {.val {format_obj$type}}, not {.val character}.")
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
  if (is.character(format) && length(format) == 1L) {
    format <- .format_get(format)
  }

  if (!inherits(format, "ks_format")) {
    cli_abort("{.arg format} must be a {.cls ks_format} object or a registered format name.")
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

  extra_args <- list(...)
  nocase <- isTRUE(format$ignore_case)

  n <- length(x)
  result <- vector("list", n)
  is_miss <- is.na(x) | is.nan(x)

  # Handle missing values
  miss_idx <- which(is_miss)
  for (idx in miss_idx) {
    result[[idx]] <- if (!keep_na && !is.null(format$missing_label)) {
      format$missing_label
    } else {
      NA_character_
    }
  }

  non_miss <- which(!is_miss)
  if (length(non_miss) == 0L) return(result)

  # Init non-missing result slots
  for (idx in non_miss) result[[idx]] <- character(0)

  map_keys <- names(format$mappings)
  map_labels <- unlist(format$mappings, use.names = FALSE)
  val_str <- as.character(x[non_miss])
  lookup_vals <- if (nocase) tolower(val_str) else val_str

  # Pre-parse range entries
  range_entries <- list()
  discrete_indices <- integer(0)
  if (format$type == "numeric") {
    for (i in seq_along(map_keys)) {
      parsed <- .parse_range_key(map_keys[i])
      if (!is.null(parsed)) {
        range_entries[[length(range_entries) + 1L]] <- list(
          low = parsed$low, high = parsed$high,
          inc_low = parsed$inc_low, inc_high = parsed$inc_high,
          label = map_labels[i]
        )
      } else {
        discrete_indices <- c(discrete_indices, i)
      }
    }
  } else {
    discrete_indices <- seq_along(map_keys)
  }

  expr_map <- list()
  has_any_match <- logical(length(non_miss))

  # Vectorized discrete matching: check each key against all values at once
  for (i in discrete_indices) {
    lookup_key <- if (nocase) tolower(map_keys[i]) else map_keys[i]
    matched_pos <- which(lookup_vals == lookup_key)

    if (length(matched_pos) > 0L) {
      label <- map_labels[i]
      has_any_match[matched_pos] <- TRUE
      if (grepl("\\.x\\d+", label)) {
        expr_map[[label]] <- c(expr_map[[label]], non_miss[matched_pos])
      } else {
        for (p in matched_pos) {
          result[[non_miss[p]]] <- c(result[[non_miss[p]]], label)
        }
      }
    }
  }

  # Vectorized range matching
  if (length(range_entries) > 0L) {
    vals <- suppressWarnings(as.numeric(x[non_miss]))
    valid_num <- !is.na(vals)

    for (re in range_entries) {
      low_ok <- if (re$inc_low) vals >= re$low else vals > re$low
      high_ok <- if (re$inc_high) vals <= re$high else vals < re$high
      in_rng <- low_ok & high_ok & valid_num

      if (any(in_rng)) {
        has_any_match[in_rng] <- TRUE
        if (grepl("\\.x\\d+", re$label)) {
          expr_map[[re$label]] <- c(expr_map[[re$label]], non_miss[which(in_rng)])
        } else {
          for (p in which(in_rng)) {
            result[[non_miss[p]]] <- c(result[[non_miss[p]]], re$label)
          }
        }
      }
    }
  }

  # Evaluate deferred expression labels and append to results
  if (length(expr_map) > 0L) {
    for (expr_str in names(expr_map)) {
      indices <- expr_map[[expr_str]]
      evaled <- .eval_expr_label(expr_str, extra_args, indices)
      for (k in seq_along(indices)) {
        result[[indices[k]]] <- c(result[[indices[k]]], evaled[k])
      }
    }
  }

  # Unmatched: .other or original value
  no_match <- non_miss[!has_any_match]
  # Also check expr_map hasn't already matched these
  if (length(expr_map) > 0L) {
    expr_matched <- unique(unlist(expr_map, use.names = FALSE))
    no_match <- setdiff(no_match, expr_matched)
  }
  for (idx in no_match) {
    if (!is.null(format$other_label)) {
      if (grepl("\\.x\\d+", format$other_label)) {
        evaled <- .eval_expr_label(format$other_label, extra_args, idx)
        result[[idx]] <- c(result[[idx]], evaled)
      } else {
        result[[idx]] <- format$other_label
      }
    } else {
      result[[idx]] <- as.character(x[idx])
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
    cli_abort("{.arg data} must be a data frame.")
  }

  result <- data

  for (col_name in names(formats)) {
    if (!col_name %in% names(data)) {
      cli_warn("Column {.val {col_name}} not found in data frame.")
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
