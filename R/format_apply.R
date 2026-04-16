#' Apply Format to Data (like 'SAS' PUT function)
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
#' # Basic discrete formatting
#' fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")
#' fput(c("M", "F", NA, "X"), "sex")
#' # [1] "Male" "Female" "Unknown" "X"
#'
#' # Preserve NA instead of applying missing label
#' sex_f <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
#' fput(c("M", "F", NA), sex_f, keep_na = TRUE)
#' # [1] "Male" "Female" NA
#'
#' # Numeric range formatting
#' fparse(text = '
#' VALUE score (numeric)
#'   (0, 50]  = "Low"
#'   (50, 100] = "High"
#'   .other   = "Out of range"
#' ;
#' ')
#' fput(c(0, 1, 50, 51, 100, 101), "score")
#' # [1] "Out of range" "Low" "Low" "High" "High" "Out of range"
#' fclear()
fput <- function(x, format, ..., keep_na = FALSE) {
  # Resolve format by name if string provided
  if (is.character(format) && length(format) == 1L) {
    format <- .format_get(format)
  }

  if (!inherits(format, "ks_format")) {
    cli_abort(
      "{.arg format} must be a {.cls ks_format} object or a registered format name."
    )
  }

  # Handle NULL input
  if (is.null(x)) {
    if (.is_value_type(format$type)) return(.typed_na(format$type)[0L])
    return(character(0))
  }

  # Delegate to datetime formatter for date/time/datetime types
  if (format$type %in% c("date", "time", "datetime")) {
    return(.apply_datetime_format(x, format, keep_na = keep_na))
  }

  # Delegate to value type handler for Date/POSIXct/logical types
  if (.is_value_type(format$type)) {
    return(.fput_value_type(x, format, keep_na = keep_na))
  }

  extra_args <- list(...)

  # Recycle x when extra args are longer (vectorized .xN support)
  if (length(extra_args) > 0L) {
    arg_lens <- vapply(extra_args, length, integer(1L))
    non_scalar <- arg_lens[arg_lens > 1L]
    if (length(non_scalar) > 0L) {
      target_len <- non_scalar[1L]
      # All non-scalar extra args must have the same length
      if (!all(non_scalar == target_len)) {
        cli_abort(
          "Length mismatch in extra arguments: lengths {paste(arg_lens, collapse = ', ')}. All non-scalar arguments must have the same length."
        )
      }
      nx <- length(x)
      if (nx == 1L) {
        x <- rep(x, target_len)
      } else if (nx != target_len) {
        cli_abort(
          "Length mismatch: {.arg x} has length {nx} but extra arguments have length {target_len}. They must be equal (or {.arg x} must be scalar)."
        )
      }
    }
  }

  nocase <- isTRUE(format$ignore_case)
  caller_env <- parent.frame()

  n <- length(x)
  result <- rep(NA_character_, n)

  # Identify missing values (NA, NaN)
  is_miss <- is_missing(x)

  # Apply missing label
  if (!is.null(format$missing_label) && !keep_na) {
    miss_label <- format$missing_label
    if (.has_eval_attr(miss_label) || .is_expr_label(miss_label)) {
      miss_idx <- which(is_miss)
      if (length(miss_idx) > 0L) {
        result[miss_idx] <- .eval_expr_label(
          miss_label, extra_args, miss_idx, parent_env = caller_env
        )
      }
    } else {
      result[is_miss] <- miss_label
    }
  }

  non_miss <- which(!is_miss)
  if (length(non_miss) == 0L) return(result)

  map_keys <- names(format$mappings)
  map_labels <- unlist(format$mappings, use.names = FALSE)
  is_xn_expr <- grepl("\\.x\\d+", map_labels)
  map_eval <- vapply(format$mappings, .has_eval_attr, logical(1L))
  is_expr_label <- is_xn_expr | map_eval

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
    is_expr <- is_expr_label[pos[found]]

    # Assign static labels in bulk
    static <- !is_expr
    if (any(static)) {
      result[found_nm[static]] <- found_labels[static]
      matched[found_nm[static]] <- TRUE
    }

    # Defer expression labels — group by label
    if (any(is_expr)) {
      expr_idx <- which(is_expr)
      expr_labels <- found_labels[expr_idx]
      unique_expr <- unique(expr_labels)
      if (length(unique_expr) == 1L) {
        # Fast path: single expression label (common case)
        expr_map[[unique_expr]] <- c(expr_map[[unique_expr]], found_nm[expr_idx])
      } else {
        # General case: multiple expression labels
        grouped <- split(found_nm[expr_idx], expr_labels)
        for (lbl in names(grouped)) {
          expr_map[[lbl]] <- c(expr_map[[lbl]], grouped[[lbl]])
        }
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
            re_is_eval <- .is_expr_label(re$label) || .has_eval_attr(re$label)
            if (re_is_eval) {
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
      other_is_eval <- .is_expr_label(format$other_label) ||
                       .has_eval_attr(format$other_label)
      if (other_is_eval) {
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
      result[indices] <- .eval_expr_label(
        expr_str, extra_args, indices, parent_env = caller_env
      )
    }
  }

  result
}

# ---------------------------------------------------------------------------
# Internal helper: apply format with native value types (Date/POSIXct/logical)
# ---------------------------------------------------------------------------

#' @keywords internal
.fput_value_type <- function(x, format, keep_na = FALSE) {
  vtype <- format$type
  na_val <- .typed_na(vtype)

  n <- length(x)
  result <- rep(na_val, n)

  # Missing values → stay as typed NA
  is_miss <- is_missing(x)
  # (For value types, .missing is always NULL → NA of correct type)

  non_miss <- which(!is_miss)
  if (length(non_miss) == 0L) return(result)

  nocase <- isTRUE(format$ignore_case)
  map_keys <- names(format$mappings)

  # Build typed values vector (preserves Date/POSIXct class)
  map_values <- .typed_map_values(format$mappings)

  # Phase 1: Exact matching on string keys
  val_str <- as.character(x[non_miss])
  pos <- if (nocase) {
    match(tolower(val_str), tolower(map_keys))
  } else {
    match(val_str, map_keys)
  }

  found <- !is.na(pos)
  matched <- logical(n)

  if (any(found)) {
    found_nm <- non_miss[found]
    result[found_nm] <- map_values[pos[found]]
    matched[found_nm] <- TRUE
  }

  # Phase 2: Range matching for Date/POSIXct
  if (vtype %in% c("Date", "POSIXct")) {
    range_entries <- list()
    for (i in seq_along(map_keys)) {
      parsed <- .parse_date_range_key(map_keys[i], format$date_format)
      if (!is.null(parsed)) {
        range_entries[[length(range_entries) + 1L]] <- list(
          low = parsed$low, high = parsed$high,
          inc_low = parsed$inc_low, inc_high = parsed$inc_high,
          value = map_values[i]
        )
      }
    }

    if (length(range_entries) > 0L) {
      unmatched_nm <- non_miss[!matched[non_miss]]
      if (length(unmatched_nm) > 0L) {
        # Convert input to the target type for range comparison
        vals <- if (vtype == "Date") {
          if (!is.null(format$date_format)) {
            suppressWarnings(as.Date(as.character(x[unmatched_nm]),
                                     format = format$date_format))
          } else {
            suppressWarnings(as.Date(as.character(x[unmatched_nm])))
          }
        } else {
          if (!is.null(format$date_format)) {
            suppressWarnings(as.POSIXct(as.character(x[unmatched_nm]),
                                        format = format$date_format))
          } else {
            suppressWarnings(as.POSIXct(as.character(x[unmatched_nm])))
          }
        }
        valid <- !is.na(vals)

        for (re in range_entries) {
          still_free <- !matched[unmatched_nm] & valid
          if (!any(still_free)) break

          idx <- which(still_free)
          v <- vals[idx]
          low_ok  <- if (re$inc_low)  v >= re$low  else v > re$low
          high_ok <- if (re$inc_high) v <= re$high else v < re$high
          in_rng <- low_ok & high_ok

          if (any(in_rng)) {
            target <- unmatched_nm[idx[in_rng]]
            result[target] <- re$value
            matched[target] <- TRUE
          }
        }
      }
    }
  }

  # Phase 3: Unmatched → typed NA (no .other for value types)
  # result already initialized with typed NA, so nothing to do

  result
}

# ---------------------------------------------------------------------------
# Internal helper: vectorized format names (per-element format application)
# ---------------------------------------------------------------------------

#' @keywords internal
.fput_vectorized <- function(x, format_names, type_check = NULL, ...) {
  n <- length(x)
  result <- rep(NA_character_, n)

  # Group by unique format name for efficiency
  unique_fmts <- unique(format_names[!is.na(format_names)])

  for (fmt_name in unique_fmts) {
    idx <- which(format_names == fmt_name)
    format_obj <- .format_get(fmt_name)
    if (!inherits(format_obj, "ks_format")) {
      cli_warn("{.val {fmt_name}} is not a VALUE format ({.cls ks_format}), skipping.")
      next
    }
    if (!is.null(type_check) &&
        !format_obj$type %in% c(type_check, "date", "time", "datetime")) {
      cli_warn("Format {.val {fmt_name}} is type {.val {format_obj$type}}, not {.val {type_check}}.")
    }
    result[idx] <- fput(x[idx], format_obj, ...)
  }

  result
}


#' Apply Numeric Format by Name (like 'SAS' PUTN)
#'
#' Looks up a numeric VALUE format by name from the global format library
#' and applies it to the input vector.
#'
#' @param x Numeric vector of values to format
#' @param format_name Character. Name of a registered numeric format,
#'   or a character vector of format names (same length as \code{x}) to apply
#'   a different format per element (like 'SAS' PUTN with a variable format).
#' @param ... Additional arguments passed to \code{\link{fput}} for expression
#'   labels (mapped to \code{.x1}, \code{.x2}, etc.).
#'
#' @return Character vector with formatted labels
#'
#' @export
#'
#' @examples
#' # Numeric range formatting
#' fparse(text = '
#' VALUE age (numeric)
#'   [0, 18)    = "Child"
#'   [18, 65)   = "Adult"
#'   [65, HIGH]  = "Senior"
#'   .missing   = "Age Unknown"
#' ;
#' ')
#' fputn(c(5, 25, 70, NA), "age")
#' # [1] "Child" "Adult" "Senior" "Age Unknown"
#'
#' # SAS date format (auto-resolved, no pre-creation needed)
#' fputn(as.Date("2025-01-15"), "DATE9.")
#' # [1] "15JAN2025"
#'
#' # Time format (seconds since midnight)
#' fputn(c(0, 3600, 45000), "TIME8.")
#' # [1] "00:00:00" "01:00:00" "12:30:00"
#' fclear()
fputn <- function(x, format_name, ...) {
  # Support vectorized format names (like SAS PUTN with variable format)
  if (length(format_name) > 1L) {
    if (length(format_name) != length(x)) {
      cli_abort("{.arg format_name} must be length 1 or same length as {.arg x} ({length(x)}).")
    }
    return(.fput_vectorized(x, format_name, type_check = "numeric", ...))
  }
  format_obj <- .format_get(format_name)
  if (!inherits(format_obj, "ks_format")) {
    cli_abort("{.val {format_name}} is not a VALUE format ({.cls ks_format}).")
  }
  if (!format_obj$type %in% c("numeric", "date", "time", "datetime")) {
    cli_warn("Format {.val {format_name}} is type {.val {format_obj$type}}, not {.val numeric}.")
  }
  fput(x, format_obj, ...)
}

#' Apply Character Format by Name (like 'SAS' PUTC)
#'
#' Looks up a character VALUE format by name from the global format library
#' and applies it to the input vector.
#'
#' @param x Character vector of values to format
#' @param format_name Character. Name of a registered character format,
#'   or a character vector of format names (same length as \code{x}) to apply
#'   a different format per element (like 'SAS' PUTC with a variable format).
#' @param ... Additional arguments passed to \code{\link{fput}} for expression
#'   labels (mapped to \code{.x1}, \code{.x2}, etc.).
#'
#' @return Character vector with formatted labels
#'
#' @export
#'
#' @examples
#' # Apply character format by name
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' fputc(c("M", "F"), "sex")
#' # [1] "Male" "Female"
#'
#' # Bidirectional: forward direction
#' fnew_bid(
#'   "A" = "Active",
#'   "I" = "Inactive",
#'   "P" = "Pending",
#'   name = "status"
#' )
#' fputc(c("A", "I", "P", "A"), "status")
#' # [1] "Active" "Inactive" "Pending" "Active"
#' fclear()
fputc <- function(x, format_name, ...) {
  # Support vectorized format names (like SAS PUTC with variable format)
  if (length(format_name) > 1L) {
    if (length(format_name) != length(x)) {
      cli_abort("{.arg format_name} must be length 1 or same length as {.arg x} ({length(x)}).")
    }
    return(.fput_vectorized(x, format_name, type_check = "character", ...))
  }
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
#' # Basic multilabel: a value can match multiple labels
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
#'
#' # Multilabel with .missing and .other
#' fnew(
#'   "0,100,TRUE,TRUE"  = "Valid Score",
#'   "0,49,TRUE,TRUE"   = "Below Average",
#'   "50,100,TRUE,TRUE" = "Above Average",
#'   "90,100,TRUE,TRUE" = "Excellent",
#'   .missing = "No Score",
#'   .other = "Out of Range",
#'   name = "score_ml", type = "numeric", multilabel = TRUE
#' )
#' fput_all(c(95, 45, NA, 150), "score_ml")
#' # [[1]] "Valid Score" "Above Average" "Excellent"
#' # [[2]] "Valid Score" "Below Average"
#' # [[3]] "No Score"
#' # [[4]] "Out of Range"
#'
#' # Parse multilabel from text
#' fparse(text = '
#' VALUE risk (numeric, multilabel)
#'   [0, 3]  = "Low Risk"
#'   [0, 7]  = "Monitored"
#'   (3, 7]  = "Medium Risk"
#'   (7, 10] = "High Risk"
#' ;
#' ')
#' fput_all(c(2, 5, 9), "risk")
#' # [[1]] "Low Risk" "Monitored"
#' # [[2]] "Monitored" "Medium Risk"
#' # [[3]] "High Risk"
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

  # Value types: no multilabel support, delegate to fput and wrap
  if (.is_value_type(format$type)) {
    result <- .fput_value_type(x, format, keep_na = keep_na)
    return(as.list(result))
  }

  extra_args <- list(...)
  nocase <- isTRUE(format$ignore_case)
  caller_env <- parent.frame()

  n <- length(x)
  result <- vector("list", n)
  is_miss <- is_missing(x)

  miss_idx <- which(is_miss)
  if (!keep_na && !is.null(format$missing_label)) {
    miss_label <- format$missing_label
    if (.has_eval_attr(miss_label) || .is_expr_label(miss_label)) {
      if (length(miss_idx) > 0L) {
        evaled <- .eval_expr_label(
          miss_label, extra_args, miss_idx, parent_env = caller_env
        )
        result[miss_idx] <- as.list(evaled)
      }
    } else {
      result[miss_idx] <- list(miss_label)
    }
  } else {
    result[miss_idx] <- list(NA_character_)
  }

  non_miss <- which(!is_miss)
  if (length(non_miss) == 0L) return(result)

  result[non_miss] <- list(character(0))

  map_keys <- names(format$mappings)
  map_labels <- unlist(format$mappings, use.names = FALSE)
  map_eval <- vapply(format$mappings, .has_eval_attr, logical(1L))
  val_str <- as.character(x[non_miss])
  lookup_vals <- if (nocase) tolower(val_str) else val_str

  range_entries <- list()
  if (format$type == "numeric") {
    is_range <- vapply(map_keys, function(k) !is.null(.parse_range_key(k)), logical(1L))
    discrete_indices <- which(!is_range)
    range_idx <- which(is_range)
    for (i in range_idx) {
      parsed <- .parse_range_key(map_keys[i])
      range_entries[[length(range_entries) + 1L]] <- list(
        low = parsed$low, high = parsed$high,
        inc_low = parsed$inc_low, inc_high = parsed$inc_high,
        label = map_labels[i]
      )
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
      label_is_eval <- .is_expr_label(label) || map_eval[i]
      if (label_is_eval) {
        expr_map[[label]] <- c(expr_map[[label]], non_miss[matched_pos])
      } else {
        target <- non_miss[matched_pos]
        result[target] <- Map(c, result[target], list(label))
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
        re_is_eval <- .is_expr_label(re$label) || .has_eval_attr(re$label)
        if (re_is_eval) {
          expr_map[[re$label]] <- c(expr_map[[re$label]], non_miss[which(in_rng)])
        } else {
          target <- non_miss[which(in_rng)]
          result[target] <- Map(c, result[target], list(re$label))
        }
      }
    }
  }

  # Evaluate deferred expression labels and append to results
  if (length(expr_map) > 0L) {
    for (expr_str in names(expr_map)) {
      indices <- expr_map[[expr_str]]
      evaled <- .eval_expr_label(
        expr_str, extra_args, indices, parent_env = caller_env
      )
      result[indices] <- Map(c, result[indices], as.list(evaled))
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
      other_is_eval <- .is_expr_label(format$other_label) ||
                       .has_eval_attr(format$other_label)
      if (other_is_eval) {
        evaled <- .eval_expr_label(
          format$other_label, extra_args, idx, parent_env = caller_env
        )
        result[[idx]] <- c(result[[idx]], evaled)
      } else {
        result[[idx]] <- c(result[[idx]], format$other_label)
      }
    } else {
      result[[idx]] <- as.character(x[idx])
    }
  }

  result
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
#' # Apply formats to multiple columns
#' df <- data.frame(
#'   id = 1:6,
#'   sex = c("M", "F", "M", "F", NA, "X"),
#'   age = c(15, 25, 45, 70, 35, NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' sex_f <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
#' fparse(text = '
#' VALUE age (numeric)
#'   [0, 18)   = "Child"
#'   [18, 65)  = "Adult"
#'   [65, HIGH] = "Senior"
#'   .missing  = "Age Unknown"
#' ;
#' ')
#' age_f <- format_get("age")
#'
#' fput_df(df, sex = sex_f, age = age_f, suffix = "_label")
#'
#' # Date formatting in data frames
#' patients <- data.frame(
#'   id = 1:4,
#'   visit_date = as.Date(c("2025-01-10", "2025-02-15", "2025-03-20", NA)),
#'   stringsAsFactors = FALSE
#' )
#' visit_fmt <- fnew_date("DATE9.", name = "visit_fmt", .missing = "NOT RECORDED")
#' fput_df(patients, visit_date = visit_fmt)
#' fclear()
fput_df <- function(data, ..., suffix = "_fmt", replace = FALSE) {
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

  result
}

#' Apply Format Using a Composite Key
#'
#' Convenience wrapper around [fput()] that pastes multiple vectors together
#' into a composite key before lookup. Useful when a format is keyed on the
#' combination of several columns (e.g., `USUBJID|VISITNUM`).
#'
#' @param ... Vectors to paste together into a composite key.
#'   All vectors are recycled to a common length by [paste()].
#' @param format A [ks_format] object or a registered format name (character
#'   string).
#' @param sep Separator inserted between the pasted components
#'   (default `"|"`).
#' @param keep_na If `TRUE`, `NA` inputs remain `NA` in the output instead
#'   of being mapped via `.missing`. Passed through to [fput()].
#'
#' @return A character vector of formatted labels, the same length as the
#'   (recycled) input vectors.
#'
#' @examples
#' # Build a lookup keyed on two columns
#' fnew(
#'   "A|1" = "2025-01-15",
#'   "A|2" = "2025-02-20",
#'   "B|1" = "2025-03-10",
#'   .other = "NOT FOUND",
#'   name = "visit_date",
#'   type = "character"
#' )
#'
#' subj  <- c("A", "A", "B", "B")
#' visit <- c(1, 2, 1, 3)
#'
#' fputk(subj, visit, format = "visit_date")
#' # -> "2025-01-15" "2025-02-20" "2025-03-10" "NOT FOUND"
#'
#' fclear()
#'
#' @seealso [fput()], [fputn()], [fputc()]
#' @export
fputk <- function(..., format, sep = "|", keep_na = FALSE) {
  args <- list(...)
  if (length(args) < 1L) {
    cli_abort("At least one key component must be provided in {.code ...}.")
  }
  keys <- do.call(paste, c(args, list(sep = sep)))
  # Propagate NA: paste() coerces NA to "NA" — restore real NA so
  # fput() can apply .missing handling correctly.
  any_na <- Reduce(`|`, lapply(args, is.na))
  keys[any_na] <- NA_character_
  fput(keys, format, keep_na = keep_na)
}
