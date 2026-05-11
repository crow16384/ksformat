#' Utilities for ksformat Package
#'
#' Internal helper functions for format creation and manipulation.
#'
#' @name utilities
#' @keywords internal
NULL


# ===========================================================================
# Value type constants and helpers
# ===========================================================================

#' Recognised value types for ks_format objects.
#' These types store native R objects instead of character labels.
#' @keywords internal
#' @noRd
.value_types <- c("Date", "POSIXct", "logical")

#' Range-bucketing types (Date/POSIXct/numeric input -> character labels)
#' @keywords internal
#' @noRd
.range_types <- c("numeric", "date_range", "datetime_range")

#' Check whether a type uses numeric range matching internally
#' @keywords internal
#' @noRd
.is_range_type <- function(type) {
  type %in% .range_types
}

#' Check whether a type uses Date/POSIXct range bounds
#' @keywords internal
#' @noRd
.is_date_range_type <- function(type) {
  type %in% c("date_range", "datetime_range")
}

#' Check whether a type is the stratified-range type
#'
#' Stratified ranges combine one or more discrete stratum columns with a
#' numeric / date / datetime range key. They use \code{fputk()} for lookup
#' and a per-stratum list of range tables internally.
#' @keywords internal
#' @noRd
.is_stratified_type <- function(type) {
  identical(type, "stratified_range")
}

#' Map a stratified-range subtype to its underlying range-table type
#' @keywords internal
#' @noRd
.stratified_subtype_to_range_type <- function(range_subtype) {
  switch(range_subtype,
    numeric  = "numeric",
    date     = "date_range",
    datetime = "datetime_range",
    cli_abort("Unknown stratified range subtype: {.val {range_subtype}}.")
  )
}

#' Check whether a type string is a value type
#' @keywords internal
#' @noRd
.is_value_type <- function(type) {
  type %in% .value_types
}

#' Return a typed NA for the given value type
#' @keywords internal
#' @noRd
.typed_na <- function(type) {
  switch(type,
    Date    = as.Date(NA_real_, origin = "1970-01-01"),
    POSIXct = as.POSIXct(NA_real_, origin = "1970-01-01"),
    logical = NA,
    NA
  )
}

#' Extract values from a ks_format mapping list as a typed vector
#'
#' Uses \code{do.call(c, ...)} which preserves Date/POSIXct class.
#' Returns NULL for empty mappings.
#' @keywords internal
#' @noRd
.typed_map_values <- function(mappings) {
  if (length(mappings) == 0L) return(NULL)
  do.call(c, unname(mappings))
}

#' Parse a Date range key like "2020-01-01,2025-12-31,TRUE,FALSE"
#'
#' @param key Character string to parse.
#' @param date_format Optional strptime format for parsing date bounds.
#' @return A list with components low, high (Date), inc_low, inc_high (logical),
#'   or NULL if the key does not look like a date range.
#' @keywords internal
#' @noRd
.parse_date_range_key <- function(key, date_format = NULL) {
  parts <- strsplit(key, ",", fixed = TRUE)[[1]]
  if (length(parts) < 2L || length(parts) > 4L) return(NULL)

  low_str  <- trimws(parts[1])
  high_str <- trimws(parts[2])

  # Parse bounds
  parse_bound <- function(s, is_low) {
    su <- toupper(s)
    if (su == "LOW")  return(as.Date(-Inf, origin = "1970-01-01"))
    if (su == "HIGH") return(as.Date(Inf,  origin = "1970-01-01"))
    if (!is.null(date_format)) {
      d <- as.Date(s, format = date_format)
    } else {
      d <- suppressWarnings(as.Date(s))
    }
    if (is.na(d)) return(NULL)
    d
  }

  low  <- parse_bound(low_str,  TRUE)
  high <- parse_bound(high_str, FALSE)
  if (is.null(low) || is.null(high)) return(NULL)

  inc_low  <- TRUE
  inc_high <- FALSE
  if (length(parts) >= 3L) inc_low  <- toupper(trimws(parts[3])) == "TRUE"
  if (length(parts) >= 4L) inc_high <- toupper(trimws(parts[4])) == "TRUE"

  list(low = low, high = high, inc_low = inc_low, inc_high = inc_high)
}

#' Parse a POSIXct datetime range key like "2020-01-01 00:00:00,2025-12-31 23:59:59,TRUE,FALSE"
#'
#' Returns POSIXct bounds. \code{LOW}/\code{HIGH} map to \code{-Inf}/\code{Inf}.
#'
#' @param key Character string to parse.
#' @param date_format Optional strptime format for parsing datetime bounds.
#' @return A list with components low, high (POSIXct), inc_low, inc_high
#'   (logical), or NULL if the key does not look like a datetime range.
#' @keywords internal
#' @noRd
.parse_datetime_range_key <- function(key, date_format = NULL) {
  parts <- strsplit(key, ",", fixed = TRUE)[[1]]
  if (length(parts) < 2L || length(parts) > 4L) return(NULL)

  low_str  <- trimws(parts[1])
  high_str <- trimws(parts[2])

  parse_bound <- function(s) {
    su <- toupper(s)
    if (su == "LOW")  return(as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC"))
    if (su == "HIGH") return(as.POSIXct(Inf,  origin = "1970-01-01", tz = "UTC"))
    if (!is.null(date_format)) {
      d <- suppressWarnings(as.POSIXct(s, format = date_format, tz = "UTC"))
    } else {
      d <- suppressWarnings(as.POSIXct(s, tz = "UTC"))
    }
    if (is.na(d)) return(NULL)
    d
  }

  low  <- parse_bound(low_str)
  high <- parse_bound(high_str)
  if (is.null(low) || is.null(high)) return(NULL)

  inc_low  <- TRUE
  inc_high <- FALSE
  if (length(parts) >= 3L) inc_low  <- toupper(trimws(parts[3])) == "TRUE"
  if (length(parts) >= 4L) inc_high <- toupper(trimws(parts[4])) == "TRUE"

  list(low = low, high = high, inc_low = inc_low, inc_high = inc_high)
}

#' Coerce input vector to numeric for range comparison
#'
#' Dispatches based on the format type:
#' \itemize{
#'   \item numeric: uses input directly if numeric, else \code{as.numeric()}.
#'   \item date_range: parses to \code{Date} (via \code{date_format} if
#'     supplied) and unclasses to numeric (days since 1970).
#'   \item datetime_range: parses to \code{POSIXct} (UTC) and unclasses to
#'     numeric (seconds since epoch).
#' }
#' @keywords internal
#' @noRd
.to_range_numeric <- function(x, type, date_format = NULL) {
  if (identical(type, "numeric")) {
    if (is.numeric(x)) return(x)
    return(suppressWarnings(as.numeric(x)))
  }
  if (identical(type, "date_range")) {
    d <- if (inherits(x, "Date")) {
      x
    } else if (inherits(x, "POSIXt")) {
      as.Date(x)
    } else if (!is.null(date_format)) {
      suppressWarnings(as.Date(as.character(x), format = date_format))
    } else {
      suppressWarnings(as.Date(as.character(x)))
    }
    return(as.numeric(unclass(d)))
  }
  if (identical(type, "datetime_range")) {
    d <- if (inherits(x, "POSIXt")) {
      x
    } else if (inherits(x, "Date")) {
      as.POSIXct(x, tz = "UTC")
    } else if (!is.null(date_format)) {
      suppressWarnings(as.POSIXct(as.character(x), format = date_format, tz = "UTC"))
    } else {
      suppressWarnings(as.POSIXct(as.character(x), tz = "UTC"))
    }
    return(as.numeric(unclass(d)))
  }
  suppressWarnings(as.numeric(x))
}

#' @keywords internal
#' @noRd
.typed_value_to_string <- function(value, type, date_format = NULL) {
  if (is.na(value)) return("NA")
  switch(type,
    Date    = if (!is.null(date_format)) format(value, date_format) else as.character(value),
    POSIXct = if (!is.null(date_format)) format(value, date_format) else as.character(value),
    logical = as.character(value),
    as.character(value)
  )
}

#' Parse a string value into a native typed object
#' @keywords internal
#' @noRd
.parse_typed_value <- function(value_str, type, date_format = NULL) {
  switch(type,
    Date = {
      if (!is.null(date_format)) as.Date(value_str, format = date_format)
      else as.Date(value_str)
    },
    POSIXct = {
      if (!is.null(date_format)) as.POSIXct(value_str, format = date_format)
      else as.POSIXct(value_str)
    },
    logical = as.logical(value_str),
    value_str
  )
}


# ===========================================================================
# Expression label helpers
# ===========================================================================

#' Check if a label string is an R expression
#'
#' A label is treated as an expression if it references
#' \code{.x1}, \code{.x2}, etc.
#'
#' @param label Character string (the label/value side of a mapping)
#' @return Logical
#' @keywords internal
#' @noRd
.is_expr_label <- function(label) {
  grepl("\\.x\\d+", label)
}


#' Check if a label has the eval attribute
#'
#' @param label Character string (the label/value side of a mapping)
#' @return Logical
#' @keywords internal
#' @noRd
.has_eval_attr <- function(label) {
  isTRUE(attr(label, "eval"))
}


#' Evaluate an expression label with extra arguments
#'
#' Parses the expression string and evaluates it in an environment
#' containing \code{.x1}, \code{.x2}, etc. subsetted to the given indices.
#'
#' @param expr_str Character. The expression string.
#' @param extra_args List. Positional arguments from \code{fput(x, fmt, ...)}.
#' @param indices Integer vector. Indices in the original vector that matched.
#' @return Character vector of evaluated results.
#' @keywords internal
#' @noRd
.eval_expr_label <- function(expr_str, extra_args, indices,
                             parent_env = baseenv()) {
  # Build .x1, .x2, ... bindings via list2env (single C-level call)
  n_args <- length(extra_args)
  if (n_args > 0L) {
    xn_vals <- vector("list", n_args)
    xn_names <- paste0(".x", seq_len(n_args))
    for (j in seq_len(n_args)) {
      av <- extra_args[[j]]
      xn_vals[[j]] <- if (length(av) == 1L) av else av[indices]
    }
    env <- list2env(stats::setNames(xn_vals, xn_names), parent = parent_env)
  } else {
    env <- new.env(parent = parent_env)
  }

  # Use cached parsed expression to avoid repeat parse() calls
  parsed <- if (exists(expr_str, envir = .expr_parse_cache, inherits = FALSE)) {
    .expr_parse_cache[[expr_str]]
  } else {
    p <- tryCatch(
      parse(text = expr_str),
      error = function(e) {
        cli_warn(c(
          "Expression parse failed: {conditionMessage(e)}",
          "i" = "Expression: {expr_str}"
        ))
        NULL
      }
    )
    if (!is.null(p)) assign(expr_str, p, envir = .expr_parse_cache)
    p
  }
  if (is.null(parsed)) return(rep(NA_character_, length(indices)))

  result <- tryCatch(
    as.character(eval(parsed, envir = env)),
    error = function(e) {
      cli_warn(c(
        "Expression evaluation failed: {conditionMessage(e)}",
        "i" = "Expression: {expr_str}"
      ))
      rep(NA_character_, length(indices))
    }
  )

  # Ensure result length matches indices
  if (length(result) == 0L) {
    cli_warn("Expression returned empty result, using NA. Expression: {expr_str}")
    result <- rep(NA_character_, length(indices))
  } else if (length(result) != length(indices)) {
    if (length(result) == 1L) {
      result <- rep(result, length(indices))
    } else {
      cli_warn("Expression returned {length(result)} values, expected {length(indices)}. Recycling/truncating.")
      result <- rep_len(result, length(indices))
    }
  }

  result
}


#' Create a Key-Value Mapping for Format Creation
#'
#' Convenience helper for building data-driven formats with \code{\link{fnew}}.
#' Returns a named vector (or list) with class \code{"ks_fmap"} that signals
#' \code{fnew()} to use the natural direction: names are \strong{input keys},
#' values are \strong{output labels/objects} — regardless of the format type.
#'
#' Without \code{fmap()}, \code{fnew()} reverses named vectors for character
#' and numeric types (the \code{factor()} convention \code{c(Label = "Code")}).
#' Wrapping your data in \code{fmap()} suppresses this reversal, so
#' \code{fmap(keys, values)} works identically for character, numeric, Date,
#' POSIXct, and logical formats.
#'
#' @param keys Character vector of input keys (lookup values).
#' @param values Vector of output labels or objects (character, numeric, Date,
#'   POSIXct, logical, etc.).
#'
#' @return A named vector (or list, for non-atomic values) with class
#'   \code{c("ks_fmap", <original class>)}. Names are \code{keys}, values are
#'   \code{values}.
#'
#' @export
#'
#' @seealso \code{\link{fnew}} for format creation.
#'
#' @examples
#' # Character lookup: keys -> labels
#' fmap(c("M", "F"), c("Male", "Female")) |> fnew(name = "sex")
#' fput(c("M", "F"), "sex")
#' fclear()
#'
#' # Date lookup from a data frame
#' ids   <- c("SUBJ-001", "SUBJ-002")
#' dates <- as.Date(c("2023-03-09", "2024-08-13"))
#' fmap(ids, dates) |> fnew(type = "Date", name = "icdtn")
#' fput("SUBJ-001", "icdtn")
#' fclear()
fmap <- function(keys, values) {
  if (length(keys) != length(values)) {
    cli_abort(
      "{.arg keys} (length {length(keys)}) and {.arg values} (length {length(values)}) must have the same length."
    )
  }
  keys <- as.character(keys)
  out <- stats::setNames(values, keys)
  class(out) <- c("ks_fmap", class(out))
  out
}


#' Check if Value is Missing
#'
#' Element-wise check for missing values including NA and NaN.
#' Optionally treats empty strings as missing.
#'
#' @param x Value to check
#'
#' @return Logical vector. NULL input returns \code{logical(0)}.
#'
#' @export
#'
#' @examples
#' is_missing(NA)          # TRUE
#' is_missing(NaN)         # TRUE
#' is_missing("")          # TRUE
#' is_missing("text")      # FALSE
#' is_missing(c(1, NA, NaN)) # FALSE TRUE TRUE
is_missing <- function(x) {
  if (is.null(x)) return(logical(0))
  # is.na() already returns TRUE for NaN on numeric inputs, so no need for
  # a separate is.nan() pass. For non-numeric types, also treat empty
  # strings as missing.
  if (is.numeric(x)) return(is.na(x))
  is.na(x) | !nzchar(x)
}

#' Create Range Specification
#'
#' Helper function to create range specifications for numeric formats.
#'
#' @param low Numeric. Lower bound of the range.
#' @param high Numeric. Upper bound of the range.
#' @param label Character. Label for values in this range.
#' @param inc_low Logical. If \code{TRUE} (default), the lower bound is inclusive
#'   (>=). If \code{FALSE}, exclusive (>).
#' @param inc_high Logical. If \code{TRUE}, the upper bound is inclusive (<=).
#'   If \code{FALSE} (default), exclusive (<).
#'
#' @return A \code{range_spec} object (list with low, high, label, inc_low, inc_high).
#'
#' @details
#' By default, ranges are half-open: \code{[low, high)} — the lower bound is
#' included and the upper bound is excluded. This matches 'SAS' PROC FORMAT
#' range semantics and prevents overlap between adjacent ranges.
#'
#' @export
#'
#' @examples
#' range_spec(0, 18, "Child")          # [0, 18)
#' range_spec(18, 65, "Adult")         # [18, 65)
#' range_spec(65, Inf, "Senior", inc_high = TRUE)  # [65, Inf]
range_spec <- function(low, high, label, inc_low = TRUE, inc_high = FALSE) {
  if (!is.numeric(low) || !is.numeric(high)) {
    cli_abort("Range bounds must be numeric.")
  }

  if (low > high) {
    cli_abort("Lower bound must be less than or equal to upper bound.")
  }

  structure(
    list(
      low = low,
      high = high,
      label = as.character(label),
      inc_low = inc_low,
      inc_high = inc_high
    ),
    class = "range_spec"
  )
}

#' Check if Value Falls in Range
#'
#' @param x Numeric value
#' @param range_spec Range specification
#'
#' @return Logical
#' @keywords internal
#' @noRd
in_range <- function(x, range_spec) {
  if (!inherits(range_spec, "range_spec")) {
    return(FALSE)
  }
  if (length(x) == 1L && is.na(x)) return(FALSE)

  low_ok <- if (range_spec$inc_low) x >= range_spec$low else x > range_spec$low
  high_ok <- if (range_spec$inc_high) x <= range_spec$high else x < range_spec$high
  low_ok & high_ok
}


#' Parse a range key string into components
#'
#' Parses range keys stored in format mappings. Supports two formats:
#' \itemize{
#'   \item 4-part: \code{"low,high,inc_low,inc_high"} (e.g. \code{"0,18,TRUE,FALSE"})
#'   \item 2-part (legacy): \code{"low,high"} (e.g. \code{"0,18"}) — defaults to \code{[low, high)}
#' }
#'
#' @param key Character. The mapping key to parse.
#' @return A list with \code{low}, \code{high}, \code{inc_low}, \code{inc_high},
#'   or \code{NULL} if the key is not a valid range.
#' @keywords internal
#' @noRd
.parse_range_key <- function(key) {
  parts <- strsplit(key, ",")[[1]]

  if (length(parts) == 4) {
    low <- suppressWarnings(as.numeric(parts[1]))
    high <- suppressWarnings(as.numeric(parts[2]))
    inc_low <- as.logical(parts[3])
    inc_high <- as.logical(parts[4])
    if (!is.na(low) && !is.na(high) && !is.na(inc_low) && !is.na(inc_high)) {
      return(list(low = low, high = high, inc_low = inc_low, inc_high = inc_high))
    }
  }

  if (length(parts) == 2) {
    low <- suppressWarnings(as.numeric(parts[1]))
    high <- suppressWarnings(as.numeric(parts[2]))
    if (!is.na(low) && !is.na(high)) {
      return(list(low = low, high = high, inc_low = TRUE, inc_high = FALSE))
    }
  }

  NULL
}


#' Build a precomputed range table from a mappings list
#'
#' Walks the mapping keys once with \code{.parse_range_key()} and returns
#' parallel atomic vectors describing the range entries, plus indices of the
#' discrete (non-range) entries and metadata about labels (eval flag, .xN
#' expression). The result is attached to a \code{ks_format} object so that
#' \code{fput()} / \code{fput_all()} can skip per-call parsing of range keys.
#'
#' Returns \code{NULL} for non-numeric formats (other types do not use the
#' generic numeric range scan in \code{fput()}).
#'
#' @param mappings Named list of label values (the \code{mappings} field of a
#'   \code{ks_format} object).
#' @param type Character. Format type. Range table is only built for
#'   \code{"numeric"}.
#' @return A list with components:
#'   \itemize{
#'     \item \code{low}, \code{high}: numeric vectors of range bounds.
#'     \item \code{inc_low}, \code{inc_high}: logical vectors of inclusivity.
#'     \item \code{label}: character vector of range labels (with any
#'       \code{eval} attribute preserved on each element).
#'     \item \code{is_eval}: logical vector — TRUE if the label is an
#'       expression label (\code{.xN} or has \code{eval} attribute).
#'     \item \code{range_idx}: integer indices of range entries in the
#'       original \code{mappings}.
#'     \item \code{discrete_idx}: integer indices of non-range (discrete)
#'       entries in the original \code{mappings}.
#'     \item \code{mapping_is_eval}: logical vector (length of
#'       \code{mappings}) — TRUE if each label is an expression label.
#'     \item \code{mapping_labels}: character vector of all labels
#'       (unlisted, names dropped) for fast bulk access.
#'     \item \code{sorted_disjoint}: logical. TRUE when range entries are
#'       sorted by \code{low} and non-overlapping (enables
#'       \code{findInterval()} fast path).
#'     \item \code{sort_perm}: integer permutation that sorts the range
#'       entries by \code{low}. When \code{sorted_disjoint} is TRUE, the
#'       \code{low}/\code{high}/etc. vectors above are already in this
#'       order; otherwise the original order is preserved and
#'       \code{sort_perm} is informational.
#'   }
#' @keywords internal
#' @noRd
.build_range_table <- function(mappings, type, date_format = NULL) {
  n <- length(mappings)
  map_labels <- if (n == 0L) character(0) else unlist(mappings, use.names = FALSE)
  # Vectorized eval-flag detection: .is_expr_label is already vectorized
  # via grepl; only .has_eval_attr needs a per-element check.
  mapping_is_eval <- if (n == 0L) {
    logical(0)
  } else {
    .is_expr_label(map_labels) |
      vapply(mappings, .has_eval_attr, logical(1L))
  }

  base <- list(
    low = numeric(0), high = numeric(0),
    inc_low = logical(0), inc_high = logical(0),
    label = character(0), is_eval = logical(0),
    range_idx = integer(0),
    discrete_idx = seq_len(n),
    mapping_is_eval = mapping_is_eval,
    mapping_labels = map_labels,
    sorted_disjoint = FALSE,
    sort_perm = integer(0)
  )

  if (n == 0L || !.is_range_type(type)) return(base)

  # Pick the right key parser. For date/datetime range types we parse keys
  # as ISO date strings (or via `date_format` if provided); the resulting
  # Date/POSIXct bounds are then unclassed to numeric so the rest of the
  # range-table machinery (findInterval fast path, sorted/disjoint check)
  # works unchanged.
  parser <- switch(type,
    numeric         = function(k) .parse_range_key(k),
    date_range      = function(k) .parse_date_range_key(k, date_format),
    datetime_range  = function(k) .parse_datetime_range_key(k, date_format)
  )

  map_keys <- names(mappings)
  # Two-pass build: identify range indices first, then preallocate
  parsed_list <- vector("list", n)
  is_range <- logical(n)
  for (i in seq_len(n)) {
    p <- parser(map_keys[i])
    if (!is.null(p)) {
      parsed_list[[i]] <- p
      is_range[i] <- TRUE
    }
  }
  ridx <- which(is_range)
  nr <- length(ridx)
  if (nr == 0L) return(base)

  # For date_range / datetime_range, unclass the typed bounds to numeric
  # (days since 1970 for Date, seconds since epoch for POSIXct). The
  # comparison semantics are identical to numeric ranges.
  to_num <- function(v) as.numeric(unclass(v))
  lows  <- vapply(parsed_list[ridx], function(p) to_num(p$low),  numeric(1L))
  highs <- vapply(parsed_list[ridx], function(p) to_num(p$high), numeric(1L))
  incl  <- vapply(parsed_list[ridx], `[[`, logical(1L), "inc_low")
  inch  <- vapply(parsed_list[ridx], `[[`, logical(1L), "inc_high")

  # Canonicalise: sort range entries by (low, high) so fast path triggers
  # regardless of the definition order used in fnew()/fparse(). Original
  # mapping order is preserved on the ks_format object itself; only the
  # range_table is reordered here.
  sort_perm <- order(lows, highs)
  lows <- lows[sort_perm]
  highs <- highs[sort_perm]
  incl <- incl[sort_perm]
  inch <- inch[sort_perm]
  ridx_sorted <- ridx[sort_perm]

  labels_r  <- map_labels[ridx_sorted]
  is_eval_r <- mapping_is_eval[ridx_sorted]

  # Check disjoint: adjacent ranges may touch only when not both inclusive.
  disjoint <- TRUE
  if (nr > 1L) {
    for (j in seq_len(nr - 1L)) {
      if (highs[j] > lows[j + 1L]) { disjoint <- FALSE; break }
      if (highs[j] == lows[j + 1L] && inch[j] && incl[j + 1L]) {
        disjoint <- FALSE; break
      }
    }
  }

  list(
    low = lows, high = highs,
    inc_low = incl, inc_high = inch,
    label = labels_r, is_eval = is_eval_r,
    range_idx = ridx_sorted,
    discrete_idx = setdiff(seq_len(n), ridx),
    mapping_is_eval = mapping_is_eval,
    mapping_labels = map_labels,
    sorted_disjoint = disjoint,
    sort_perm = sort_perm
  )
}


#' Split a stratified mapping key into (stratum, range_key)
#'
#' Stratified keys have the shape \code{"STRATUM<sep>RANGE_KEY"}, where
#' \code{STRATUM} may itself contain \code{sep} (e.g. composite strata
#' produced by \code{fputk()}). We split on the rightmost occurrence of
#' \code{sep} whose suffix is a valid range key for \code{range_subtype}.
#'
#' Returns \code{NULL} if no split yields a parseable range key (callers
#' should treat such keys as not stratified — typically directives like
#' \code{".missing"} / \code{".other"} that have already been peeled off
#' before reaching this helper).
#'
#' @param key Character scalar.
#' @param strata_sep Separator string.
#' @param range_subtype One of \code{"numeric"}, \code{"date"},
#'   \code{"datetime"}.
#' @param date_format Optional strptime format passed to date parsers.
#' @return A list \code{list(stratum, range_key)} or \code{NULL}.
#' @keywords internal
#' @noRd
.split_stratified_key <- function(key, strata_sep, range_subtype,
                                  date_format = NULL) {
  if (!is.character(key) || length(key) != 1L || is.na(key)) return(NULL)
  parser <- switch(range_subtype,
    numeric  = function(s) .parse_range_key(s),
    date     = function(s) .parse_date_range_key(s, date_format),
    datetime = function(s) .parse_datetime_range_key(s, date_format),
    NULL
  )
  if (is.null(parser)) return(NULL)

  # Find all occurrences of strata_sep
  positions <- gregexpr(strata_sep, key, fixed = TRUE)[[1]]
  if (length(positions) == 1L && positions[1L] == -1L) return(NULL)

  sep_len <- nchar(strata_sep)
  # Try rightmost first so longest stratum (with embedded separators) wins.
  for (pos in rev(positions)) {
    stratum <- substr(key, 1L, pos - 1L)
    range_key <- substr(key, pos + sep_len, nchar(key))
    if (!nzchar(stratum) || !nzchar(range_key)) next
    parsed <- tryCatch(parser(range_key), error = function(e) NULL)
    if (!is.null(parsed)) {
      return(list(stratum = stratum, range_key = range_key))
    }
  }
  NULL
}

#' Build per-stratum range tables for a stratified_range format
#'
#' Groups \code{mappings} by stratum (via \code{.split_stratified_key()})
#' and builds one numeric range table per stratum using the existing
#' \code{.build_range_table()} machinery.
#'
#' Mapping keys that cannot be split into a valid stratified key are
#' silently ignored here; \code{fnew()} is responsible for raising an
#' informative error before this helper is called.
#'
#' @return Named list. Each element is the result of
#'   \code{.build_range_table()} for the mappings belonging to that
#'   stratum. The list also carries attribute \code{"strata"} (character
#'   vector of stratum names in first-seen order).
#' @keywords internal
#' @noRd
.build_stratified_range_tables <- function(mappings, range_subtype,
                                           strata_sep = "|",
                                           date_format = NULL) {
  if (length(mappings) == 0L) {
    return(structure(list(), strata = character(0)))
  }
  range_type <- .stratified_subtype_to_range_type(range_subtype)
  keys <- names(mappings)

  strata <- character(length(keys))
  range_keys <- character(length(keys))
  ok <- logical(length(keys))
  for (i in seq_along(keys)) {
    sp <- .split_stratified_key(keys[i], strata_sep, range_subtype,
                                date_format)
    if (!is.null(sp)) {
      strata[i] <- sp$stratum
      range_keys[i] <- sp$range_key
      ok[i] <- TRUE
    }
  }
  if (!any(ok)) {
    return(structure(list(), strata = character(0)))
  }

  # Preserve first-seen stratum order
  unique_strata <- unique(strata[ok])
  out <- vector("list", length(unique_strata))
  names(out) <- unique_strata
  for (s in unique_strata) {
    sel <- which(ok & strata == s)
    sub <- mappings[sel]
    names(sub) <- range_keys[sel]
    out[[s]] <- .build_range_table(sub, range_type, date_format)
  }
  attr(out, "strata") <- unique_strata
  out
}


#' Build canonical range keys from low/high/inc vectors
#'
#' Internal helper shared by \code{fmap_ranges()} and \code{fmap_strata()}.
#' Validates inputs, recycles scalar inclusivity flags, formats bounds to
#' ISO 8601 for Date / POSIXct, and returns a list with the canonical
#' four-part keys, the bound-type label, and the recycled flags.
#' @keywords internal
#' @noRd
.build_range_keys <- function(low, high, inc_low, inc_high,
                              date_format = NULL) {
  n <- length(low)
  if (length(high) != n) {
    cli_abort(
      "{.arg low} (length {n}) and {.arg high} (length {length(high)}) must have the same length."
    )
  }
  if (length(inc_low) == 1L) inc_low <- rep(inc_low, n)
  if (length(inc_high) == 1L) inc_high <- rep(inc_high, n)
  if (length(inc_low) != n || length(inc_high) != n) {
    cli_abort(
      "{.arg inc_low} and {.arg inc_high} must be length 1 or {n}."
    )
  }
  if (!is.logical(inc_low) || !is.logical(inc_high)) {
    cli_abort("{.arg inc_low} and {.arg inc_high} must be logical.")
  }
  if (anyNA(inc_low) || anyNA(inc_high)) {
    cli_abort("{.arg inc_low}/{.arg inc_high} must not contain NA.")
  }

  # Determine bound type and format
  bound_type <- if (inherits(low, "POSIXt") || inherits(high, "POSIXt")) {
    "datetime"
  } else if (inherits(low, "Date") || inherits(high, "Date")) {
    "date"
  } else if (is.numeric(low) && is.numeric(high)) {
    "numeric"
  } else {
    cli_abort(
      "{.arg low}/{.arg high} must be numeric, Date, or POSIXct."
    )
  }

  # Validate ordering (allow infinities; LOW/HIGH not supported in builder)
  if (any(!is.na(low) & !is.na(high) & low > high)) {
    cli_abort("All {.arg low} values must be <= corresponding {.arg high}.")
  }

  fmt_bound <- function(v) {
    if (bound_type == "numeric") {
      return(as.character(v))
    }
    if (bound_type == "date") {
      d <- as.Date(v)
      if (!is.null(date_format)) {
        return(format(d, date_format))
      }
      return(format(d, "%Y-%m-%d"))
    }
    # datetime
    p <- as.POSIXct(v, tz = "UTC")
    if (!is.null(date_format)) {
      return(format(p, date_format, tz = "UTC"))
    }
    format(p, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }

  low_str <- fmt_bound(low)
  high_str <- fmt_bound(high)

  keys <- paste(low_str, high_str, inc_low, inc_high, sep = ",")
  list(keys = keys, bound_type = bound_type,
       inc_low = inc_low, inc_high = inc_high)
}


#' Build a Vector of Range Mappings
#'
#' Construct a \code{ks_fmap}-classed named character vector whose names
#' encode numeric / Date / POSIXct range bounds and whose values are the
#' corresponding labels. The result is intended to be passed to
#' \code{\link{fnew}} as a single positional argument (it suppresses the
#' default name reversal).
#'
#' Bounds are formatted as ISO 8601: \code{"\%Y-\%m-\%d"} for Date,
#' \code{"\%Y-\%m-\%d \%H:\%M:\%S"} (UTC) for POSIXct. Override with
#' \code{date_format} if needed.
#'
#' @param low,high Numeric, \code{Date}, or \code{POSIXct} vectors of equal
#'   length giving the lower / upper bounds of each range.
#' @param label Character vector of labels (same length as \code{low}).
#' @param inc_low,inc_high Logical, length 1 or \code{length(low)}. Whether
#'   each bound is inclusive. Defaults match
#'   \code{\link{range_spec}}: \code{[low, high)}.
#' @param date_format Optional strptime format string used when formatting
#'   \code{Date}/\code{POSIXct} bounds into key strings.
#'
#' @return A \code{ks_fmap} object (named character vector) suitable for
#'   passing to \code{fnew()}.
#' @export
#' @seealso \code{\link{fmap}}, \code{\link{fmap_strata}}, \code{\link{fnew}}
#' @examples
#' rng <- fmap_ranges(
#'   low   = c(0, 18, 65),
#'   high  = c(18, 65, Inf),
#'   label = c("Child", "Adult", "Senior"),
#'   inc_high = c(FALSE, FALSE, TRUE)
#' )
#' fnew(rng, type = "numeric", name = "age_groups")
#' fput(c(5, 25, 90), "age_groups")
#' fclear()
fmap_ranges <- function(low, high, label,
                        inc_low = TRUE, inc_high = FALSE,
                        date_format = NULL) {
  if (length(low) != length(label)) {
    cli_abort(
      "{.arg low}/{.arg high} and {.arg label} must have the same length."
    )
  }
  parts <- .build_range_keys(low, high, inc_low, inc_high, date_format)
  out <- stats::setNames(as.character(label), parts$keys)
  class(out) <- c("ks_fmap", class(out))
  out
}


#' Build a Vector of Stratified Range Mappings
#'
#' Companion to \code{\link{fmap_ranges}} for the \code{stratified_range}
#' format type. Each row pairs a stratum (e.g. study arm, subject id, or a
#' composite key produced by \code{fputk()}) with a numeric / Date /
#' POSIXct range and a label. The returned \code{ks_fmap} vector carries
#' the chosen \code{sep} as an attribute so that
#' \code{\link{fnew}(type = "stratified_range")} picks it up automatically.
#'
#' @param stratum Character vector of stratum identifiers.
#' @param low,high Range bounds. See \code{\link{fmap_ranges}}.
#' @param label Character vector of labels.
#' @param inc_low,inc_high Logical, length 1 or \code{length(low)}. See
#'   \code{\link{fmap_ranges}}.
#' @param sep Separator inserted between stratum and range key. Must match
#'   the \code{sep} subsequently passed to \code{\link{fputk}}.
#' @param date_format Optional strptime format string.
#'
#' @return A \code{ks_fmap} object with an attached \code{"strata_sep"}
#'   attribute.
#' @export
#' @seealso \code{\link{fmap_ranges}}, \code{\link{fputk}}, \code{\link{fnew}}
#' @examples
#' visits <- fmap_strata(
#'   stratum  = c("ARM_A", "ARM_A", "ARM_B"),
#'   low      = c(0, 7, 0),
#'   high     = c(7, 14, 10),
#'   label    = c("Baseline", "Week 1", "Baseline")
#' )
#' fnew(visits, type = "stratified_range", range_subtype = "numeric",
#'      name = "visit_window")
#' fputk(c("ARM_A", "ARM_B"), c(3, 5), format = "visit_window")
#' fclear()
fmap_strata <- function(stratum, low, high, label,
                        inc_low = TRUE, inc_high = FALSE,
                        sep = "|", date_format = NULL) {
  if (!is.character(sep) || length(sep) != 1L || is.na(sep) || !nzchar(sep)) {
    cli_abort("{.arg sep} must be a single non-empty character string.")
  }
  if (length(stratum) != length(label)) {
    cli_abort(
      "{.arg stratum} and {.arg label} must have the same length."
    )
  }
  base <- fmap_ranges(low, high, label, inc_low, inc_high, date_format)
  prefixed_keys <- paste0(as.character(stratum), sep, names(base))
  out <- stats::setNames(unclass(base), prefixed_keys)
  attr(out, "strata_sep") <- sep
  class(out) <- c("ks_fmap", class(out))
  out
}


#' Extract Range Entries from a Format
#'
#' Returns the range-based mappings of a \code{ks_format} object as a tidy
#' data frame. Discrete entries (plain values, \code{.missing}, \code{.other})
#' are excluded.
#'
#' @param fmt A \code{ks_format} object, or a character name of a format
#'   registered in the global library.
#'
#' @return A \code{data.frame} with columns \code{low}, \code{high},
#'   \code{inc_low}, \code{inc_high}, and \code{label}. Rows are returned in
#'   the order ranges appear in the format. If the format has no range
#'   entries, an empty data frame with the same columns is returned.
#'
#' @details
#' Range keys are stored internally as strings such as
#' \code{"0,18,TRUE,FALSE"}. \code{franges()} parses these keys back into
#' their numeric bounds and inclusivity flags, making it easy to inspect,
#' filter, or programmatically reuse range definitions.
#'
#' Bounds parsed as \code{HIGH} or \code{LOW} appear as \code{Inf} and
#' \code{-Inf} respectively.
#'
#' @export
#'
#' @examples
#' fparse(text = '
#' VALUE age (numeric)
#'   [0, 18)    = "Child"
#'   [18, 65)   = "Adult"
#'   [65, HIGH] = "Senior"
#'   .missing   = "Unknown"
#' ;
#' ')
#' franges("age")
#' fclear()
franges <- function(fmt) {
  if (is.character(fmt) && length(fmt) == 1L) {
    fmt <- format_get(fmt)
  }
  if (!inherits(fmt, "ks_format")) {
    cli_abort("{.arg fmt} must be a {.cls ks_format} object or the name of a registered format.")
  }

  empty <- data.frame(
    low = numeric(0),
    high = numeric(0),
    inc_low = logical(0),
    inc_high = logical(0),
    label = character(0),
    stringsAsFactors = FALSE
  )

  if (length(fmt$mappings) == 0L) return(empty)

  keys <- names(fmt$mappings)
  rows <- vector("list", length(keys))
  n <- 0L
  for (i in seq_along(keys)) {
    parsed <- .parse_range_key(keys[i])
    if (is.null(parsed)) next
    n <- n + 1L
    rows[[n]] <- data.frame(
      low = parsed$low,
      high = parsed$high,
      inc_low = parsed$inc_low,
      inc_high = parsed$inc_high,
      label = as.character(fmt$mappings[[i]]),
      stringsAsFactors = FALSE
    )
  }

  if (n == 0L) return(empty)
  do.call(rbind, rows[seq_len(n)])
}

#' Reverse-Lookup Range Bounds from Labels
#'
#' Given a vector of values that match the \emph{labels} of a range-based
#' format, returns the corresponding \code{low} / \code{high} bounds (and
#' inclusivity flags) for each input. Useful for reconstructing the
#' underlying range from a coded value.
#'
#' @param x A vector of values to look up against the format's labels.
#'   Coerced to character before matching.
#' @param fmt A \code{ks_format} object, or a character name of a format
#'   registered in the global library.
#'
#' @return A \code{data.frame} with one row per element of \code{x} and
#'   columns \code{low}, \code{high}, \code{inc_low}, \code{inc_high}.
#'   Rows where the input does not match any range label contain \code{NA}.
#'
#' @details
#' For \code{multilabel} formats where the same label maps to several
#' ranges, only the \emph{first} matching range is returned. For full
#' multi-match behaviour, call \code{\link{franges}()} directly and join
#' on \code{label}.
#'
#' @seealso \code{\link{franges}}
#'
#' @export
#'
#' @examples
#' fparse(text = '
#' VALUE visit_ther (numeric)
#'   [LOW,  1] =  0
#'   [ 8, 22] =  2
#'   [22, 36] =  4
#'   [37, 50] =  6
#' ;
#' ')
#' fmap_to_ranges(c(0, 2, 4, 6), "visit_ther")
#' fclear()
fmap_to_ranges <- function(x, fmt) {
  df <- franges(fmt)
  idx <- match(as.character(x), df$label)
  data.frame(
    low = df$low[idx],
    high = df$high[idx],
    inc_low = df$inc_low[idx],
    inc_high = df$inc_high[idx],
    stringsAsFactors = FALSE
  )
}

#' Parse Cache for Expression Labels
#'
#' Caches parsed R expressions (from \code{parse()}) keyed by the expression
#' string to avoid repeated parsing on successive \code{fput()} calls.
#'
#' @keywords internal
.expr_parse_cache <- new.env(hash = TRUE, parent = emptyenv())

#' Format Library (Global Environment for Storing Formats)
#'
#' @keywords internal
.format_library <- new.env(parent = emptyenv())

#' Register Format in Library (Internal)
#'
#' Stores a format definition in the global format library.
#'
#' @param format A ks_format or ks_invalue object
#' @param name Character. Name to store format under (uses format$name if NULL)
#' @param overwrite Logical. Allow overwriting existing format. Default TRUE.
#'
#' @return Invisibly returns the format object
#' @keywords internal
#' @noRd
.format_register <- function(format, name = NULL, overwrite = TRUE) {
  if (!inherits(format, c("ks_format", "ks_invalue"))) {
    cli_abort("{.arg format} must be a {.cls ks_format} or {.cls ks_invalue} object.")
  }

  if (is.null(name)) {
    name <- format$name
  }

  if (is.null(name) || is.na(name) || !nzchar(name)) {
    return(invisible(format))
  }

  if (exists(name, envir = .format_library) && !overwrite) {
    cli_abort("Format {.val {name}} already exists. Use {.code overwrite = TRUE} to replace.")
  }

  assign(name, format, envir = .format_library)
  invisible(format)
}

#' Retrieve Format from Library (Internal)
#'
#' @param name Character. Name of the format to retrieve
#' @return A ks_format or ks_invalue object
#' @keywords internal
#' @noRd
.format_get <- function(name) {
  if (!exists(name, envir = .format_library)) {
    # Case-insensitive lookup: try matching available names
    available <- ls(envir = .format_library)
    ci_match <- available[tolower(available) == tolower(name)]
    if (length(ci_match) == 1L) {
      return(get(ci_match, envir = .format_library))
    }
    if (length(ci_match) > 1L) {
      cli_abort(c(
        "Ambiguous format name {.val {name}}: multiple case-insensitive matches found.",
        "i" = "Matches: {.val {ci_match}}"
      ))
    }

    # Fallback: check if it's a built-in SAS datetime format
    if (.is_sas_datetime_format(name)) {
      fmt <- fnew_date(pattern = name, name = name)
      return(fmt)
    }

    if (length(available) == 0) {
      cli_abort(c(
        "Format {.val {name}} not found.",
        "i" = "Format library is empty."
      ))
    }
    cli_abort(c(
      "Format {.val {name}} not found.",
      "i" = "Available: {.val {available}}"
    ))
  }
  get(name, envir = .format_library)
}

#' Retrieve a Format from the Library
#'
#' Returns a format or invalue object by name. Used when you need the object
#' (e.g. for \code{\link{fput_df}} or \code{\link{fexport}}) rather than
#' applying by name with \code{\link{fput}}, \code{\link{fputn}}, or
#' \code{\link{fputc}}.
#'
#' @param name Character. Name of a registered format or invalue.
#' @return A \code{ks_format} or \code{ks_invalue} object.
#' @export
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' sex_fmt <- format_get("sex")
#' fput_df(data.frame(sex = c("M", "F")), sex = sex_fmt)
#' fclear()
format_get <- function(name) {
  .format_get(name)
}

#' Print Format(s) from Library
#'
#' Displays format information from the global format library.
#' When called without arguments, lists all registered format names.
#' When called with a name, displays the full definition of that format.
#'
#' @param name Character. Optional name of a specific format to display.
#'   If \code{NULL} (default), lists all registered formats.
#'
#' @return Invisible \code{NULL}. This function is for display only.
#'
#' @export
#'
#' @seealso \code{\link{flist}} for a programmatic alternative that returns
#'   a character vector of registered names.
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' flist()          # character vector of names
#' fprint()         # list all formats
#' fprint("sex")    # show specific format
#' fclear()
fprint <- function(name = NULL) {
  if (is.null(name)) {
    fnames <- ls(envir = .format_library)
    if (length(fnames) == 0) {
      cat("Format library is empty\n")
    } else {
      cat("Registered formats:\n")
      objs <- mget(fnames, envir = .format_library)
      for (nm in fnames) {
        obj <- objs[[nm]]
        type_str <- if (inherits(obj, "ks_format")) {
          if (obj$type %in% c("date", "time", "datetime")) {
            sas_info <- if (!is.null(obj$sas_name)) paste0(", ", obj$sas_name, ".") else ""
            paste0("VALUE (", obj$type, sas_info, ")")
          } else {
            paste0("VALUE (", obj$type, ")")
          }
        } else if (inherits(obj, "ks_invalue")) {
          paste0("INVALUE (", obj$target_type, ")")
        } else {
          "unknown"
        }
        n_mappings <- length(obj$mappings)
        cat("  ", nm, " - ", type_str, ", ", n_mappings, " mapping(s)\n", sep = "")
      }
    }
  } else {
    if (!exists(name, envir = .format_library)) {
      cli_abort(c(
        "Format {.val {name}} not found.",
        "i" = "Use {.fn fprint} to see available formats."
      ))
    }
    obj <- get(name, envir = .format_library)
    print(obj)
  }
  invisible(NULL)
}

#' List Format Names from Library
#'
#' Returns a character vector of all format and invalue names currently
#' registered in the global format library.
#'
#' @return A character vector of registered format names, sorted alphabetically.
#'   Returns \code{character(0)} if the library is empty.
#'
#' @export
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' flist()
#' fclear()
flist <- function() {
  sort(ls(envir = .format_library))
}

#' Remove Format(s) from Library
#'
#' Removes one or all formats from the global format library.
#' When called without arguments, clears all formats.
#' When called with a name, removes only that format.
#'
#' @param name Character. Optional name of a specific format to remove.
#'   If \code{NULL} (default), removes all formats.
#'
#' @return Invisible \code{NULL}
#'
#' @export
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", name = "sex")
#' fclear("sex")   # remove one format
#' fclear()        # remove all formats
fclear <- function(name = NULL) {
  if (is.null(name)) {
    nms <- ls(envir = .format_library)
    if (length(nms) > 0) {
      rm(list = nms, envir = .format_library)
    }
    cli_inform("All formats cleared from library.")
  } else {
    if (!exists(name, envir = .format_library)) {
      cli_warn("Format {.val {name}} not found in library.")
      return(invisible(NULL))
    }
    rm(list = name, envir = .format_library)
    cli_inform("Format {.val {name}} removed from library.")
  }
  invisible(NULL)
}

#' Validate Format Object (Internal)
#'
#' Validates the structure and content of a format object during
#' creation or parsing. Provides clear error descriptions for the user.
#'
#' @param format_obj A ks_format or ks_invalue object to validate
#' @return Invisibly returns the format object if valid
#' @keywords internal
#' @noRd
.format_validate <- function(format_obj) {
  if (inherits(format_obj, "ks_format")) {
    name_str <- if (!is.null(format_obj$name)) {
      paste0(" '", format_obj$name, "'")
    } else {
      ""
    }

    # Check that all labels are character strings (not for value types)
    if (length(format_obj$mappings) > 0 && !.is_value_type(format_obj$type)) {
      labels <- unlist(format_obj$mappings)
      non_char <- !vapply(labels, is.character, logical(1L))
      if (any(non_char)) {
        bad_idx <- which(non_char)
        bad_keys <- names(format_obj$mappings)[bad_idx]
        cli_abort(c(
          "Format{name_str}: All labels must be character strings.",
          "x" = "Non-character labels found for keys: {.val {bad_keys}}"
        ))
      }
    }

    # Check for duplicate keys
    keys <- names(format_obj$mappings)
    if (anyDuplicated(keys)) {
      dupes <- unique(keys[duplicated(keys)])
      cli_warn("Format{name_str}: Duplicate keys: {.val {dupes}}")
    }

    # For numeric type, validate that keys are valid numbers or ranges
    if (format_obj$type == "numeric" && length(format_obj$mappings) > 0) {
      for (key in keys) {
        parsed <- .parse_range_key(key)
        if (is.null(parsed) && is.na(suppressWarnings(as.numeric(key)))) {
          cli_warn("Format{name_str}: Key {.val {key}} is not a valid numeric value or range.")
        }
      }
    }

  } else if (inherits(format_obj, "ks_invalue")) {
    name_str <- if (!is.null(format_obj$name)) {
      paste0(" '", format_obj$name, "'")
    } else {
      ""
    }

    # Check for duplicate keys
    if (length(format_obj$mappings) > 0) {
      keys <- names(format_obj$mappings)
      if (anyDuplicated(keys)) {
        dupes <- unique(keys[duplicated(keys)])
        cli_warn("Invalue{name_str}: Duplicate keys: {.val {dupes}}")
      }
    }
  }

  invisible(format_obj)
}


# ===========================================================================
# Named vector expansion helper
# ===========================================================================

#' Expand Named Vectors in \code{list(...)} Output
#'
#' Internal helper that detects unnamed elements in a \code{list(...)} result
#' that are named atomic vectors or named lists, and expands them into
#' individual key-value pairs. This allows \code{fnew()}, \code{finput()}, and
#' \code{fnew_bid()} to accept named vectors alongside individual \code{...}
#' arguments.
#'
#' @param dots_list List. The result of \code{list(...)}.
#' @param reverse Logical. If \code{TRUE}, swap names and values for expanded
#'   entries (except \code{.missing} and \code{.other} directives).
#'   Used by \code{fnew()} for character/numeric types where the named-vector
#'   convention \code{c(Label = "Code")} is reversed relative to the internal
#'   \code{"Code" = "Label"} representation. Set to \code{FALSE} for value
#'   types (\code{Date}, \code{POSIXct}, \code{logical}) where names are
#'   already the input keys and values are native R objects. See
#'   \code{fnew()} details and \code{vignette("usage_examples")} Example 21.
#'   Default \code{FALSE}.
#' @return A flat named list with the same structure as a standard
#'   \code{list(...)} output.
#' @keywords internal
#' @noRd
.expand_named_vectors <- function(dots_list, reverse = FALSE) {
  if (length(dots_list) == 0L) return(dots_list)

  outer_names <- names(dots_list)
  result <- list()

  for (i in seq_along(dots_list)) {
    nm <- if (!is.null(outer_names)) outer_names[i] else ""
    elem <- dots_list[[i]]

    # Unnamed element that is a named vector/list -> expand
    if (is.na(nm) || !nzchar(nm)) {
      elem_names <- names(elem)

      if (is.null(elem_names) || !all(nzchar(elem_names))) {
        cli_abort("Unnamed argument at position {i} must be a fully named vector or list.")
      }

      for (j in seq_along(elem)) {
        key <- elem_names[j]
        val <- elem[[j]]

        if (reverse && !key %in% c(".missing", ".other")) {
          # Swap: name becomes value, value becomes name
          entry <- stats::setNames(list(key), as.character(val))
        } else {
          entry <- stats::setNames(list(val), key)
        }
        result <- c(result, entry)
      }
    } else {
      # Regular named scalar argument -> keep as-is
      entry <- stats::setNames(list(elem), nm)
      result <- c(result, entry)
    }
  }

  result
}
