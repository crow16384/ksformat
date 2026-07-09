#' Parse Format Definitions from 'SAS'-like Text
#'
#' Reads format definitions written in a human-friendly 'SAS'-like syntax
#' and returns a list of \code{ks_format} and/or \code{ks_invalue} objects.
#' All parsed formats are automatically stored in the global format library.
#'
#' @param text Character string or character vector containing format definitions.
#'   If a character vector, lines are concatenated with newlines.
#' @param file Path to a text file containing format definitions.
#'   Exactly one of \code{text} or \code{file} must be provided.
#' @param verbose Logical. If \code{TRUE}, the parsed formats are
#'   printed to the console.  Default is \code{FALSE} to suppress output
#'   (the result is returned invisibly).
#'
#' @return A named list of \code{ks_format} and/or \code{ks_invalue} objects.
#'   Names correspond to the format names defined in the text.
#'   All formats are automatically registered in the global format library.
#'
#' @details
#' The syntax supports two block types:
#'
#' \strong{VALUE} blocks define formats (value -> label):
#' \preformatted{
#' VALUE name (type)
#'   "value1" = "Label 1"
#'   "value2" = "Label 2"
#'   [low, high) = "Range Label (half-open)"
#'   (low, high] = "Range Label (open-low, closed-high)"
#'   pattern = "..."  # date/time/datetime patterns, or numeric display patterns
#'   .missing = "Missing Label"
#'   .other = "Other Label"
#' ;
#' }
#'
#' \strong{INVALUE} blocks define reverse formats (label -> numeric value):
#' \preformatted{
#' INVALUE name
#'   "Label 1" = 1
#'   "Label 2" = 2
#' ;
#' }
#'
#' \strong{Syntax rules:}
#' \itemize{
#'   \item Blocks start with \code{VALUE} or \code{INVALUE} keyword and end with \code{;}
#'   \item The type in parentheses is optional; defaults to \code{"auto"} for VALUE,
#'         \code{"numeric"} for INVALUE
#'   \\item Use \code{pattern = "..."} for date/time/datetime display patterns
#'         and \code{pattern: "..."} in the block header for numeric display patterns
#'   \item Values can be quoted or unquoted
#'   \item Ranges use interval notation with explicit bounds
#'   \item Legacy range syntax \code{low - high} is also supported
#'   \item Special range keywords: \code{LOW} (-Inf) and \code{HIGH} (Inf)
#'   \item \code{.missing} and \code{.other} are special directives
#'   \item Lines starting with \code{/*}, \code{*}, \code{//}, or \code{#} are comments
#' }
#'
#' \strong{Block options:}
#'
#' Comma-separated options can be placed inside the parentheses after the type:
#' \itemize{
#'   \item \code{nocase} — enables case-insensitive key matching (equivalent to
#'         \code{ignore_case = TRUE} in \code{\link{fnew}}).
#'   \item \code{multilabel} — allows overlapping ranges where a single value
#'         matches multiple labels (used with \code{\link{fput_all}}).
#' }
#' Options can be combined: \code{VALUE name (character, nocase, multilabel)}.
#'
#' @export
#' @examples
#' # Parse multiple format definitions from text
#' fparse(text = '
#' VALUE sex (character)
#'   "M" = "Male"
#'   "F" = "Female"
#'   .missing = "Unknown"
#' ;
#'
#' VALUE age (numeric)
#'   [0, 18)    = "Child"
#'   [18, 65)   = "Adult"
#'   [65, HIGH]  = "Senior"
#'   .missing   = "Age Unknown"
#' ;
#'
#' // Invalue block
#' INVALUE race_inv
#'   "White" = 1
#'   "Black" = 2
#'   "Asian" = 3
#' ;
#' ')
#'
#' fput(c("M", "F", NA), "sex")
#' fputn(c(5, 25, 70, NA), "age")
#' finputn(c("White", "Black"), "race_inv")
#' flist()
#' fprint()
#' fclear()
#'
#' # Parse date/time/datetime format definitions
#' fparse(text = '
#' VALUE enrldt (date)
#'   pattern = "DATE9."
#'   .missing = "Not Enrolled"
#' ;
#'
#' VALUE visit_time (time)
#'   pattern = "TIME8."
#' ;
#'
#' VALUE stamp (datetime)
#'   pattern = "DATETIME20."
#' ;
#' ')
#'
#' fput(as.Date("2025-03-01"), "enrldt")
#' fput(36000, "visit_time")
#' fput(as.POSIXct("2025-03-01 10:00:00", tz = "UTC"), "stamp")
#' fclear()
#'
#' # Numeric display pattern format
#' fparse(text = '
#' VALUE currency (numeric, pattern: "$%,.2f")
#'   .missing = "NO DATA"
#' ;
#' ')
#' fputn(c(1234.56, -7890.12, NA), "currency")
#' fclear()
#'
#' # Case-insensitive format (nocase option)
#' fparse(text = '
#' VALUE yesno (character, nocase)
#'   "Y" = "Yes"
#'   "N" = "No"
#'   .other = "Unknown"
#' ;
#' ')
#' fput(c("y", "N", "YES"), "yesno")
#' # [1] "Yes" "No" "Unknown"
#' fclear()
#'
#' # Parse multilabel format
#' fparse(text = '
#' VALUE risk (numeric, multilabel)
#'   [0, 3]  = "Low Risk"
#'   [0, 7]  = "Monitored"
#'   (3, 7]  = "Medium Risk"
#'   (7, 10] = "High Risk"
#' ;
#' ')
#' fput_all(c(2, 5, 9), "risk")
#' fclear()
fparse <- function(text = NULL, file = NULL, verbose = FALSE) {
  if (is.null(text) && is.null(file)) {
    cli_abort("Either {.arg text} or {.arg file} must be provided.")
  }
  if (!is.null(text) && !is.null(file)) {
    cli_abort("Only one of {.arg text} or {.arg file} should be provided, not both.")
  }
  if (!is.null(text) && !is.character(text)) {
    cli_abort("{.arg text} must be a character string or character vector.")
  }
  if (!is.null(file) && (!is.character(file) || length(file) != 1L)) {
    cli_abort("{.arg file} must be a single file path string.")
  }

  if (!is.null(file)) {
    if (!file.exists(file)) {
      cli_abort("File not found: {.file {file}}")
    }
    lines <- readLines(file, warn = FALSE)
  } else {
    if (length(text) > 1) {
      text <- paste(text, collapse = "\n")
    }
    lines <- strsplit(text, "\n")[[1]]
  }

  # Parse into blocks
  blocks <- .parse_blocks(lines)

  # Convert blocks to format objects and auto-register
  result <- list()
  for (block in blocks) {
    obj <- .block_to_format(block)

    # Validate
    .format_validate(obj)

    # Auto-register in library
    .format_register(obj, name = block$name, overwrite = TRUE)

    result[[block$name]] <- obj
  }

  if (verbose) result else invisible(result)
}


# ---------------------------------------------------------------------------
# Internal parser helpers
# ---------------------------------------------------------------------------

#' Parse text lines into block structures
#' @keywords internal
.parse_blocks <- function(lines) {
  blocks <- list()
  current_block <- NULL
  in_block <- FALSE

  for (i in seq_along(lines)) {
    line <- trimws(lines[i])

    # Skip empty lines and comments
    if (line == "" || grepl("^(/\\*|\\*|//|#)", line)) {
      next
    }

    # Check for block start: VALUE or INVALUE
    block_match <- regmatches(line, regexec(
      "^(VALUE|INVALUE)\\s+([\\w.-]+)\\s*(?:\\(([^)]+)\\))?\\s*$",
      line, ignore.case = TRUE, perl = TRUE
    ))[[1]]

    if (length(block_match) >= 3) {
      if (in_block) {
        cli_warn(c(
          "Line {i}: New block started before previous block ended with {.val ;}.",
          "i" = "Closing previous block {.val {current_block$name}}."
        ))
        blocks <- c(blocks, list(current_block))
      }

      block_type <- toupper(block_match[2])
      block_name <- block_match[3]

      block_subtype <- if (length(block_match) >= 4 && block_match[4] != "") {
        trimws(block_match[4])
      } else {
        # Default: "auto" for VALUE, "numeric" for INVALUE
        if (block_type == "INVALUE") "numeric" else "auto"
      }

      # Parse multilabel flag from subtype
      block_multilabel <- FALSE
      if (grepl("multilabel", block_subtype, ignore.case = TRUE)) {
        block_multilabel <- TRUE
        block_subtype <- gsub(",?\\s*multilabel", "", block_subtype,
                              ignore.case = TRUE)
        block_subtype <- gsub("multilabel\\s*,?", "", block_subtype,
                              ignore.case = TRUE)
        block_subtype <- trimws(block_subtype)
        if (block_subtype == "") {
          block_subtype <- if (block_type == "INVALUE") "numeric" else "auto"
        }
      }

      # Parse nocase flag from subtype
      block_nocase <- FALSE
      if (grepl("nocase", block_subtype, ignore.case = TRUE)) {
        block_nocase <- TRUE
        block_subtype <- gsub(",?\\s*nocase", "", block_subtype,
                              ignore.case = TRUE)
        block_subtype <- gsub("nocase\\s*,?", "", block_subtype,
                              ignore.case = TRUE)
        block_subtype <- trimws(block_subtype)
        if (block_subtype == "") {
          block_subtype <- if (block_type == "INVALUE") "numeric" else "auto"
        }
      }

      # Parse format: spec from subtype (e.g. "format: %Y-%m-%d")
      block_date_format <- NULL
      fmt_match <- regmatches(block_subtype, regexec(
        "format:\\s*([^,)]+)", block_subtype, ignore.case = TRUE
      ))[[1]]
      if (length(fmt_match) >= 2 && fmt_match[1] != "") {
        block_date_format <- trimws(fmt_match[2])
        block_subtype <- gsub(",?\\s*format:\\s*[^,)]+", "", block_subtype,
                              ignore.case = TRUE)
        block_subtype <- gsub("format:\\s*[^,)]+\\s*,?", "", block_subtype,
                              ignore.case = TRUE)
        block_subtype <- trimws(block_subtype)
        if (block_subtype == "") {
          block_subtype <- if (block_type == "INVALUE") "numeric" else "auto"
        }
      }

      # Parse range_subtype: option for stratified_range
      block_range_subtype <- NULL
      rs_match <- regmatches(block_subtype, regexec(
        "range_subtype:\\s*([A-Za-z]+)", block_subtype, ignore.case = TRUE
      ))[[1]]
      if (length(rs_match) >= 2 && rs_match[1] != "") {
        block_range_subtype <- tolower(trimws(rs_match[2]))
        block_subtype <- gsub(",?\\s*range_subtype:\\s*[A-Za-z]+", "",
                              block_subtype, ignore.case = TRUE)
        block_subtype <- gsub("range_subtype:\\s*[A-Za-z]+\\s*,?", "",
                              block_subtype, ignore.case = TRUE)
        block_subtype <- trimws(block_subtype)
        if (block_subtype == "") block_subtype <- "auto"
      }

      # Parse strata_sep: option for stratified_range
      block_strata_sep <- NULL
      ss_match <- regmatches(block_subtype, regexec(
        "strata_sep:\\s*([^,)\\s]+)", block_subtype, ignore.case = TRUE
      ))[[1]]
      if (length(ss_match) >= 2 && ss_match[1] != "") {
        block_strata_sep <- trimws(ss_match[2])
        block_subtype <- gsub(",?\\s*strata_sep:\\s*[^,)\\s]+", "",
                              block_subtype, ignore.case = TRUE)
        block_subtype <- gsub("strata_sep:\\s*[^,)\\s]+\\s*,?", "",
                              block_subtype, ignore.case = TRUE)
        block_subtype <- trimws(block_subtype)
        if (block_subtype == "") block_subtype <- "auto"
      }

      # Parse pattern: option for numeric display formats
      block_num_pattern <- NULL
      pattern_match <- regmatches(block_subtype, regexec(
        "pattern:\\s*(?:\"([^\"]*)\"|'([^']*)'|([^,)]+))",
        block_subtype, ignore.case = TRUE, perl = TRUE
      ))[[1]]
      if (length(pattern_match) >= 2 && pattern_match[1] != "") {
        block_num_pattern <- if (length(pattern_match) >= 2 && nzchar(pattern_match[2])) {
          pattern_match[2]
        } else if (length(pattern_match) >= 3 && nzchar(pattern_match[3])) {
          pattern_match[3]
        } else {
          pattern_match[4]
        }
        block_subtype <- gsub(
          ",?\\s*pattern:\\s*(?:\"[^\"]*\"|'[^']*'|[^,)]+)",
          "",
          block_subtype,
          ignore.case = TRUE,
          perl = TRUE
        )
        block_subtype <- trimws(block_subtype)
        if (block_subtype == "") block_subtype <- if (block_type == "INVALUE") "numeric" else "auto"
      }

      current_block <- list(
        type = block_type,
        name = block_name,
        subtype = block_subtype,
        multilabel = block_multilabel,
        nocase = block_nocase,
        date_format = block_date_format,
        num_pattern = block_num_pattern,
        range_subtype = block_range_subtype,
        strata_sep = block_strata_sep,
        entries = list(),
        line_start = i
      )
      in_block <- TRUE
      next
    }

    # Warn if line looks like a block header but didn't match
    if (!in_block && grepl("^(VALUE|INVALUE)\\b", line, ignore.case = TRUE)) {
      cli_warn(c(
        "Line {i}: Looks like a block header but could not be parsed: {.val {line}}",
        "i" = "Check the format name or syntax."
      ))
      next
    }

    # Check for block end (standalone ; or trailing ;)
    ends_with_semi <- grepl(";\\s*$", line)
    if (grepl("^;\\s*$", line)) {
      if (in_block && !is.null(current_block)) {
        blocks <- c(blocks, list(current_block))
        current_block <- NULL
        in_block <- FALSE
      }
      next
    }

    # Warn about lines outside any block
    if (!in_block) {
      cli_warn("Line {i}: Ignoring line outside of any block: {.val {line}}")
      next
    }

    # Parse mapping line within a block
    if (!is.null(current_block)) {
      # Strip trailing comma and/or semicolon from mapping lines
      mapping_line <- sub("[,;]+\\s*$", "", line)
      entry <- .parse_mapping_line(mapping_line, i)
      if (!is.null(entry)) {
        current_block$entries <- c(current_block$entries, list(entry))
      }
      # Close block if line ended with ;
      if (ends_with_semi) {
        blocks <- c(blocks, list(current_block))
        current_block <- NULL
        in_block <- FALSE
      }
    }
  }

  # Handle unclosed block
  if (in_block && !is.null(current_block)) {
    cli_warn("Block {.val {current_block$name}} was not closed with {.val ;}. Closing automatically.")
    blocks <- c(blocks, list(current_block))
  }

  return(blocks)
}


#' Parse a single mapping line
#' @keywords internal
.parse_mapping_line <- function(line, line_num) {
  # Remove inline comments
  line <- sub("\\s*//.*$", "", line)
  line <- sub("\\s*/\\*.*\\*/\\s*$", "", line)
  line <- trimws(line)

  if (line == "") return(NULL)

  # Split on '='
  eq_pos <- regexpr("=", line)
  if (eq_pos < 0) {
    cli_warn("Line {line_num}: Could not parse mapping (no {.val =} found): {.val {line}}")
    return(NULL)
  }

  lhs <- trimws(substring(line, 1, eq_pos - 1))
  rhs <- trimws(substring(line, eq_pos + 1))

  # Check for (eval) marker before unquoting
  has_eval <- grepl("\\(eval\\)\\s*$", rhs)
  if (has_eval) {
    rhs <- trimws(sub("\\s*\\(eval\\)\\s*$", "", rhs))
  }

  # Handle unquoted NA/NaN literals before unquoting (e.g. .other = NA)
  is_quoted <- grepl("^([\"']).*\\1$", rhs)
  if (!is_quoted && rhs %in% c("NA", "NaN")) {
    rhs <- NA_character_
  } else {
    rhs <- .unquote(rhs)
  }

  # Set eval attribute if (eval) marker was found
  if (has_eval) {
    attr(rhs, "eval") <- TRUE
  }

  # Check for .missing / .other directives
  if (lhs == ".missing") {
    return(list(type = "missing", value = rhs))
  }
  if (lhs == ".other") {
    return(list(type = "other", value = rhs))
  }

  # Check for interval notation: [low, high) or (low, high] etc.
  interval_match <- regmatches(lhs, regexec(
    "^(\\[|\\()\\s*(-?[0-9]+(?:\\.[0-9]+)?|LOW|HIGH|Inf|-Inf)\\s*,\\s*(-?[0-9]+(?:\\.[0-9]+)?|LOW|HIGH|Inf|-Inf)\\s*(\\]|\\))$",
    lhs, ignore.case = TRUE
  ))[[1]]

  if (length(interval_match) == 5) {
    left_bracket <- interval_match[2]
    low_str <- trimws(interval_match[3])
    high_str <- trimws(interval_match[4])
    right_bracket <- interval_match[5]

    low_val <- .parse_range_bound(low_str, is_low = TRUE)
    high_val <- .parse_range_bound(high_str, is_low = FALSE)

    inc_low <- (left_bracket == "[")
    inc_high <- (right_bracket == "]")

    if (!is.na(low_val) && !is.na(high_val)) {
      return(list(type = "range", low = low_val, high = high_val,
                  inc_low = inc_low, inc_high = inc_high, label = rhs))
    }
  }

  # Check for date / datetime interval notation: [YYYY-MM-DD, YYYY-MM-DD)
  # or [YYYY-MM-DD HH:MM[:SS], ...]. Bounds are kept as strings so the
  # range_table builder can dispatch to .parse_date_range_key() /
  # .parse_datetime_range_key() using the format's date_format.
  date_bound <- "(\\d{4}-\\d{2}-\\d{2}(?:[ T]\\d{2}:\\d{2}(?::\\d{2})?)?|LOW|HIGH)"
  date_iv_re <- paste0("^(\\[|\\()\\s*", date_bound, "\\s*,\\s*",
                       date_bound, "\\s*(\\]|\\))$")
  date_iv_match <- regmatches(lhs, regexec(date_iv_re, lhs,
                                           ignore.case = TRUE))[[1]]

  if (length(date_iv_match) == 5) {
    left_bracket  <- date_iv_match[2]
    low_str       <- trimws(date_iv_match[3])
    high_str      <- trimws(date_iv_match[4])
    right_bracket <- date_iv_match[5]

    inc_low  <- (left_bracket == "[")
    inc_high <- (right_bracket == "]")

    return(list(type = "range", low = low_str, high = high_str,
                inc_low = inc_low, inc_high = inc_high, label = rhs,
                bound_kind = "date"))
  }

  # Check for legacy range: low - high pattern (no brackets)
  range_match <- regmatches(lhs, regexec(
    "^(-?[0-9]+(?:\\.[0-9]+)?|LOW|HIGH|Inf|-Inf)\\s*-\\s*(-?[0-9]+(?:\\.[0-9]+)?|LOW|HIGH|Inf|-Inf)$",
    lhs, ignore.case = TRUE
  ))[[1]]

  if (length(range_match) == 3) {
    low_str <- trimws(range_match[2])
    high_str <- trimws(range_match[3])

    low_val <- .parse_range_bound(low_str, is_low = TRUE)
    high_val <- .parse_range_bound(high_str, is_low = FALSE)

    if (!is.na(low_val) && !is.na(high_val)) {
      # Legacy syntax defaults to [low, high)
      return(list(type = "range", low = low_val, high = high_val,
                  inc_low = TRUE, inc_high = FALSE, label = rhs))
    }
  }

  # Discrete mapping
  lhs <- .unquote(lhs)
  return(list(type = "discrete", key = lhs, label = rhs))
}


#' Parse a range bound value
#' @keywords internal
.parse_range_bound <- function(s, is_low = TRUE) {
  s_upper <- toupper(s)

  if (s_upper == "LOW") return(-Inf)
  if (s_upper == "HIGH") return(Inf)

  val <- suppressWarnings(as.numeric(s))
  return(val)
}


#' Remove surrounding quotes from a string
#' @keywords internal
.unquote <- function(s) {
  if (is.na(s) || nchar(s) < 2L) return(s)
  if (grepl("^([\"']).*\\1$", s)) {
    return(substring(s, 2, nchar(s) - 1))
  }
  s
}


#' Convert a parsed block to a ks_format or ks_invalue object
#' @keywords internal
.block_to_format <- function(block) {
  if (block$type == "VALUE") {
    return(.block_to_ks_format(block))
  } else if (block$type == "INVALUE") {
    return(.block_to_ks_invalue(block))
  } else {
    cli_abort("Unknown block type: {.val {block$type}}.")
  }
}


#' Convert VALUE block to ks_format
#' @keywords internal
.block_to_ks_format <- function(block) {
  # Value types (case-sensitive: "Date", "POSIXct", "logical")
  if (block$subtype %in% .value_types) {
    return(.block_to_value_type_format(block))
  }

  # Handle date/time/datetime blocks (case-insensitive)
  if (tolower(block$subtype) %in% c("date", "time", "datetime")) {
    return(.block_to_ks_datetime_format(block))
  }

  # Stratified range blocks: build mappings then delegate to fnew()
  if (identical(tolower(block$subtype), "stratified_range")) {
    return(.block_to_stratified_range_format(block))
  }

  # Numeric display pattern formats: VALUE name (numeric, pattern: "...")
  if (tolower(block$subtype) %in% c("auto", "numeric") &&
      (!is.null(block$num_pattern) || any(vapply(block$entries, function(e) {
        identical(e$type, "discrete") && tolower(e$key) == "pattern"
      }, logical(1L)))) ) {
    return(.block_to_numeric_pattern_format(block))
  }

  mappings <- list()
  missing_label <- NULL
  other_label <- NULL

  for (entry in block$entries) {
    if (entry$type == "missing") {
      missing_label <- entry$value
    } else if (entry$type == "other") {
      other_label <- entry$value
    } else if (entry$type == "discrete") {
      mappings[[entry$key]] <- entry$label
    } else if (entry$type == "range") {
      inc_low <- if (!is.null(entry$inc_low)) entry$inc_low else TRUE
      inc_high <- if (!is.null(entry$inc_high)) entry$inc_high else FALSE
      range_key <- paste0(entry$low, ",", entry$high, ",",
                          toupper(inc_low), ",", toupper(inc_high))
      mappings[[range_key]] <- entry$label
    }
  }

  # Build the format
  type <- block$subtype

  format_obj <- structure(
    list(
      name = block$name,
      type = type,
      mappings = mappings,
      missing_label = missing_label,
      other_label = other_label,
      multilabel = isTRUE(block$multilabel),
      ignore_case = isTRUE(block$nocase),
      date_format = block$date_format,
      created = Sys.time()
    ),
    class = "ks_format"
  )

  # Auto-detect type if needed
  if (type == "auto") {
    has_ranges <- any(vapply(block$entries, function(e) identical(e$type, "range"), logical(1L)))
    has_date_bounds <- any(vapply(block$entries, function(e) {
      identical(e$type, "range") && identical(e$bound_kind, "date")
    }, logical(1L)))
    if (has_date_bounds) {
      # Choose date_range vs datetime_range based on whether any bound
      # carries a time component.
      has_time <- any(vapply(block$entries, function(e) {
        identical(e$type, "range") && identical(e$bound_kind, "date") &&
          (grepl("[ T]\\d{2}:\\d{2}", as.character(e$low)) ||
           grepl("[ T]\\d{2}:\\d{2}", as.character(e$high)))
      }, logical(1L)))
      format_obj$type <- if (has_time) "datetime_range" else "date_range"
    } else if (has_ranges) {
      format_obj$type <- "numeric"
    } else {
      format_obj$type <- detect_format_type(names(mappings))
    }
  }

  format_obj$range_table <- .build_range_table(mappings, format_obj$type,
                                               format_obj$date_format)

  return(format_obj)
}


#' Convert VALUE block with numeric display pattern to ks_format
#' @keywords internal
#' @noRd
.block_to_numeric_pattern_format <- function(block) {
  pattern <- block$num_pattern
  missing_label <- NULL
  other_label <- NULL

  pattern_entry <- NULL
  other_entries <- 0L

  for (entry in block$entries) {
    if (entry$type == "missing") {
      missing_label <- entry$value
    } else if (entry$type == "other") {
      other_label <- entry$value
    } else if (entry$type == "discrete" && tolower(entry$key) == "pattern") {
      pattern_entry <- entry$label
    } else if (entry$type %in% c("discrete", "range")) {
      other_entries <- other_entries + 1L
    }
  }

  if (is.null(pattern) && !is.null(pattern_entry)) {
    pattern <- pattern_entry
  }

  if (is.null(pattern)) {
    cli_abort(c(
      "Numeric pattern format {.val {block$name}} must define a {.val pattern}.",
      "i" = 'Use {.val pattern: "$%,.2f"} inside the block header or {.val pattern = "$%,.2f"} inside the block body.'
    ))
  }

  if (!is.null(block$num_pattern) && !is.null(pattern_entry) &&
      !identical(block$num_pattern, pattern_entry)) {
    cli_abort(c(
      "Conflicting numeric patterns were provided for {.val {block$name}}.",
      "i" = "Keep only one pattern source in the block."
    ))
  }

  if (other_entries > 0L) {
    cli_abort(c(
      "Numeric pattern formats cannot mix a pattern with explicit mappings.",
      "i" = "Keep only {.val .missing} / {.val .other} directives in the block."
    ))
  }

  fmt <- fnew(
    pattern,
    name = block$name,
    type = "numeric",
    .missing = missing_label,
    .other = other_label
  )

  return(fmt)
}


#' Convert VALUE block with stratified_range type to ks_format
#'
#' Stratified blocks support both canonical keys
#' (\code{"STRATUM<sep>low,high,inc_low,inc_high"}) supplied as quoted
#' discrete LHS, and the friendly interval form
#' (\code{STRATUM <sep> [low, high)}). Per-stratum directives appear as
#' \code{"STRATUM<sep>.missing" = "..."} or, with the \code{.missing|S}
#' shorthand, as plain discrete entries that are recognised here.
#' @keywords internal
#' @noRd
.block_to_stratified_range_format <- function(block) {
  strata_sep <- if (!is.null(block$strata_sep)) block$strata_sep else "|"
  range_subtype <- if (!is.null(block$range_subtype)) block$range_subtype else "numeric"
  date_format <- block$date_format

  # Escape strata_sep for regex use (special chars: . | ( ) [ ] { } * + ? ^ $ \)
  sep_re <- gsub("([\\.|()\\[\\]{}*+?^$\\\\])", "\\\\\\1", strata_sep,
                 perl = TRUE)

  num_token <- "(?:-?[0-9]+(?:\\.[0-9]+)?|LOW|HIGH|Inf|-Inf)"
  date_token <- "(?:\\d{4}-\\d{2}-\\d{2}(?:[ T]\\d{2}:\\d{2}(?::\\d{2})?)?|LOW|HIGH)"

  # Pre-build the friendly-interval regex once (used per entry below).
  friendly_tok <- if (range_subtype == "numeric") num_token else date_token
  friendly_pat <- paste0("^(.+?)", sep_re, "\\s*(\\[|\\()\\s*(",
                         friendly_tok, ")\\s*,\\s*(", friendly_tok,
                         ")\\s*(\\]|\\))\\s*$")

  mappings <- list()
  missing_label <- NULL
  other_label <- NULL

  canonicalise <- function(key) {
    if (startsWith(key, paste0(".missing", strata_sep))) {
      return(list(kind = "missing_stratum",
                  stratum = substr(key,
                                   nchar(".missing") + nchar(strata_sep) + 1L,
                                   nchar(key))))
    }
    if (startsWith(key, paste0(".other", strata_sep))) {
      return(list(kind = "other_stratum",
                  stratum = substr(key,
                                   nchar(".other") + nchar(strata_sep) + 1L,
                                   nchar(key))))
    }
    # Already canonical?
    split_try <- .split_stratified_key(key, strata_sep, range_subtype,
                                        date_format)
    if (!is.null(split_try)) {
      return(list(kind = "canonical", key = key))
    }
    # Friendly interval: stratum<sep>[low,high)
    m <- regmatches(key, regexec(friendly_pat, key, perl = TRUE))[[1]]
    if (length(m) == 6L) {
      stratum <- .unquote(trimws(m[2]))
      inc_low <- (m[3] == "[")
      low_str <- m[4]
      high_str <- m[5]
      inc_high <- (m[6] == "]")
      norm_bound <- function(s) {
        su <- toupper(s)
        if (range_subtype == "numeric") {
          if (su %in% c("LOW", "-INF")) return("-Inf")
          if (su %in% c("HIGH", "INF")) return("Inf")
          return(s)
        }
        # date / datetime
        if (su %in% c("LOW", "-INF")) return("LOW")
        if (su %in% c("HIGH", "INF")) return("HIGH")
        s
      }
      low_norm <- norm_bound(low_str)
      high_norm <- norm_bound(high_str)
      canon <- paste(low_norm, high_norm, toupper(inc_low),
                     toupper(inc_high), sep = ",")
      return(list(kind = "canonical",
                  key = paste0(stratum, strata_sep, canon)))
    }
    NULL
  }

  for (entry in block$entries) {
    if (entry$type == "missing") {
      missing_label <- entry$value
      next
    }
    if (entry$type == "other") {
      other_label <- entry$value
      next
    }
    if (entry$type == "range") {
      cli_warn(c(
        "Bare range entry in stratified_range block has no stratum; ignoring.",
        "i" = "Use {.val STRATUM{strata_sep}[low, high)} syntax."
      ))
      next
    }
    if (entry$type != "discrete") next

    info <- canonicalise(entry$key)
    if (is.null(info)) {
      cli_warn(c(
        "Could not parse stratified entry {.val {entry$key}}; treating as discrete.",
        "i" = "Expected {.val STRATUM{strata_sep}RANGE} key."
      ))
      mappings[[entry$key]] <- entry$label
      next
    }
    if (info$kind == "canonical") {
      mappings[[info$key]] <- entry$label
    } else if (info$kind == "missing_stratum") {
      mappings[[paste0(".missing", strata_sep, info$stratum)]] <- entry$label
    } else if (info$kind == "other_stratum") {
      mappings[[paste0(".other", strata_sep, info$stratum)]] <- entry$label
    }
  }

  # Assemble extras for fnew()
  extras <- list()
  if (!is.null(missing_label)) extras[[".missing"]] <- missing_label
  if (!is.null(other_label)) extras[[".other"]] <- other_label

  do.call(fnew, c(
    mappings, extras,
    list(
      name = block$name,
      type = "stratified_range",
      range_subtype = range_subtype,
      strata_sep = strata_sep,
      date_format = date_format,
      multilabel = isTRUE(block$multilabel),
      ignore_case = isTRUE(block$nocase)
    )
  ))
}


#' Convert VALUE block with a value type (Date/POSIXct/logical) to ks_format
#' @keywords internal
#' @noRd
.block_to_value_type_format <- function(block) {
  vtype <- block$subtype
  date_format <- block$date_format

  mappings <- list()

  for (entry in block$entries) {
    if (entry$type == "missing" || entry$type == "other") {
      # Silently ignore — value type formats always use typed NA
    } else if (entry$type == "discrete") {
      mappings[[entry$key]] <- .parse_typed_value(entry$label, vtype, date_format)
    } else if (entry$type == "range") {
      inc_low <- if (!is.null(entry$inc_low)) entry$inc_low else TRUE
      inc_high <- if (!is.null(entry$inc_high)) entry$inc_high else FALSE
      range_key <- paste0(entry$low, ",", entry$high, ",",
                          toupper(inc_low), ",", toupper(inc_high))
      mappings[[range_key]] <- .parse_typed_value(entry$label, vtype, date_format)
    }
  }

  format_obj <- structure(
    list(
      name = block$name,
      type = vtype,
      mappings = mappings,
      missing_label = NULL,
      other_label = NULL,
      multilabel = FALSE,
      ignore_case = isTRUE(block$nocase),
      date_format = date_format,
      created = Sys.time()
    ),
    class = "ks_format"
  )

  format_obj
}


#' Convert VALUE block with date/time/datetime type to ks_format
#' @keywords internal
.block_to_ks_datetime_format <- function(block) {
  # Look for a pattern entry (discrete mapping with key "pattern")
  pattern <- NULL
  missing_label <- NULL

  for (entry in block$entries) {
    if (entry$type == "discrete" && tolower(entry$key) == "pattern") {
      pattern <- entry$label
    } else if (entry$type == "missing") {
      missing_label <- entry$value
    }
  }

  if (is.null(pattern)) {
    cli_abort(c(
      "Date/time format {.val {block$name}} must have a {.val pattern} entry.",
      "i" = 'e.g., pattern = "DATE9."'
    ))
  }

  fmt <- fnew_date(
    pattern = pattern,
    name = block$name,
    type = tolower(block$subtype)
  )

  if (!is.null(missing_label)) {
    fmt$missing_label <- missing_label
  }

  return(fmt)
}


#' Convert INVALUE block to ks_invalue
#' @keywords internal
.block_to_ks_invalue <- function(block) {
  mappings <- list()

  for (entry in block$entries) {
    if (entry$type == "discrete") {
      mappings[[entry$key]] <- entry$label
    } else if (entry$type == "missing") {
      mappings[[".missing_marker"]] <- entry$value
    }
  }

  # Extract missing_value if set
  missing_value <- NA
  if (".missing_marker" %in% names(mappings)) {
    mv <- mappings[[".missing_marker"]]
    if (toupper(mv) == "NA" || mv == ".") {
      missing_value <- NA
    } else {
      missing_value <- mv
    }
    mappings[[".missing_marker"]] <- NULL
  }

  # Use block subtype; for INVALUE blocks, default is "numeric"
  target_type <- block$subtype

  invalue_obj <- structure(
    list(
      name = block$name,
      target_type = target_type,
      mappings = mappings,
      missing_value = missing_value,
      ignore_case = isTRUE(block$nocase),
      created = Sys.time()
    ),
    class = "ks_invalue"
  )

  return(invalue_obj)
}

