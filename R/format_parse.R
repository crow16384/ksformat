#' Parse Format Definitions from SAS-like Text
#'
#' Reads format definitions written in a human-friendly SAS-like syntax
#' and returns a list of \code{ks_format} and/or \code{ks_invalue} objects.
#'
#' @param text Character string or character vector containing format definitions.
#'   If a character vector, lines are concatenated with newlines.
#' @param file Path to a text file containing format definitions.
#'   Exactly one of \code{text} or \code{file} must be provided.
#' @param register Logical. If \code{TRUE}, parsed formats are automatically
#'   registered in the global format library. Default is \code{FALSE}.
#'
#' @return A named list of \code{ks_format} and/or \code{ks_invalue} objects.
#'   Names correspond to the format names defined in the text.
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
#'   .missing = "Missing Label"
#'   .other = "Other Label"
#' ;
#' }
#'
#' \strong{INVALUE} blocks define reverse formats (label -> value):
#' \preformatted{
#' INVALUE name (target_type)
#'   "Label 1" = "value1"
#'   "Label 2" = "value2"
#' ;
#' }
#'
#' \strong{Syntax rules:}
#' \itemize{
#'   \item Blocks start with \code{VALUE} or \code{INVALUE} keyword and end with \code{;}
#'   \item The type in parentheses is optional; defaults to \code{"auto"}
#'   \item Values can be quoted or unquoted
#'   \item Ranges use interval notation with explicit bounds:
#'     \code{[0, 18)} (inclusive low, exclusive high),
#'     \code{(0, 100]} (exclusive low, inclusive high),
#'     \code{[0, 100]} (both inclusive),
#'     \code{(0, 100)} (both exclusive)
#'   \item Legacy range syntax \code{low - high} is also supported and defaults to \code{[low, high)}
#'   \item Decimal numbers are fully supported: \code{[0.5, 1.5)}
#'   \item Special range keywords: \code{LOW} (-Inf) and \code{HIGH} (Inf)
#'   \item \code{.missing} and \code{.other} are special directives
#'   \item Lines starting with \code{/*} or \code{*} or \code{//} are comments
#'   \item Blank lines are ignored
#' }
#'
#' @export
#' @examples
#' txt <- '
#' VALUE sex (character)
#'   "M" = "Male"
#'   "F" = "Female"
#'   .missing = "Unknown"
#' ;
#'
#' VALUE age (numeric)
#'   [0, 18) = "Child"
#'   [18, 65) = "Adult"
#'   [65, HIGH] = "Senior"
#' ;
#' '
#'
#' formats <- format_parse(text = txt)
#' format_apply(c("M", "F", NA), formats$sex)
format_parse <- function(text = NULL, file = NULL, register = FALSE) {
  # Validate inputs
  if (is.null(text) && is.null(file)) {
    stop("Either 'text' or 'file' must be provided")
  }
  if (!is.null(text) && !is.null(file)) {
    stop("Only one of 'text' or 'file' should be provided, not both")
  }

  # Read input
  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("File not found: ", file)
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

  # Convert blocks to format objects
  result <- list()
  for (block in blocks) {
    obj <- .block_to_format(block)
    result[[block$name]] <- obj

    if (register) {
      format_register(obj, name = block$name, overwrite = TRUE)
    }
  }

  return(result)
}


#' Export Formats to SAS-like Text
#'
#' Converts \code{ks_format} and/or \code{ks_invalue} objects to
#' human-readable SAS-like text representation.
#'
#' @param ... Named \code{ks_format} or \code{ks_invalue} objects to export.
#' @param formats A named list of format objects. Alternative to \code{...}.
#' @param file Optional file path to write the output to. If \code{NULL},
#'   returns the text as a character string.
#'
#' @return If \code{file} is \code{NULL}, returns a character string with the
#'   SAS-like text. If \code{file} is specified, writes to the file and returns
#'   the path invisibly.
#'
#' @export
#' @examples
#' sex_fmt <- format_create("M" = "Male", "F" = "Female",
#'                          .missing = "Unknown", name = "sex")
#' cat(format_export(sex = sex_fmt))
format_export <- function(..., formats = NULL, file = NULL) {
  if (is.null(formats)) {
    formats <- list(...)
  }

  if (length(formats) == 0) {
    stop("At least one format object must be provided")
  }

  # Ensure all items are named
  fmt_names <- names(formats)
  if (is.null(fmt_names) || any(fmt_names == "")) {
    # Try to get names from the objects themselves
    for (i in seq_along(formats)) {
      if (is.null(fmt_names) || fmt_names[i] == "") {
        obj_name <- formats[[i]]$name
        if (!is.null(obj_name)) {
          fmt_names[i] <- obj_name
        } else {
          fmt_names[i] <- paste0("unnamed_", i)
        }
      }
    }
    names(formats) <- fmt_names
  }

  text_blocks <- character(0)

  for (nm in names(formats)) {
    obj <- formats[[nm]]
    if (inherits(obj, "ks_format")) {
      text_blocks <- c(text_blocks, .format_to_text(obj, nm))
    } else if (inherits(obj, "ks_invalue")) {
      text_blocks <- c(text_blocks, .invalue_to_text(obj, nm))
    } else {
      warning("Skipping '", nm, "': not a ks_format or ks_invalue object")
    }
  }

  output <- paste(text_blocks, collapse = "\n\n")

  if (!is.null(file)) {
    writeLines(output, file)
    message("Formats written to: ", file)
    return(invisible(file))
  }

  return(output)
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
      "^(VALUE|INVALUE)\\s+(\\w+)\\s*(?:\\(([^)]+)\\))?\\s*$",
      line, ignore.case = TRUE
    ))[[1]]

    if (length(block_match) >= 3) {
      if (in_block) {
        warning("Line ", i, ": New block started before previous block ended with ';'. ",
                "Closing previous block '", current_block$name, "'.")
        blocks <- c(blocks, list(current_block))
      }

      block_type <- toupper(block_match[2])
      block_name <- block_match[3]
      block_subtype <- if (length(block_match) >= 4 && block_match[4] != "") {
        trimws(block_match[4])
      } else {
        "auto"
      }

      current_block <- list(
        type = block_type,
        name = block_name,
        subtype = block_subtype,
        entries = list(),
        line_start = i
      )
      in_block <- TRUE
      next
    }

    # Check for block end
    if (grepl("^;", line)) {
      if (in_block && !is.null(current_block)) {
        blocks <- c(blocks, list(current_block))
        current_block <- NULL
        in_block <- FALSE
      }
      next
    }

    # Parse mapping line within a block
    if (in_block && !is.null(current_block)) {
      entry <- .parse_mapping_line(line, i)
      if (!is.null(entry)) {
        current_block$entries <- c(current_block$entries, list(entry))
      }
    }
  }

  # Handle unclosed block
  if (in_block && !is.null(current_block)) {
    warning("Block '", current_block$name, "' was not closed with ';'. Closing automatically.")
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
    warning("Line ", line_num, ": Could not parse mapping (no '=' found): ", line)
    return(NULL)
  }

  lhs <- trimws(substring(line, 1, eq_pos - 1))
  rhs <- trimws(substring(line, eq_pos + 1))

  # Unquote rhs
  rhs <- .unquote(rhs)

  # Check for .missing / .other directives
  if (lhs == ".missing") {
    return(list(type = "missing", value = rhs))
  }
  if (lhs == ".other") {
    return(list(type = "other", value = rhs))
  }

  # Check for interval notation: [low, high) or (low, high] etc.
  interval_match <- regmatches(lhs, regexec(
    "^(\\[|\\()\\s*(-?[0-9.]+|LOW|HIGH|Inf|-Inf)\\s*,\\s*(-?[0-9.]+|LOW|HIGH|Inf|-Inf)\\s*(\\]|\\))$",
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

  # Check for legacy range: low - high pattern (no brackets)
  range_match <- regmatches(lhs, regexec(
    "^(-?[0-9.]+|LOW|HIGH|Inf|-Inf)\\s*-\\s*(-?[0-9.]+|LOW|HIGH|Inf|-Inf)$",
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
  if (grepl("^[\"'].*[\"']$", s)) {
    return(substring(s, 2, nchar(s) - 1))
  }
  return(s)
}


#' Convert a parsed block to a ks_format or ks_invalue object
#' @keywords internal
.block_to_format <- function(block) {
  if (block$type == "VALUE") {
    return(.block_to_ks_format(block))
  } else if (block$type == "INVALUE") {
    return(.block_to_ks_invalue(block))
  } else {
    stop("Unknown block type: ", block$type)
  }
}


#' Convert VALUE block to ks_format
#' @keywords internal
.block_to_ks_format <- function(block) {
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
      # Store range as "low,high,inc_low,inc_high" key
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
      created = Sys.time()
    ),
    class = "ks_format"
  )

  # Auto-detect type if needed
  if (type == "auto") {
    has_ranges <- any(sapply(block$entries, function(e) e$type == "range"))
    if (has_ranges) {
      format_obj$type <- "numeric"
    } else {
      format_obj$type <- detect_format_type(names(mappings), mappings)
    }
  }

  return(format_obj)
}


#' Convert INVALUE block to ks_invalue
#' @keywords internal
.block_to_ks_invalue <- function(block) {
  mappings <- list()

  for (entry in block$entries) {
    if (entry$type == "discrete") {
      mappings[[entry$key]] <- entry$label
    } else if (entry$type == "missing") {
      # For invalue, .missing sets the missing_value
      # The value side is the target
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

  target_type <- block$subtype

  invalue_obj <- structure(
    list(
      name = block$name,
      target_type = target_type,
      mappings = mappings,
      missing_value = missing_value,
      created = Sys.time()
    ),
    class = "ks_invalue"
  )

  # Auto-detect type if needed
  if (target_type == "auto") {
    invalue_obj$target_type <- detect_invalue_type(mappings)
  }

  return(invalue_obj)
}


# ---------------------------------------------------------------------------
# Internal export helpers
# ---------------------------------------------------------------------------

#' Convert ks_format to SAS-like text
#' @keywords internal
.format_to_text <- function(fmt, name) {
  lines <- character(0)

  # Header
  type_part <- if (!is.null(fmt$type) && fmt$type != "auto") {
    paste0(" (", fmt$type, ")")
  } else {
    ""
  }
  lines <- c(lines, paste0("VALUE ", name, type_part))

  # Mappings
  for (i in seq_along(fmt$mappings)) {
    key <- names(fmt$mappings)[i]
    label <- fmt$mappings[[i]]

    # Try to parse as a range key
    parsed <- .parse_range_key(key)
    if (!is.null(parsed)) {
      left_bracket <- if (parsed$inc_low) "[" else "("
      right_bracket <- if (parsed$inc_high) "]" else ")"
      low <- .format_range_bound(parsed$low, is_low = TRUE)
      high <- .format_range_bound(parsed$high, is_low = FALSE)
      lines <- c(lines, paste0("  ", left_bracket, low, ", ", high,
                                right_bracket, " = \"", label, "\""))
    } else {
      lines <- c(lines, paste0("  \"", key, "\" = \"", label, "\""))
    }
  }

  # Special directives
  if (!is.null(fmt$missing_label)) {
    lines <- c(lines, paste0("  .missing = \"", fmt$missing_label, "\""))
  }
  if (!is.null(fmt$other_label)) {
    lines <- c(lines, paste0("  .other = \"", fmt$other_label, "\""))
  }

  lines <- c(lines, ";")
  return(paste(lines, collapse = "\n"))
}


#' Convert ks_invalue to SAS-like text
#' @keywords internal
.invalue_to_text <- function(inv, name) {
  lines <- character(0)

  # Header
  type_part <- if (!is.null(inv$target_type) && inv$target_type != "auto") {
    paste0(" (", inv$target_type, ")")
  } else {
    ""
  }
  lines <- c(lines, paste0("INVALUE ", name, type_part))

  # Mappings
  for (i in seq_along(inv$mappings)) {
    key <- names(inv$mappings)[i]
    value <- inv$mappings[[i]]
    lines <- c(lines, paste0("  \"", key, "\" = \"", value, "\""))
  }

  # Missing value
  if (!is.na(inv$missing_value)) {
    lines <- c(lines, paste0("  .missing = \"", inv$missing_value, "\""))
  }

  lines <- c(lines, ";")
  return(paste(lines, collapse = "\n"))
}


#' Format a numeric range bound for text output
#' @keywords internal
.format_range_bound <- function(val, is_low = TRUE) {
  if (is.infinite(val)) {
    if (val < 0) return("LOW")
    return("HIGH")
  }
  return(as.character(val))
}
