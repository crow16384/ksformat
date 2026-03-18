#' Parse Format Definitions from SAS-like Text
#'
#' Reads format definitions written in a human-friendly SAS-like syntax
#' and returns a list of \code{ks_format} and/or \code{ks_invalue} objects.
#' All parsed formats are automatically stored in the global format library.
#'
#' @param text Character string or character vector containing format definitions.
#'   If a character vector, lines are concatenated with newlines.
#' @param file Path to a text file containing format definitions.
#'   Exactly one of \code{text} or \code{file} must be provided.
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
#'   \item Values can be quoted or unquoted
#'   \item Ranges use interval notation with explicit bounds
#'   \item Legacy range syntax \code{low - high} is also supported
#'   \item Special range keywords: \code{LOW} (-Inf) and \code{HIGH} (Inf)
#'   \item \code{.missing} and \code{.other} are special directives
#'   \item Lines starting with \code{/*}, \code{*}, \code{//}, or \code{#} are comments
#' }
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
fparse <- function(text = NULL, file = NULL) {
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

  result
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
#' # Export a character format
#' sex_fmt <- fnew("M" = "Male", "F" = "Female",
#'                 .missing = "Unknown", name = "sex")
#' cat(fexport(sex = sex_fmt))
#'
#' # Export a numeric range format
#' fparse(text = '
#' VALUE bmi (numeric)
#'   [0, 18.5)  = "Underweight"
#'   [18.5, 25) = "Normal"
#'   [25, 30)   = "Overweight"
#'   [30, HIGH] = "Obese"
#'   .missing   = "No data"
#' ;
#' ')
#' bmi_fmt <- format_get("bmi")
#' cat(fexport(bmi = bmi_fmt))
#'
#' # Export a multilabel format
#' risk_fmt <- fnew(
#'   "0,3,TRUE,TRUE" = "Low Risk",
#'   "0,7,TRUE,TRUE" = "Monitored",
#'   "3,7,FALSE,TRUE" = "Medium Risk",
#'   "7,10,FALSE,TRUE" = "High Risk",
#'   name = "risk", type = "numeric", multilabel = TRUE
#' )
#' cat(fexport(risk = risk_fmt))
#'
#' # Export a date format
#' enrl_fmt <- fnew_date("DATE9.", name = "enrldt", .missing = "Not Enrolled")
#' cat(fexport(enrldt = enrl_fmt))
#' fclear()
fexport <- function(..., formats = NULL, file = NULL) {
  if (is.null(formats)) {
    formats <- list(...)
  }

  if (length(formats) == 0) {
    cli_abort("At least one format object must be provided.")
  }

  # Ensure all items are named
  fmt_names <- names(formats)
  if (is.null(fmt_names)) {
    fmt_names <- rep("", length(formats))
  }
  if (any(fmt_names == "")) {
    for (i in seq_along(formats)) {
      if (fmt_names[i] == "") {
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
      cli_warn("Skipping {.val {nm}}: not a {.cls ks_format} or {.cls ks_invalue} object.")
    }
  }

  output <- paste(text_blocks, collapse = "\n\n")

  if (!is.null(file)) {
    writeLines(output, file)
    cli_inform("Formats written to: {.file {file}}")
    return(invisible(file))
  }

  return(output)
}


#' Import Formats from SAS PROC FORMAT CNTLOUT CSV
#'
#' Reads a CSV file produced by the SAS \code{PROC FORMAT} with
#' \code{CNTLOUT=} option (typically exported via \code{PROC EXPORT})
#' and converts compatible format definitions into \code{ks_format} and
#' \code{ks_invalue} objects.
#'
#' @details
#' The SAS format catalogue CSV is expected to contain the standard CNTLOUT
#' columns: \code{FMTNAME}, \code{START}, \code{END}, \code{LABEL},
#' \code{TYPE}, \code{HLO}, \code{SEXCL}, \code{EEXCL}.
#'
#' \strong{Supported SAS format types:}
#' \describe{
#'   \item{\code{N}}{Numeric VALUE format \eqn{\to} \code{ks_format} with
#'     \code{type = "numeric"}}
#'   \item{\code{C}}{Character VALUE format \eqn{\to} \code{ks_format} with
#'     \code{type = "character"}}
#'   \item{\code{I}}{Numeric INVALUE (informat) \eqn{\to} \code{ks_invalue}
#'     with \code{target_type = "numeric"}}
#'   \item{\code{J}}{Character INVALUE (informat) \eqn{\to} \code{ks_invalue}
#'     with \code{target_type = "character"}}
#' }
#'
#' \strong{Incompatible types (logged with a warning):}
#' \describe{
#'   \item{\code{P}}{PICTURE formats \eqn{-} no equivalent in ksformat}
#' }
#'
#' Rows with SAS special missing values (\code{.A}\eqn{-}\code{.Z},
#' \code{._}) in the HLO field are logged as incompatible entries and skipped
#' because R has no equivalent concept.
#'
#' @param file Path to the CSV file exported from a SAS format catalogue.
#' @param register Logical; if \code{TRUE} (default), each imported format is
#'   registered in the global format library.
#' @param overwrite Logical; if \code{TRUE} (default), existing library entries
#'   with the same name are overwritten.
#'
#' @return A named list of \code{ks_format} and \code{ks_invalue} objects that
#'   were successfully imported. Returned invisibly.
#'
#' @export
#' @examples
#' \dontrun{
#' # In SAS:
#' # proc format library=work cntlout=fmts; run;
#' # proc export data=fmts outfile="formats.csv" dbms=csv replace; run;
#'
#' # In R:
#' imported <- fimport("formats.csv")
#' fprint()
#' }
fimport <- function(file, register = TRUE, overwrite = TRUE) {
  if (!file.exists(file)) {
    cli_abort("File not found: {.file {file}}")
  }

  data <- utils::read.csv(file, stringsAsFactors = FALSE, na.strings = "")

  # Normalize column names to uppercase
  names(data) <- toupper(names(data))

  # Validate required columns
  required <- c("FMTNAME", "START", "END", "LABEL", "TYPE")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    cli_abort(c(
      "Required column{?s} missing from CSV: {.val {missing_cols}}",
      "i" = "Expected a SAS {.code PROC FORMAT CNTLOUT=} export."
    ))
  }

  # Supply defaults for optional columns
  if (!"HLO"   %in% names(data)) data$HLO   <- ""
  if (!"SEXCL" %in% names(data)) data$SEXCL <- "N"
  if (!"EEXCL" %in% names(data)) data$EEXCL <- "N"

  # Ensure character types for key fields
  data$FMTNAME <- trimws(as.character(data$FMTNAME))
  data$START   <- as.character(ifelse(is.na(data$START), "", data$START))
  data$END     <- as.character(ifelse(is.na(data$END),   "", data$END))
  data$LABEL   <- as.character(ifelse(is.na(data$LABEL), "", data$LABEL))
  data$TYPE    <- trimws(toupper(as.character(data$TYPE)))
  data$HLO     <- toupper(as.character(ifelse(is.na(data$HLO), "", data$HLO)))
  data$SEXCL   <- toupper(as.character(ifelse(is.na(data$SEXCL), "N", data$SEXCL)))
  data$EEXCL   <- toupper(as.character(ifelse(is.na(data$EEXCL), "N", data$EEXCL)))

  # ---- Classify format types ----
  supported_types <- c("N", "C", "I", "J")
  all_types <- unique(data$TYPE)
  unsupported <- setdiff(all_types, supported_types)

  if (length(unsupported) > 0) {
    # Identify formats with unsupported types
    for (utype in unsupported) {
      affected <- unique(data$FMTNAME[data$TYPE == utype])
      type_desc <- switch(utype,
        "P" = "PICTURE",
        paste0("unknown (", utype, ")")
      )
      cli_warn(c(
        "Skipping {type_desc} format{?s}: {.val {affected}}",
        "i" = "TYPE={.val {utype}} is not supported by ksformat."
      ))
    }
    data <- data[data$TYPE %in% supported_types, , drop = FALSE]
  }

  if (nrow(data) == 0) {
    cli_inform("No compatible formats found in {.file {file}}.")
    return(invisible(list()))
  }

  # ---- Group by FMTNAME + TYPE and build format objects ----
  format_key <- paste(data$FMTNAME, data$TYPE, sep = "|")
  groups <- split(seq_len(nrow(data)), format_key)

  result <- list()
  skipped_entries <- list()

  for (gname in names(groups)) {
    idx <- groups[[gname]]
    rows <- data[idx, , drop = FALSE]
    fmt_name   <- rows$FMTNAME[1]
    fmt_type   <- rows$TYPE[1]

    if (fmt_type %in% c("N", "C")) {
      obj <- .cntlout_to_ks_format(rows, fmt_name, fmt_type, skipped_entries)
      skipped_entries <- obj$skipped
      obj <- obj$format
    } else {
      obj <- .cntlout_to_ks_invalue(rows, fmt_name, fmt_type, skipped_entries)
      skipped_entries <- obj$skipped
      obj <- obj$invalue
    }

    if (is.null(obj)) next

    # Validate
    tryCatch(
      .format_validate(obj),
      error = function(e) {
        cli_warn(c(
          "Format {.val {fmt_name}} failed validation and was skipped.",
          "x" = conditionMessage(e)
        ))
        obj <<- NULL
      }
    )
    if (is.null(obj)) next

    # Register
    if (register) {
      .format_register(obj, name = fmt_name, overwrite = overwrite)
    }

    result[[fmt_name]] <- obj
  }

  # ---- Report skipped entries ----
  if (length(skipped_entries) > 0) {
    for (entry in skipped_entries) {
      cli_warn(c(
        "Skipped incompatible entry in format {.val {entry$format}}:",
        "x" = entry$reason
      ))
    }
  }

  n_fmt <- sum(vapply(result, inherits, logical(1), "ks_format"))
  n_inv <- sum(vapply(result, inherits, logical(1), "ks_invalue"))
  cli_inform(c(
    "v" = "Imported {n_fmt} format{?s} and {n_inv} invalue{?s} from {.file {file}}."
  ))

  return(invisible(result))
}


#' Convert CNTLOUT rows to ks_format
#' @keywords internal
.cntlout_to_ks_format <- function(rows, fmt_name, fmt_type, skipped) {
  type <- if (fmt_type == "N") "numeric" else "character"
  mappings <- list()
  missing_label <- NULL
  other_label   <- NULL
  multilabel    <- FALSE

  for (i in seq_len(nrow(rows))) {
    hlo   <- rows$HLO[i]
    start <- rows$START[i]
    end   <- rows$END[i]
    label <- rows$LABEL[i]
    sexcl <- rows$SEXCL[i]
    eexcl <- rows$EEXCL[i]

    # Check for multilabel flag
    if (grepl("T", hlo, fixed = TRUE)) {
      multilabel <- TRUE
    }

    # OTHER entry
    if (grepl("O", hlo, fixed = TRUE)) {
      other_label <- label
      next
    }

    # Special missing values (.A-.Z, ._) — no R equivalent
    if (grepl("S", hlo, fixed = TRUE)) {
      skipped <- c(skipped, list(list(
        format = fmt_name,
        reason = paste0(
          "SAS special missing value '", start,
          "' (HLO='", hlo, "') has no R equivalent."
        )
      )))
      next
    }

    # Standard missing (numeric: START is ".", character: blank)
    if (type == "numeric" && start == ".") {
      missing_label <- label
      next
    }
    if (type == "character" && start == "" && end == "" &&
        !grepl("[LH]", hlo)) {
      missing_label <- label
      next
    }

    # ---- Range vs discrete ----
    has_low  <- grepl("L", hlo, fixed = TRUE)
    has_high <- grepl("H", hlo, fixed = TRUE)

    is_range <- FALSE
    if (has_low || has_high) {
      is_range <- TRUE
    } else if (type == "numeric" && start != end) {
      is_range <- TRUE
    }

    if (is_range && type == "numeric") {
      low  <- if (has_low)  -Inf else suppressWarnings(as.numeric(start))
      high <- if (has_high)  Inf else suppressWarnings(as.numeric(end))

      if (is.na(low) || is.na(high)) {
        skipped <- c(skipped, list(list(
          format = fmt_name,
          reason = paste0(
            "Could not parse range bounds: START='", start,
            "', END='", end, "'."
          )
        )))
        next
      }

      inc_low  <- (sexcl != "Y")
      inc_high <- (eexcl != "Y")

      range_key <- paste0(low, ",", high, ",", toupper(inc_low), ",",
                          toupper(inc_high))
      mappings[[range_key]] <- label
    } else {
      mappings[[trimws(start)]] <- label
    }
  }

  if (length(mappings) == 0 && is.null(missing_label) && is.null(other_label)) {
    skipped <- c(skipped, list(list(
      format = fmt_name,
      reason = "No valid mappings could be extracted."
    )))
    return(list(format = NULL, skipped = skipped))
  }

  format_obj <- structure(
    list(
      name         = fmt_name,
      type         = type,
      mappings     = mappings,
      missing_label = missing_label,
      other_label  = other_label,
      multilabel   = multilabel,
      ignore_case  = FALSE,
      created      = Sys.time()
    ),
    class = "ks_format"
  )

  list(format = format_obj, skipped = skipped)
}


#' Convert CNTLOUT rows to ks_invalue
#' @keywords internal
.cntlout_to_ks_invalue <- function(rows, fmt_name, fmt_type, skipped) {
  target_type   <- if (fmt_type == "I") "numeric" else "character"
  mappings      <- list()
  missing_value <- NA

  for (i in seq_len(nrow(rows))) {
    hlo   <- rows$HLO[i]
    start <- rows$START[i]
    label <- rows$LABEL[i]

    # OTHER — ks_invalue doesn't support .other, skip
    if (grepl("O", hlo, fixed = TRUE)) {
      skipped <- c(skipped, list(list(
        format = fmt_name,
        reason = "OTHER directive in INVALUE is not supported; entry skipped."
      )))
      next
    }

    # Special missing
    if (grepl("S", hlo, fixed = TRUE)) {
      skipped <- c(skipped, list(list(
        format = fmt_name,
        reason = paste0(
          "SAS special missing value '", start,
          "' in INVALUE has no R equivalent."
        )
      )))
      next
    }

    # Range entries — ks_invalue doesn't support ranges
    has_low  <- grepl("L", hlo, fixed = TRUE)
    has_high <- grepl("H", hlo, fixed = TRUE)
    if (has_low || has_high || (start != rows$END[i] && start != "")) {
      skipped <- c(skipped, list(list(
        format = fmt_name,
        reason = paste0(
          "Range entry '", start, "'-'", rows$END[i],
          "' in INVALUE is not supported."
        )
      )))
      next
    }

    # Standard missing value
    if (start == "." || start == "") {
      # Convert label to the appropriate target type
      if (target_type == "numeric") {
        missing_value <- suppressWarnings(as.numeric(label))
      } else {
        missing_value <- label
      }
      next
    }

    # Discrete mapping: key = input text, value = output value
    val <- if (target_type == "numeric") {
      suppressWarnings(as.numeric(label))
    } else {
      label
    }

    if (target_type == "numeric" && is.na(val) && label != "") {
      skipped <- c(skipped, list(list(
        format = fmt_name,
        reason = paste0(
          "INVALUE label '", label, "' for key '", start,
          "' could not be converted to numeric."
        )
      )))
      next
    }

    mappings[[start]] <- val
  }

  if (length(mappings) == 0 && is.na(missing_value)) {
    skipped <- c(skipped, list(list(
      format = fmt_name,
      reason = "No valid INVALUE mappings could be extracted."
    )))
    return(list(invalue = NULL, skipped = skipped))
  }

  invalue_obj <- structure(
    list(
      name          = fmt_name,
      target_type   = target_type,
      mappings      = mappings,
      missing_value = missing_value,
      created       = Sys.time()
    ),
    class = "ks_invalue"
  )

  list(invalue = invalue_obj, skipped = skipped)
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

      current_block <- list(
        type = block_type,
        name = block_name,
        subtype = block_subtype,
        multilabel = block_multilabel,
        nocase = block_nocase,
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
  # Handle date/time/datetime blocks
  if (tolower(block$subtype) %in% c("date", "time", "datetime")) {
    return(.block_to_ks_datetime_format(block))
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
      created = Sys.time()
    ),
    class = "ks_format"
  )

  # Auto-detect type if needed
  if (type == "auto") {
    has_ranges <- any(vapply(block$entries, function(e) identical(e$type, "range"), logical(1L)))
    if (has_ranges) {
      format_obj$type <- "numeric"
    } else {
      format_obj$type <- detect_format_type(names(mappings))
    }
  }

  return(format_obj)
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
      created = Sys.time()
    ),
    class = "ks_invalue"
  )

  return(invalue_obj)
}


# ---------------------------------------------------------------------------
# Internal export helpers
# ---------------------------------------------------------------------------

#' Convert ks_format to SAS-like text
#' @keywords internal
.format_to_text <- function(fmt, name) {
  if (fmt$type %in% c("date", "time", "datetime")) {
    return(.datetime_format_to_text(fmt, name))
  }

  parts <- vector("list", length(fmt$mappings) + 4L)
  idx <- 1L

  type_str <- if (!is.null(fmt$type) && fmt$type != "auto") fmt$type else NULL
  ml_str <- if (isTRUE(fmt$multilabel)) "multilabel" else NULL
  nc_str <- if (isTRUE(fmt$ignore_case)) "nocase" else NULL
  annot_parts <- c(type_str, ml_str, nc_str)
  type_part <- if (length(annot_parts) > 0L) {
    paste0(" (", paste(annot_parts, collapse = ", "), ")")
  } else {
    ""
  }
  parts[[idx]] <- paste0("VALUE ", name, type_part); idx <- idx + 1L

  for (i in seq_along(fmt$mappings)) {
    key <- names(fmt$mappings)[i]
    label <- fmt$mappings[[i]]
    parsed <- .parse_range_key(key)
    if (!is.null(parsed)) {
      left_bracket <- if (parsed$inc_low) "[" else "("
      right_bracket <- if (parsed$inc_high) "]" else ")"
      low <- .format_range_bound(parsed$low, is_low = TRUE)
      high <- .format_range_bound(parsed$high, is_low = FALSE)
      parts[[idx]] <- paste0("  ", left_bracket, low, ", ", high,
                             right_bracket, " = \"", label, "\"")
    } else {
      parts[[idx]] <- paste0("  \"", key, "\" = \"", label, "\"")
    }
    idx <- idx + 1L
  }

  if (!is.null(fmt$missing_label)) {
    parts[[idx]] <- paste0("  .missing = \"", fmt$missing_label, "\""); idx <- idx + 1L
  }
  if (!is.null(fmt$other_label)) {
    parts[[idx]] <- paste0("  .other = \"", fmt$other_label, "\""); idx <- idx + 1L
  }
  parts[[idx]] <- ";"
  paste(unlist(parts[seq_len(idx)]), collapse = "\n")
}


#' Convert datetime ks_format to SAS-like text
#' @keywords internal
.datetime_format_to_text <- function(fmt, name) {
  parts <- vector("list", 4L)
  idx <- 1L

  parts[[idx]] <- paste0("VALUE ", name, " (", fmt$type, ")"); idx <- idx + 1L

  pattern_str <- if (!is.null(fmt$sas_name)) {
    paste0(fmt$sas_name, ".")
  } else {
    fmt$dt_pattern
  }
  parts[[idx]] <- paste0("  pattern = \"", pattern_str, "\""); idx <- idx + 1L

  if (!is.null(fmt$missing_label)) {
    parts[[idx]] <- paste0("  .missing = \"", fmt$missing_label, "\""); idx <- idx + 1L
  }
  parts[[idx]] <- ";"
  paste(unlist(parts[seq_len(idx)]), collapse = "\n")
}


#' Convert ks_invalue to SAS-like text
#' @keywords internal
.invalue_to_text <- function(inv, name) {
  parts <- vector("list", length(inv$mappings) + 3L)
  idx <- 1L

  type_part <- if (!is.null(inv$target_type) && inv$target_type != "numeric") {
    paste0(" (", inv$target_type, ")")
  } else {
    ""
  }
  parts[[idx]] <- paste0("INVALUE ", name, type_part); idx <- idx + 1L

  for (i in seq_along(inv$mappings)) {
    key <- names(inv$mappings)[i]
    value <- inv$mappings[[i]]
    parts[[idx]] <- paste0("  \"", key, "\" = \"", value, "\""); idx <- idx + 1L
  }

  if (!is.null(inv$missing_value) && !identical(inv$missing_value, NA)) {
    parts[[idx]] <- paste0("  .missing = \"", inv$missing_value, "\""); idx <- idx + 1L
  }
  parts[[idx]] <- ";"
  paste(unlist(parts[seq_len(idx)]), collapse = "\n")
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
