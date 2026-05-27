# ===========================================================================
# Format serialization: text export, CSV import, and CNTLOUT conversion.
#
# Public API: fexport(), fimport().
# Internal helpers: .format_to_text(), .datetime_format_to_text(),
# .stratified_format_to_text(), .invalue_to_text(), .format_range_bound(),
# .format_date_bound(), .cntlout_to_ks_format(), .cntlout_to_ks_invalue().
#
# Parsing (text -> objects) lives in format_parse.R.
# ===========================================================================

#' Export Formats to 'SAS'-like Text
#'
#' Converts \code{ks_format} and/or \code{ks_invalue} objects to
#' human-readable 'SAS'-like text representation.
#'
#' @param ... Named \code{ks_format} or \code{ks_invalue} objects to export.
#' @param formats A named list of format objects. Alternative to \code{...}.
#' @param file Optional file path to write the output to. If \code{NULL},
#'   returns the text as a character string.
#'
#' @return If \code{file} is \code{NULL}, returns a character string with the
#'   'SAS'-like text. If \code{file} is specified, writes to the file and returns
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


#' Import Formats from 'SAS' PROC FORMAT CNTLOUT CSV
#'
#' Reads a CSV file produced by 'SAS' \code{PROC FORMAT} with
#' \code{CNTLOUT=} option (typically exported via \code{PROC EXPORT})
#' and converts compatible format definitions into \code{ks_format} and
#' \code{ks_invalue} objects.
#'
#' @details
#' The 'SAS' format catalogue CSV is expected to contain the standard CNTLOUT
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
#' # In SAS:
#' # proc format library=work cntlout=fmts; run;
#' # proc export data=fmts outfile="formats.csv" dbms=csv replace; run;
#'
#' csv_file <- system.file("extdata", "test_cntlout.csv", package = "ksformat")
#' imported <- fimport(csv_file)
#' flist()
#' fprint()
#' fclear()
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
      range_table  = .build_range_table(mappings, type),
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
# Internal text-rendering helpers (object -> SAS-like text)
# ---------------------------------------------------------------------------

#' Convert ks_format to SAS-like text
#' @keywords internal
.format_to_text <- function(fmt, name) {
  if (fmt$type %in% c("date", "time", "datetime")) {
    return(.datetime_format_to_text(fmt, name))
  }
  if (.is_stratified_type(fmt$type)) {
    return(.stratified_format_to_text(fmt, name))
  }

  parts <- vector("list", length(fmt$mappings) + 4L)
  idx <- 1L

  type_str <- if (!is.null(fmt$type) && fmt$type != "auto") fmt$type else NULL
  ml_str <- if (isTRUE(fmt$multilabel)) "multilabel" else NULL
  nc_str <- if (isTRUE(fmt$ignore_case)) "nocase" else NULL
  df_str <- if (!is.null(fmt$date_format)) paste0("format: ", fmt$date_format) else NULL
  annot_parts <- c(type_str, ml_str, nc_str, df_str)
  type_part <- if (length(annot_parts) > 0L) {
    paste0(" (", paste(annot_parts, collapse = ", "), ")")
  } else {
    ""
  }
  parts[[idx]] <- paste0("VALUE ", name, type_part); idx <- idx + 1L

  is_vtype <- .is_value_type(fmt$type)
  is_date_rng <- .is_date_range_type(fmt$type)

  for (i in seq_along(fmt$mappings)) {
    key <- names(fmt$mappings)[i]
    label <- fmt$mappings[[i]]

    # Convert native values to strings for text output
    label_str <- if (is_vtype) {
      .typed_value_to_string(label, fmt$type, fmt$date_format)
    } else {
      as.character(label)
    }

    eval_suffix <- if (!is_vtype && .has_eval_attr(label)) " (eval)" else ""

    # Try to display range keys in interval notation
    parsed <- .parse_range_key_by_type(key, fmt$type, fmt$date_format)
    if (!is.null(parsed)) {
      left_bracket <- if (parsed$inc_low) "[" else "("
      right_bracket <- if (parsed$inc_high) "]" else ")"
      low <- if (is_vtype || is_date_rng) {
        .format_date_bound(parsed$low, fmt$date_format, fmt$type, is_low = TRUE)
      } else {
        .format_range_bound(parsed$low, is_low = TRUE)
      }
      high <- if (is_vtype || is_date_rng) {
        .format_date_bound(parsed$high, fmt$date_format, fmt$type, is_low = FALSE)
      } else {
        .format_range_bound(parsed$high, is_low = FALSE)
      }
      parts[[idx]] <- paste0("  ", left_bracket, low, ", ", high,
                             right_bracket, " = \"", label_str, "\"", eval_suffix)
    } else {
      parts[[idx]] <- paste0("  \"", key, "\" = \"", label_str, "\"", eval_suffix)
    }
    idx <- idx + 1L
  }

  if (!is.null(fmt$missing_label)) {
    miss_eval <- if (.has_eval_attr(fmt$missing_label)) " (eval)" else ""
    parts[[idx]] <- paste0("  .missing = \"", fmt$missing_label, "\"", miss_eval); idx <- idx + 1L
  }
  if (!is.null(fmt$other_label)) {
    other_eval <- if (.has_eval_attr(fmt$other_label)) " (eval)" else ""
    parts[[idx]] <- paste0("  .other = \"", fmt$other_label, "\"", other_eval); idx <- idx + 1L
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


#' Convert stratified_range ks_format to SAS-like text
#' @keywords internal
#' @noRd
.stratified_format_to_text <- function(fmt, name) {
  strata_sep <- if (!is.null(fmt$strata_sep)) fmt$strata_sep else "|"
  range_subtype <- if (!is.null(fmt$range_subtype)) fmt$range_subtype else "numeric"

  header_opts <- c("stratified_range",
                   paste0("range_subtype: ", range_subtype),
                   paste0("strata_sep: ", strata_sep))
  if (isTRUE(fmt$multilabel)) header_opts <- c(header_opts, "multilabel")
  if (isTRUE(fmt$ignore_case)) header_opts <- c(header_opts, "nocase")
  if (!is.null(fmt$date_format)) {
    header_opts <- c(header_opts, paste0("format: ", fmt$date_format))
  }
  header <- paste0("VALUE ", name, " (", paste(header_opts, collapse = ", "), ")")

  # Group mappings by stratum, first-seen order
  strata_order <- character(0)
  grouped <- list()
  parser <- function(k) .parse_range_key_by_type(k, range_subtype, fmt$date_format)
  for (i in seq_along(fmt$mappings)) {
    key <- names(fmt$mappings)[i]
    sp <- .split_stratified_key(key, strata_sep, range_subtype,
                                fmt$date_format)
    s <- if (is.null(sp)) "" else sp$stratum
    rk <- if (is.null(sp)) key else sp$range_key
    if (!s %in% strata_order) {
      strata_order <- c(strata_order, s)
      grouped[[s]] <- list()
    }
    grouped[[s]][[length(grouped[[s]]) + 1L]] <- list(
      range_key = rk, label = fmt$mappings[[i]]
    )
  }

  is_date_rng <- range_subtype %in% c("date", "datetime")
  inner_type <- .stratified_subtype_to_range_type(range_subtype)
  out <- character(0)
  out <- c(out, header)
  for (s in strata_order) {
    for (entry in grouped[[s]]) {
      parsed <- parser(entry$range_key)
      label_str <- as.character(entry$label)
      eval_suffix <- if (.has_eval_attr(entry$label)) " (eval)" else ""
      if (!is.null(parsed)) {
        lb <- if (parsed$inc_low) "[" else "("
        rb <- if (parsed$inc_high) "]" else ")"
        low <- if (is_date_rng) {
          .format_date_bound(parsed$low, fmt$date_format, inner_type, is_low = TRUE)
        } else {
          .format_range_bound(parsed$low, is_low = TRUE)
        }
        high <- if (is_date_rng) {
          .format_date_bound(parsed$high, fmt$date_format, inner_type, is_low = FALSE)
        } else {
          .format_range_bound(parsed$high, is_low = FALSE)
        }
        out <- c(out, paste0("  \"", s, "\"", strata_sep, lb, low, ", ",
                             high, rb, " = \"", label_str, "\"", eval_suffix))
      } else {
        out <- c(out, paste0("  \"", s, strata_sep, entry$range_key,
                             "\" = \"", label_str, "\"", eval_suffix))
      }
    }
    if (!is.null(fmt$missing_by_stratum) && s %in% names(fmt$missing_by_stratum)) {
      out <- c(out, paste0("  \".missing", strata_sep, s, "\" = \"",
                           fmt$missing_by_stratum[[s]], "\""))
    }
    if (!is.null(fmt$other_by_stratum) && s %in% names(fmt$other_by_stratum)) {
      out <- c(out, paste0("  \".other", strata_sep, s, "\" = \"",
                           fmt$other_by_stratum[[s]], "\""))
    }
  }
  if (!is.null(fmt$missing_label)) {
    out <- c(out, paste0("  .missing = \"", fmt$missing_label, "\""))
  }
  if (!is.null(fmt$other_label)) {
    out <- c(out, paste0("  .other = \"", fmt$other_label, "\""))
  }
  out <- c(out, ";")
  paste(out, collapse = "\n")
}


#' Convert ks_invalue to SAS-like text
#' @keywords internal
.invalue_to_text <- function(inv, name) {
  parts <- vector("list", length(inv$mappings) + 3L)
  idx <- 1L

  type_opts <- c(
    if (!is.null(inv$target_type) && inv$target_type != "numeric") inv$target_type else NULL,
    if (isTRUE(inv$ignore_case)) "nocase" else NULL
  )
  type_part <- if (length(type_opts)) {
    paste0(" (", paste(type_opts, collapse = ", "), ")")
  } else {
    ""
  }
  parts[[idx]] <- paste0("INVALUE ", name, type_part); idx <- idx + 1L

  for (i in seq_along(inv$mappings)) {
    key <- names(inv$mappings)[i]
    value <- inv$mappings[[i]]
    eval_suffix <- if (.has_eval_attr(value)) " (eval)" else ""
    parts[[idx]] <- paste0("  \"", key, "\" = \"", value, "\"", eval_suffix); idx <- idx + 1L
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

#' Format a Date / POSIXct range bound for text output
#'
#' Renders infinite bounds as \code{LOW}/\code{HIGH}; otherwise formats the
#' Date/POSIXct value using \code{date_format} when supplied, else ISO 8601.
#' Used for value-type (Date/POSIXct) formats and the date_range /
#' datetime_range types.
#' @keywords internal
.format_date_bound <- function(val, date_format = NULL, type = NULL,
                               is_low = TRUE) {
  num <- as.numeric(val)
  if (is.na(num)) return("")
  if (is.infinite(num)) return(if (num < 0) "LOW" else "HIGH")
  if (!is.null(date_format)) return(format(val, date_format))
  as.character(val)
}
