#' Date/Time Format Support (SAS-style)
#'
#' Provides SAS-compatible date, time, and datetime formatting capabilities.
#' Built-in SAS format names (DATE9., MMDDYY10., TIME8., DATETIME20., etc.)
#' are auto-resolved and can be used directly with \code{\link{fput}},
#' \code{\link{fputn}}, and \code{\link{fputc}}.
#'
#' @name format_datetime
#' @keywords internal
NULL


# ===========================================================================
# SAS datetime format definitions
# ===========================================================================

#' @keywords internal
.sas_datetime_formats <- list(
  # ---- Date formats ----
  "DATE5"    = list(r_fmt = "%d%b",      type = "date", toupper = TRUE),
  "DATE7"    = list(r_fmt = "%d%b%y",    type = "date", toupper = TRUE),
  "DATE9"    = list(r_fmt = "%d%b%Y",    type = "date", toupper = TRUE),
  "DATE11"   = list(r_fmt = "%d-%b-%Y",  type = "date", toupper = TRUE),

  # DD/MM/YY
  "DDMMYY2"  = list(r_fmt = "%d",        type = "date", toupper = FALSE),
  "DDMMYY4"  = list(r_fmt = "%d%m",      type = "date", toupper = FALSE),
  "DDMMYY6"  = list(r_fmt = "%d%m%y",    type = "date", toupper = FALSE),
  "DDMMYY8"  = list(r_fmt = "%d/%m/%y",  type = "date", toupper = FALSE),
  "DDMMYY10" = list(r_fmt = "%d/%m/%Y",  type = "date", toupper = FALSE),

  # MM/DD/YY
  "MMDDYY2"  = list(r_fmt = "%m",        type = "date", toupper = FALSE),
  "MMDDYY4"  = list(r_fmt = "%m%d",      type = "date", toupper = FALSE),
  "MMDDYY6"  = list(r_fmt = "%m%d%y",    type = "date", toupper = FALSE),
  "MMDDYY8"  = list(r_fmt = "%m/%d/%y",  type = "date", toupper = FALSE),
  "MMDDYY10" = list(r_fmt = "%m/%d/%Y",  type = "date", toupper = FALSE),

  # YY-MM-DD
  "YYMMDD2"  = list(r_fmt = "%y",        type = "date", toupper = FALSE),
  "YYMMDD4"  = list(r_fmt = "%y%m",      type = "date", toupper = FALSE),
  "YYMMDD6"  = list(r_fmt = "%y%m%d",    type = "date", toupper = FALSE),
  "YYMMDD8"  = list(r_fmt = "%y-%m-%d",  type = "date", toupper = FALSE),
  "YYMMDD10" = list(r_fmt = "%Y-%m-%d",  type = "date", toupper = FALSE),

  # Month/Year
  "MONYY5"   = list(r_fmt = "%b%y",      type = "date", toupper = TRUE),
  "MONYY7"   = list(r_fmt = "%b%Y",      type = "date", toupper = TRUE),

  # Single-component date formats
  "YEAR4"    = list(r_fmt = "%Y",         type = "date", toupper = FALSE),
  "WEEKDATE" = list(r_fmt = "%A, %B %d, %Y", type = "date", toupper = FALSE),
  "WORDDATE" = list(r_fmt = "%B %d, %Y",     type = "date", toupper = FALSE),
  "DOWNAME"  = list(r_fmt = "%A",         type = "date", toupper = FALSE),
  "MONNAME"  = list(r_fmt = "%B",         type = "date", toupper = FALSE),
  "DAY"      = list(r_fmt = "%d",         type = "date", toupper = FALSE),
  "MONTH"    = list(r_fmt = "%m",         type = "date", toupper = FALSE),
  "JULDAY"   = list(r_fmt = "%j",         type = "date", toupper = FALSE),
  "JULIAN"   = list(r_fmt = "%j",         type = "date", toupper = FALSE),
  "QTR"      = list(r_fmt = "quarter",    type = "date", toupper = FALSE),

  # ---- Time formats (no leading zero for hours, like SAS) ----
  "TIME5"    = list(r_fmt = "%_H:%M",      type = "time", toupper = FALSE, no_lz_hour = TRUE),
  "TIME8"    = list(r_fmt = "%_H:%M:%S",   type = "time", toupper = FALSE, no_lz_hour = TRUE),
  "TIME11"   = list(r_fmt = "%_H:%M:%OS",  type = "time", toupper = FALSE, no_lz_hour = TRUE),
  "HHMM"     = list(r_fmt = "%H:%M",       type = "time", toupper = FALSE),
  "HOUR"     = list(r_fmt = "%_H",          type = "time", toupper = FALSE, no_lz_hour = TRUE),
  "MMSS"     = list(r_fmt = "%M:%S",       type = "time", toupper = FALSE),
  # ---- TOD formats (leading zero for hours, like SAS) ----
  "TOD5"     = list(r_fmt = "%H:%M",       type = "time", toupper = FALSE),
  "TOD8"     = list(r_fmt = "%H:%M:%S",    type = "time", toupper = FALSE),
  "TOD11"    = list(r_fmt = "%H:%M:%OS",   type = "time", toupper = FALSE),

  # ---- Datetime formats ----
  "DATETIME13" = list(r_fmt = "%d%b%y:%H:%M",     type = "datetime", toupper = TRUE),
  "DATETIME16" = list(r_fmt = "%d%b%y:%H:%M:%S",  type = "datetime", toupper = TRUE),
  "DATETIME18" = list(r_fmt = "%d%b%Y:%H:%M",     type = "datetime", toupper = TRUE),
  "DATETIME20" = list(r_fmt = "%d%b%Y:%H:%M:%S",  type = "datetime", toupper = TRUE),
  "DATETIME22" = list(r_fmt = "%d%b%Y:%H:%M:%OS", type = "datetime", toupper = TRUE),
  "DTDATE"     = list(r_fmt = "%d%b%Y",            type = "datetime", toupper = TRUE),
  "DTMONYY"    = list(r_fmt = "%b%Y",              type = "datetime", toupper = TRUE),
  "DTYEAR"     = list(r_fmt = "%Y",                type = "datetime", toupper = FALSE),
  "DTYYMMDD"   = list(r_fmt = "%Y-%m-%d",          type = "datetime", toupper = FALSE)
)


#' Default widths for SAS format names without explicit width
#' @keywords internal
.sas_format_defaults <- list(
  "DATE"     = "DATE9",
  "DDMMYY"   = "DDMMYY10",
  "MMDDYY"   = "MMDDYY10",
  "YYMMDD"   = "YYMMDD10",
  "MONYY"    = "MONYY7",
  "DATETIME" = "DATETIME20",
  "TIME"     = "TIME8",
  "HHMM"     = "HHMM",
  "HOUR"     = "HOUR",
  "MMSS"     = "MMSS",
  "TOD"      = "TOD8",
  "DTDATE"   = "DTDATE",
  "DTMONYY"  = "DTMONYY",
  "DTYEAR"   = "DTYEAR",
  "DTYYMMDD" = "DTYYMMDD"
)


# ===========================================================================
# Internal helpers
# ===========================================================================

#' Normalize a SAS format name (strip trailing period, uppercase)
#' @keywords internal
.normalize_sas_format_name <- function(name) {
  name <- sub("\\.$", "", name)
  toupper(name)
}


#' Resolve a SAS format definition from the built-in table
#'
#' @param name Character. SAS format name (e.g., "DATE9.", "MMDDYY10", "TIME8.")
#' @return A list with \code{r_fmt}, \code{type}, \code{toupper}, or \code{NULL}.
#' @keywords internal
.resolve_sas_format_def <- function(name) {
  norm <- .normalize_sas_format_name(name)

  # Direct match
  if (norm %in% names(.sas_datetime_formats)) {
    return(.sas_datetime_formats[[norm]])
  }

  # Try default width
  if (norm %in% names(.sas_format_defaults)) {
    default_name <- .sas_format_defaults[[norm]]
    if (default_name %in% names(.sas_datetime_formats)) {
      return(.sas_datetime_formats[[default_name]])
    }
  }

  # Dynamic resolution for TIME/TOD families (any width)
  time_match <- regmatches(norm, regexec("^(TIME|TOD)(\\d+)$", norm))[[1]]
  if (length(time_match) == 3L) {
    family <- time_match[2]
    width  <- as.integer(time_match[3])
    # TIME: no leading zero (%-H); TOD: leading zero (%H)
    h_code <- if (family == "TIME") "%_H" else "%H"
    if (width < 5L) {
      r_fmt <- h_code
    } else if (width < 8L) {
      r_fmt <- paste0(h_code, ":%M")
    } else if (width < 11L) {
      r_fmt <- paste0(h_code, ":%M:%S")
    } else {
      r_fmt <- paste0(h_code, ":%M:%OS")
    }
    return(list(r_fmt = r_fmt, type = "time", toupper = FALSE))
  }

  return(NULL)
}


#' Check if a name corresponds to a built-in SAS datetime format
#'
#' @param name Character. Format name to check.
#' @return Logical.
#' @keywords internal
.is_sas_datetime_format <- function(name) {
  !is.null(.resolve_sas_format_def(name))
}


# ===========================================================================
# Date/time format creation
# ===========================================================================

#' Create Date/Time Format
#'
#' Creates a format object for date, time, or datetime values using SAS format
#' names or custom R \code{strftime} patterns. The format is automatically
#' registered in the global format library.
#'
#' @param pattern Character. Either a SAS format name (e.g., \code{"DATE9."},
#'   \code{"MMDDYY10."}, \code{"TIME8."}, \code{"DATETIME20."}) or a custom R
#'   \code{strftime} pattern (e.g., \code{"\%Y-\%m-\%d"}).
#' @param name Character. Name to register the format under. Defaults to
#'   the SAS format name (with period) or the pattern itself.
#' @param type Character. Type of format: \code{"date"}, \code{"time"},
#'   \code{"datetime"}, or \code{"auto"} (auto-detect from SAS name).
#'   Must be specified for custom strftime patterns.
#' @param .missing Character. Label for missing values (NA). Default \code{NULL}.
#'
#' @return A \code{ks_format} object with date/time type, registered in the library.
#'
#' @details
#' \strong{SAS format names} are resolved automatically:
#' \itemize{
#'   \item \strong{Date:} DATE9., DDMMYY10., MMDDYY10., YYMMDD10., MONYY7.,
#'         YEAR4., WEEKDATE., WORDDATE., etc.
#'   \item \strong{Time:} TIME8., TIME5., HHMM., HOUR., MMSS.
#'   \item \strong{Datetime:} DATETIME20., DATETIME13., etc.
#' }
#'
#' \strong{Numeric input} is converted using R epoch (\code{"1970-01-01"}):
#' \itemize{
#'   \item Dates: numeric values are interpreted as days since 1970-01-01
#'   \item Datetimes: numeric values are interpreted as seconds since 1970-01-01
#'   \item Times: always treated as seconds since midnight
#' }
#'
#' @export
#'
#' @examples
#' # Use a SAS format name
#' fnew_date("DATE9.", name = "mydate")
#' fput(as.Date("2020-01-01"), "mydate")
#' # [1] "01JAN2020"
#'
#' # Use directly without pre-creating
#' fputn(as.Date("2020-06-15"), "MMDDYY10.")
#' # [1] "06/15/2020"
#'
#' # Custom strftime pattern (e.g., Russian style: DD.MM.YYYY)
#' fnew_date("%d.%m.%Y", name = "ru_date", type = "date")
#' fput(as.Date(c("1990-03-25", "1985-11-03", "2000-07-14")), "ru_date")
#'
#' # Custom format with missing value label
#' fnew_date("MMDDYY10.", name = "us_date", .missing = "NO DATE")
#' fput(c(as.Date("2025-01-01"), NA, as.Date("2025-12-31")), "us_date")
#' # [1] "01/01/2025" "NO DATE" "12/31/2025"
#'
#' # Numeric dates (days since 1970-01-01, R epoch)
#' r_days <- as.numeric(as.Date("2025-01-01"))
#' fputn(r_days, "DATE9.")
#'
#' # Multiple SAS date formats applied directly
#' today <- Sys.Date()
#' fputn(today, "DATE9.")
#' fputn(today, "MMDDYY10.")
#' fputn(today, "YYMMDD10.")
#' fputn(today, "MONYY7.")
#' fputn(today, "WORDDATE.")
#' fputn(today, "QTR.")
#'
#' # Time formatting (seconds since midnight)
#' fputn(c(0, 3600, 45000, 86399), "TIME8.")
#' fputn(c(0, 3600, 45000), "HHMM.")
#'
#' # Datetime formatting
#' now <- Sys.time()
#' fputn(now, "DATETIME20.")
#' fputn(now, "DTDATE.")
#' fputn(now, "DTYYMMDD.")
#' fclear()
fnew_date <- function(pattern, name = NULL, type = "auto",
                      .missing = NULL) {
  # Try to resolve as SAS format
  sas_def <- .resolve_sas_format_def(pattern)

  if (!is.null(sas_def)) {
    r_pattern <- sas_def$r_fmt
    dt_toupper <- sas_def$toupper
    if (type == "auto") type <- sas_def$type
    sas_name <- .normalize_sas_format_name(pattern)
  } else {
    # Custom strftime pattern
    r_pattern <- pattern
    dt_toupper <- FALSE
    sas_name <- NULL
    if (type == "auto") {
      cli_abort(c(
        "{.arg type} must be specified for custom strftime patterns.",
        "i" = "Use {.val date}, {.val time}, or {.val datetime}."
      ))
    }
  }

  if (!type %in% c("date", "time", "datetime")) {
    cli_abort("{.arg type} must be {.val date}, {.val time}, or {.val datetime}, got {.val {type}}.")
  }

  if (is.null(name)) {
    name <- if (!is.null(sas_name)) paste0(sas_name, ".") else pattern
  }

  format_obj <- structure(
    list(
      name = name,
      type = type,
      dt_pattern = r_pattern,
      dt_toupper = dt_toupper,
      sas_name = sas_name,
      mappings = list(),
      missing_label = .missing,
      other_label = NULL,
      multilabel = FALSE,
      created = Sys.time()
    ),
    class = "ks_format"
  )

  # Auto-register in library
  .format_register(format_obj)

  return(format_obj)
}


# ===========================================================================
# Date/time format application
# ===========================================================================

#' Apply a date/time format to a vector
#'
#' Handles Date, POSIXct, numeric (R epoch), and character inputs.
#'
#' @param x Input vector
#' @param format A ks_format object with date/time/datetime type
#' @param keep_na Logical. Preserve NA instead of applying missing label.
#' @return Character vector of formatted values
#' @keywords internal
.apply_datetime_format <- function(x, format, keep_na = FALSE) {
  if (is.null(x)) return(character(0))

  pattern <- format$dt_pattern
  do_upper <- isTRUE(format$dt_toupper)
  origin <- "1970-01-01"
  n <- length(x)

  # Determine what's missing in original input
  is_miss <- is.na(x)

  # Convert and format based on type
  if (format$type == "date") {
    result <- .format_date_values(x, pattern, origin = origin)
  } else if (format$type == "time") {
    result <- .format_time_values(x, pattern)
  } else if (format$type == "datetime") {
    result <- .format_datetime_values(x, pattern, origin = origin)
  } else {
    cli_abort("Unknown datetime type: {.val {format$type}}.")
  }

  # Apply toupper to non-NA results
  if (do_upper) {
    not_na <- !is.na(result)
    result[not_na] <- toupper(result[not_na])
  }

  # Handle missing values
  if (!keep_na && !is.null(format$missing_label)) {
    result[is_miss] <- format$missing_label
  }

  return(result)
}


# ===========================================================================
# Type-specific formatters
# ===========================================================================

#' Format date values
#' @keywords internal
.format_date_values <- function(x, pattern, origin = "1970-01-01") {
  # Special case: quarter format
  if (pattern == "quarter") {
    dates <- .to_r_date(x, origin = origin)
    month_num <- as.integer(format(dates, "%m"))
    qtr <- (month_num - 1L) %/% 3L + 1L
    result <- as.character(qtr)
    result[is.na(dates)] <- NA_character_
    return(result)
  }

  dates <- .to_r_date(x, origin = origin)
  result <- format(dates, pattern)
  return(result)
}


#' Format time values
#' @keywords internal
.format_time_values <- function(x, pattern) {
  no_lz_hour <- grepl("%_H", pattern, fixed = TRUE)
  std_pattern <- if (no_lz_hour) sub("%_H", "%H", pattern, fixed = TRUE) else pattern

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    result <- format(x, std_pattern)
    if (no_lz_hour) result <- sub("^0(\\d)", "\\1", result)
    return(result)
  }

  if (is.numeric(x)) {
    n <- length(x)
    result <- rep(NA_character_, n)
    not_na <- !is.na(x)
    if (!any(not_na)) return(result)

    total_secs <- x[not_na]
    h <- as.integer(total_secs %/% 3600)
    m <- as.integer((total_secs %% 3600) %/% 60)
    s <- total_secs %% 60

    h_str <- if (no_lz_hour) sprintf("%d", h) else sprintf("%02d", h)
    m_str <- sprintf("%02d", m)
    s_int_str <- sprintf("%02d", as.integer(s))
    s_frac_str <- sprintf("%.2f", s)

    has_os  <- grepl("%OS", std_pattern, fixed = TRUE)
    has_h   <- grepl("%H", std_pattern, fixed = TRUE)
    has_m   <- grepl("%M", std_pattern, fixed = TRUE)
    has_s   <- grepl("%S", std_pattern, fixed = TRUE) && !has_os

    fmt <- rep(std_pattern, length(total_secs))
    if (has_os) for (j in seq_along(fmt)) fmt[j] <- sub("%OS", s_frac_str[j], fmt[j], fixed = TRUE)
    if (has_h)  for (j in seq_along(fmt)) fmt[j] <- sub("%H", h_str[j], fmt[j], fixed = TRUE)
    if (has_m)  for (j in seq_along(fmt)) fmt[j] <- sub("%M", m_str[j], fmt[j], fixed = TRUE)
    if (has_s)  for (j in seq_along(fmt)) fmt[j] <- sub("%S", s_int_str[j], fmt[j], fixed = TRUE)

    result[not_na] <- fmt
    return(result)
  }

  dts <- tryCatch(as.POSIXct(x, tz = "UTC"), error = function(e) NULL)
  if (!is.null(dts)) {
    result <- format(dts, std_pattern)
    if (no_lz_hour) result <- sub("^0(\\d)", "\\1", result)
    return(result)
  }

  as.character(x)
}


#' Format datetime values
#' @keywords internal
.format_datetime_values <- function(x, pattern, origin = "1970-01-01") {
  dts <- .to_r_datetime(x, origin = origin)
  result <- format(dts, pattern)
  return(result)
}


# ===========================================================================
# Type conversion helpers
# ===========================================================================

#' Convert input to R Date
#'
#' Handles Date, POSIXct, numeric (days since 1970-01-01),
#' and character inputs.
#'
#' @param x Input vector
#' @param origin Character. Origin date for numeric conversion.
#'   Always \code{"1970-01-01"} (R epoch).
#' @return Date vector
#' @keywords internal
.to_r_date <- function(x, origin = "1970-01-01") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = origin))
  }
  if (is.character(x)) {
    parsed <- as.Date(x, tryFormats = c(
      "%Y-%m-%d", "%Y/%m/%d",
      "%d%b%Y", "%d%b%y"
    ))
    return(parsed)
  }
  cli_abort("Cannot convert to Date: unsupported type {.cls {class(x)[1]}}.")
}


#' Convert input to R POSIXct
#'
#' Handles POSIXct, Date, numeric (seconds since 1970-01-01),
#' and character inputs.
#'
#' @param x Input vector
#' @param origin Character. Origin date for numeric conversion.
#'   Always \code{"1970-01-01"} (R epoch).
#' @return POSIXct vector
#' @keywords internal
.to_r_datetime <- function(x, origin = "1970-01-01") {
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = origin, tz = "UTC"))
  }
  if (is.character(x)) {
    parsed <- as.POSIXct(x, tz = "UTC")
    return(parsed)
  }
  cli_abort("Cannot convert to datetime: unsupported type {.cls {class(x)[1]}}.")
}
