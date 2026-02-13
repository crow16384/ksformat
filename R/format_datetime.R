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

  # ---- Time formats ----
  "TIME5"    = list(r_fmt = "%H:%M",      type = "time", toupper = FALSE),
  "TIME8"    = list(r_fmt = "%H:%M:%S",   type = "time", toupper = FALSE),
  "TIME11"   = list(r_fmt = "%H:%M:%OS",  type = "time", toupper = FALSE),
  "HHMM"     = list(r_fmt = "%H:%M",      type = "time", toupper = FALSE),
  "HOUR"     = list(r_fmt = "%H",         type = "time", toupper = FALSE),
  "MMSS"     = list(r_fmt = "%M:%S",      type = "time", toupper = FALSE),
  "TOD"      = list(r_fmt = "%H:%M:%S",   type = "time", toupper = FALSE),

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
  "TOD"      = "TOD",
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
#' \strong{Numeric input} is treated using the SAS epoch:
#' \itemize{
#'   \item Dates: days since January 1, 1960
#'   \item Times: seconds since midnight
#'   \item Datetimes: seconds since January 1, 1960 00:00:00
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
#' # Custom strftime pattern
#' fnew_date("%Y/%m/%d", name = "custom_date", type = "date")
#' fput(as.Date("2020-01-01"), "custom_date")
#' # [1] "2020/01/01"
#'
#' # SAS numeric date (days since 1960-01-01)
#' fputn(21915, "DATE9.")
#'
#' # Time formatting (seconds since midnight)
#' fputn(45000, "TIME8.")
#' # [1] "12:30:00"
#' fclear()
fnew_date <- function(pattern, name = NULL, type = "auto", .missing = NULL) {
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
      stop("'type' must be specified for custom strftime patterns: ",
           "'date', 'time', or 'datetime'")
    }
  }

  if (!type %in% c("date", "time", "datetime")) {
    stop("'type' must be 'date', 'time', or 'datetime', got '", type, "'")
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
#' Handles Date, POSIXct, numeric (SAS epoch), and character inputs.
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
  n <- length(x)

  # Determine what's missing in original input
  is_miss <- is.na(x)

  # Convert and format based on type
  if (format$type == "date") {
    result <- .format_date_values(x, pattern)
  } else if (format$type == "time") {
    result <- .format_time_values(x, pattern)
  } else if (format$type == "datetime") {
    result <- .format_datetime_values(x, pattern)
  } else {
    stop("Unknown datetime type: ", format$type)
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
.format_date_values <- function(x, pattern) {
  # Special case: quarter format
  if (pattern == "quarter") {
    dates <- .to_r_date(x)
    month_num <- as.integer(format(dates, "%m"))
    qtr <- (month_num - 1L) %/% 3L + 1L
    result <- as.character(qtr)
    result[is.na(dates)] <- NA_character_
    return(result)
  }

  dates <- .to_r_date(x)
  result <- format(dates, pattern)
  return(result)
}


#' Format time values
#' @keywords internal
.format_time_values <- function(x, pattern) {
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(format(x, pattern))
  }

  if (is.numeric(x)) {
    result <- character(length(x))
    for (i in seq_along(x)) {
      if (is.na(x[i])) {
        result[i] <- NA_character_
      } else {
        total_secs <- x[i]
        h <- as.integer(total_secs %/% 3600)
        m <- as.integer((total_secs %% 3600) %/% 60)
        s <- total_secs %% 60

        r <- pattern
        r <- gsub("%H", sprintf("%02d", h), r)
        r <- gsub("%M", sprintf("%02d", m), r)
        r <- gsub("%S", sprintf("%02d", floor(s)), r)
        r <- gsub("%OS", sprintf("%.2f", s), r)
        result[i] <- r
      }
    }
    return(result)
  }

  # Character or other - try as POSIXct
  dts <- tryCatch(as.POSIXct(x, tz = "UTC"), error = function(e) NULL)
  if (!is.null(dts)) {
    return(format(dts, pattern))
  }

  as.character(x)
}


#' Format datetime values
#' @keywords internal
.format_datetime_values <- function(x, pattern) {
  dts <- .to_r_datetime(x)
  result <- format(dts, pattern)
  return(result)
}


# ===========================================================================
# Type conversion helpers (SAS epoch support)
# ===========================================================================

#' Convert input to R Date
#'
#' Handles Date, POSIXct, numeric (SAS epoch: days since 1960-01-01),
#' and character inputs.
#'
#' @param x Input vector
#' @return Date vector
#' @keywords internal
.to_r_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS epoch: days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    parsed <- as.Date(x, tryFormats = c(
      "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y",
      "%Y/%m/%d", "%d-%m-%Y", "%m-%d-%Y",
      "%d%b%Y", "%d%b%y"
    ))
    return(parsed)
  }
  stop("Cannot convert to Date: unsupported type '", class(x)[1], "'")
}


#' Convert input to R POSIXct
#'
#' Handles POSIXct, Date, numeric (SAS epoch: seconds since 1960-01-01 00:00:00),
#' and character inputs.
#'
#' @param x Input vector
#' @return POSIXct vector
#' @keywords internal
.to_r_datetime <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  if (is.numeric(x)) {
    # SAS epoch: seconds since 1960-01-01 00:00:00
    return(as.POSIXct(x, origin = "1960-01-01", tz = "UTC"))
  }
  if (is.character(x)) {
    parsed <- as.POSIXct(x, tz = "UTC")
    return(parsed)
  }
  stop("Cannot convert to datetime: unsupported type '", class(x)[1], "'")
}
