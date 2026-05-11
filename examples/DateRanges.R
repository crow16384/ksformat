## DateRanges.R
## Demonstrates date_range and datetime_range format types in ksformat.
##
## These types bucket Date / POSIXct values into character labels using
## ISO date/datetime interval bounds.  The same range-table fast path
## (findInterval) used for numeric ranges is active for sorted, disjoint
## date buckets.

devtools::load_all()

# ── 1. Fiscal-year bucketing (programmatic, via fnew) ────────────────────────

fnew(
  "2023-01-01,2024-01-01,TRUE,FALSE" = "FY23",
  "2024-01-01,2025-01-01,TRUE,FALSE" = "FY24",
  "2025-01-01,2026-01-01,TRUE,FALSE" = "FY25",
  type = "date_range",
  name = "fiscal_year"
)

dates <- as.Date(c("2023-06-15", "2024-03-01", "2024-12-31",
                   "2025-07-04", "2022-01-01", NA))

cat("\n── Fiscal year ──\n")
print(data.frame(date = dates, fy = fput(dates, "fiscal_year")))
cat("\nrange_table$sorted_disjoint:",
    format_get("fiscal_year")$range_table$sorted_disjoint, "\n")

# ── 2. Quarterly bucketing with fparse ───────────────────────────────────────

fparse(text = '
VALUE quarter (date_range)
  [2024-01-01, 2024-04-01) = "Q1-2024"
  [2024-04-01, 2024-07-01) = "Q2-2024"
  [2024-07-01, 2024-10-01) = "Q3-2024"
  [2024-10-01, 2025-01-01) = "Q4-2024"
  .other                   = "Outside 2024"
;
')

cat("\n── Quarterly ──\n")
sample_dates <- as.Date(c("2024-02-14", "2024-05-20", "2024-08-08",
                          "2024-11-30", "2025-03-01"))
print(data.frame(date = sample_dates,
                 quarter = fput(sample_dates, "quarter")))

# ── 3. LOW / HIGH open-ended bounds ──────────────────────────────────────────

fparse(text = '
VALUE era (date_range)
  [LOW,        2000-01-01) = "Pre-2000"
  [2000-01-01, 2010-01-01) = "2000s"
  [2010-01-01, 2020-01-01) = "2010s"
  [2020-01-01, HIGH]       = "2020+"
  .missing                 = "No date"
;
')

cat("\n── Era (LOW/HIGH) ──\n")
event_dates <- as.Date(c("1985-07-04", "2005-12-25",
                         "2015-06-01", "2023-11-11", NA))
print(data.frame(date = event_dates, era = fput(event_dates, "era")))

# ── 4. POSIXct input (date_range truncates to days) ──────────────────────────

cat("\n── POSIXct input → date_range (date-level precision) ──\n")
pct_dates <- as.POSIXct(c("2024-02-29 23:59:59",
                           "2024-08-15 12:00:00"), tz = "UTC")
print(data.frame(
  ts = format(pct_dates, tz = "UTC"),
  fy = fput(pct_dates, "fiscal_year")
))

# ── 5. Exclusive lower bound ─────────────────────────────────────────────────

fparse(text = '
VALUE strict (date_range)
  (2024-01-01, 2025-01-01) = "Strictly 2024"
  .other = "Boundary or outside"
;
')

cat("\n── Exclusive bound ──\n")
strict_dates <- as.Date(c("2024-01-01", "2024-06-15", "2025-01-01"))
print(data.frame(date = strict_dates,
                 label = fput(strict_dates, "strict")))

# ── 6. Overlapping multilabel ────────────────────────────────────────────────

fparse(text = '
VALUE study_window (date_range, multilabel)
  [2024-01-01, 2024-07-01) = "First Half"
  [2024-04-01, 2024-10-01) = "Mid-Year"
  [2024-07-01, 2025-01-01) = "Second Half"
;
')

cat("\n── Overlapping windows (fput_all) ──\n")
checkup_dates <- as.Date(c("2024-02-15", "2024-05-20", "2024-09-01"))
all_windows   <- fput_all(checkup_dates, "study_window")
for (i in seq_along(checkup_dates)) {
  cat(format(checkup_dates[i]), "->",
      paste(all_windows[[i]], collapse = " | "), "\n")
}

# ── 7. Auto-detection ────────────────────────────────────────────────────────

cat("\n── Auto-detection ──\n")
fparse(text = '
VALUE auto_fy
  [2024-01-01, 2025-01-01) = "2024"
;
VALUE auto_shift
  [2024-01-15 08:00, 2024-01-15 16:00) = "Day shift"
;
')
cat("auto_fy type   :", format_get("auto_fy")$type, "\n")
cat("auto_shift type:", format_get("auto_shift")$type, "\n")

# ── 8. datetime_range: shift bucketing ───────────────────────────────────────

fparse(text = '
VALUE shift (datetime_range)
  [2024-01-15 00:00, 2024-01-15 08:00) = "Night"
  [2024-01-15 08:00, 2024-01-15 16:00) = "Day"
  [2024-01-15 16:00, 2024-01-16 00:00) = "Evening"
;
')

cat("\n── Shift bucketing (datetime_range) ──\n")
timestamps <- as.POSIXct(
  c("2024-01-15 03:22:00", "2024-01-15 11:45:00", "2024-01-15 19:00:00"),
  tz = "UTC"
)
print(data.frame(ts    = format(timestamps, tz = "UTC"),
                 shift = fput(timestamps, "shift")))

# ── 9. fexport / fparse roundtrip ────────────────────────────────────────────

cat("\n── fexport ──\n")
q_obj <- format_get("quarter")
txt <- fexport(quarter = q_obj)
cat(txt, "\n")

fclear()
fparse(text = txt)
cat("── re-parsed ──\n")
print(fput(sample_dates, "quarter"))

fclear()
