# Create Date/Time Format

Creates a format object for date, time, or datetime values using SAS
format names or custom R `strftime` patterns. The format is
automatically registered in the global format library.

## Usage

``` r
fnew_date(pattern, name = NULL, type = "auto", .missing = NULL)
```

## Arguments

- pattern:

  Character. Either a SAS format name (e.g., `"DATE9."`, `"MMDDYY10."`,
  `"TIME8."`, `"DATETIME20."`) or a custom R `strftime` pattern (e.g.,
  `"%Y-%m-%d"`).

- name:

  Character. Name to register the format under. Defaults to the SAS
  format name (with period) or the pattern itself.

- type:

  Character. Type of format: `"date"`, `"time"`, `"datetime"`, or
  `"auto"` (auto-detect from SAS name). Must be specified for custom
  strftime patterns.

- .missing:

  Character. Label for missing values (NA). Default `NULL`.

## Value

A `ks_format` object with date/time type, registered in the library.

## Details

**SAS format names** are resolved automatically:

- **Date:** DATE9., DDMMYY10., MMDDYY10., YYMMDD10., MONYY7., YEAR4.,
  WEEKDATE., WORDDATE., etc.

- **Time:** TIME8., TIME5., HHMM., HOUR., MMSS.

- **Datetime:** DATETIME20., DATETIME13., etc.

**Numeric input** is converted using R epoch (`"1970-01-01"`):

- Dates: numeric values are interpreted as days since 1970-01-01

- Datetimes: numeric values are interpreted as seconds since 1970-01-01

- Times: always treated as seconds since midnight

## Examples

``` r
# Use a SAS format name
fnew_date("DATE9.", name = "mydate")
#> KS Format:mydate
#> Type: date 
#> Pattern: %d%b%Y (DATE9.) 
fput(as.Date("2020-01-01"), "mydate")
#> [1] "01JAN2020"
# [1] "01JAN2020"

# Use directly without pre-creating
fputn(as.Date("2020-06-15"), "MMDDYY10.")
#> [1] "06/15/2020"
# [1] "06/15/2020"

# Custom strftime pattern (e.g., Russian style: DD.MM.YYYY)
fnew_date("%d.%m.%Y", name = "ru_date", type = "date")
#> KS Format:ru_date
#> Type: date 
#> Pattern: %d.%m.%Y 
fput(as.Date(c("1990-03-25", "1985-11-03", "2000-07-14")), "ru_date")
#> [1] "25.03.1990" "03.11.1985" "14.07.2000"

# Custom format with missing value label
fnew_date("MMDDYY10.", name = "us_date", .missing = "NO DATE")
#> KS Format:us_date
#> Type: date 
#> Pattern: %m/%d/%Y (MMDDYY10.) 
#>   .missing => NO DATE
fput(c(as.Date("2025-01-01"), NA, as.Date("2025-12-31")), "us_date")
#> [1] "01/01/2025" "NO DATE"    "12/31/2025"
# [1] "01/01/2025" "NO DATE" "12/31/2025"

# Numeric dates (days since 1970-01-01, R epoch)
r_days <- as.numeric(as.Date("2025-01-01"))
fputn(r_days, "DATE9.")
#> [1] "01JAN2025"

# Multiple SAS date formats applied directly
today <- Sys.Date()
fputn(today, "DATE9.")
#> [1] "09JUL2026"
fputn(today, "MMDDYY10.")
#> [1] "07/09/2026"
fputn(today, "YYMMDD10.")
#> [1] "2026-07-09"
fputn(today, "MONYY7.")
#> [1] "JUL2026"
fputn(today, "WORDDATE.")
#> [1] "July 09, 2026"
fputn(today, "QTR.")
#> [1] "3"

# Time formatting (seconds since midnight)
fputn(c(0, 3600, 45000, 86399), "TIME8.")
#> [1] "0:00:00"  "1:00:00"  "12:30:00" "23:59:59"
fputn(c(0, 3600, 45000), "HHMM.")
#> [1] "00:00" "01:00" "12:30"

# Datetime formatting
now <- Sys.time()
fputn(now, "DATETIME20.")
#> [1] "09JUL2026:13:24:38"
fputn(now, "DTDATE.")
#> [1] "09JUL2026"
fputn(now, "DTYYMMDD.")
#> [1] "2026-07-09"
fclear()
#> All formats cleared from library.
```
