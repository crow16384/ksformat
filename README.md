# ksformat <img src="man/figures/logo.svg" align="right" height="139" alt="ksformat logo" />

[![PROC FORMAT for R](https://img.shields.io/badge/PROC%20FORMAT%20for%20R-ksformat-217346?logo=r)](https://github.com/crow16384/ksformat)
[![GitHub](https://img.shields.io/badge/GitHub-crow16384%2Fksformat-181717?logo=github)](https://github.com/crow16384/ksformat)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R package](https://img.shields.io/badge/R-package-276DC3?logo=r)](https://www.r-project.org/)
[![](https://cranlogs.r-pkg.org/badges/ksformat)](https://cran.r-project.org/package=ksformat)

SAS-style **PROC FORMAT** for R: create and apply value formats, range-based formatting, reverse formatting (invalue), and consistent handling of missing values (NA, NULL, NaN).

**Repository:** [github.com/crow16384/ksformat](https://github.com/crow16384/ksformat) — source code, issue tracker, and development.

## Installation

**From GitHub** (after cloning or from your repo URL):

```r
# install.packages("remotes")
remotes::install_github("crow16384/ksformat")
```

**From local source:**

```r
install.packages(".", repos = NULL, type = "source")
# or
devtools::install()
```

## Features

- **Format creation** — Value-to-label mappings like SAS PROC FORMAT
- **Format application** — Apply formats to vectors and data frames
- **Reverse formatting** — Convert labels back to values (INVALUE)
- **Missing value handling** — NA, NULL, NaN, and empty values
- **Range support** — Numeric ranges with inclusive/exclusive bounds
- **Multilabel** — A single value can match multiple labels (`fput_all`)
- **Expression labels** — Dynamic labels with `.x1`, `.x2`, … evaluated at apply-time
- **Case-insensitive matching** — `ignore_case = TRUE` in `fnew`
- **Date/time/datetime** — Built-in SAS format names and custom `strftime` patterns
- **Import/export** — Parse SAS-like text (`fparse`), export (`fexport`), import CNTLOUT CSV (`fimport`)
- **Format library** — Register and retrieve formats globally

## Quick start

### Discrete formatting

```r
library(ksformat)

fnew(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  name = "sex"
)

fput(c("M", "F", NA, "X"), "sex")
# [1] "Male"    "Female"  "Unknown" "X"
```

### Numeric ranges

```r
fparse(text = '
VALUE age (numeric)
  [0, 18)   = "Child"
  [18, 65)  = "Adult"
  [65, HIGH] = "Senior"
  .missing   = "Age Unknown"
;
')

fputn(c(5, 25, 70, NA), "age")
# [1] "Child"       "Adult"       "Senior"      "Age Unknown"
```

### Reverse formatting (invalue)

```r
finput("Male" = 1, "Female" = 2, name = "sex_inv")

finputn(c("Male", "Female", "Unknown"), "sex_inv")
# [1]  1  2 NA
```

### Format library

```r
fprint()              # list all registered formats
fmt <- format_get("sex")
fclear("sex")         # remove one format
fclear()              # clear all
```

### Data frames

```r
df <- data.frame(
  sex = c("M", "F", "M", NA),
  age = c(15, 25, 70, 35)
)

fput_df(df, sex = format_get("sex"), age = format_get("age"), suffix = "_label")
```

## Multilabel formats

With `multilabel = TRUE`, a single value can match multiple labels. Use `fput_all()` to collect all matches:

```r
fnew(
  "0,17,TRUE,TRUE"  = "Pediatric",
  "18,Inf,TRUE,TRUE" = "Adult",
  "3,5,TRUE,TRUE"   = "Serious",
  name = "ae_age", type = "numeric", multilabel = TRUE
)

fput_all(c(10, 25, 4), "ae_age")
# [[1]] "Pediatric"
# [[2]] "Adult"
# [[3]] "Pediatric" "Serious"
```

## Date/time/datetime formats

SAS date format names are auto-resolved — no pre-creation needed:

```r
fputn(Sys.Date(), "DATE9.")
# [1] "25MAR2026"

fputn(Sys.Date(), "MMDDYY10.")
# [1] "03/25/2026"

# Custom strftime pattern
fnew_date("%d.%m.%Y", name = "ru_date", type = "date")
fput(Sys.Date(), "ru_date")
# [1] "25.03.2026"
```

Time (seconds since midnight) and datetime are also supported:

```r
fputn(3600, "TIME8.")
# [1] "1:00:00"

fputn(Sys.time(), "DATETIME20.")
```

## Expression labels

Labels containing `.x1`, `.x2`, etc. are evaluated as R expressions at apply-time. Pass extra arguments through `fput(x, fmt, ...)`:

```r
stat_fmt <- fnew(
  "n"   = "sprintf('%s', .x1)",
  "pct" = "sprintf('%.1f%%', .x1 * 100)",
  name = "stat", type = "character"
)

fput(c("n", "pct"), stat_fmt, c(42, 0.053))
# [1] "42"   "5.3%"
```

Use `e()` to mark a label for evaluation even without `.xN` placeholders:

```r
fnew("ts" = e("format(Sys.time(), '%Y-%m-%d')"), name = "demo")
fput("ts", "demo")
```

## Case-insensitive matching

```r
fnew("M" = "Male", "F" = "Female", name = "sex_nc",
     type = "character", ignore_case = TRUE)

fput(c("m", "F", "M", "f"), "sex_nc")
# [1] "Male"   "Female" "Male"   "Female"
```

## Missing value handling

Priority order:

1. **NA, NULL, NaN** → `.missing` label if defined, otherwise NA
2. **Exact match** → value–label mapping
3. **Range match** → range label (numeric formats)
4. **No match** → `.other` label or original value

Options: `keep_na = TRUE`, `na_if`, `include_empty = TRUE`.

## Cheat sheet

- **In R:** run `ksformat_cheatsheet()` to open the cheat sheet in your browser (HTML), or `ksformat_cheatsheet("pdf")` for the PDF.
- **In this repo:** [HTML](inst/doc/ksformat-cheatsheet.html) | [PDF](inst/doc/ksformat-Cheat-Sheet.pdf)

## Function reference

| Area | Functions |
|------|-----------|
| **Creation** | `fnew()`, `finput()`, `fnew_bid()`, `fnew_date()`, `fparse()`, `e()` |
| **Application** | `fput()`, `fputn()`, `fputc()`, `fput_all()`, `fput_df()` |
| **Reverse** | `finputn()`, `finputc()` |
| **Library** | `format_get()`, `fprint()`, `fclear()`, `fexport()`, `fimport()` |
| **Utilities** | `is_missing()`, `range_spec()` |
| **Documentation** | `ksformat_cheatsheet()` — open cheat sheet |

## Development

```r
install.packages(c("roxygen2", "testthat", "devtools"))
devtools::document()
devtools::test()
devtools::check()
```

When bumping the package version, update `DESCRIPTION` and then run  
`Rscript scripts/sync-version.R` to refresh version references in `cran-comments.md` and any other synced files.

## License

GPL-3. See <https://www.gnu.org/licenses/gpl-3.0.html>.
