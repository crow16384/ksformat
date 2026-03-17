# ksformat

[![PROC FORMAT for R](https://img.shields.io/badge/PROC%20FORMAT%20for%20R-ksformat-217346?logo=r)](https://github.com/crow16384/ksformat)
[![GitHub](https://img.shields.io/badge/GitHub-crow16384%2Fksformat-181717?logo=github)](https://github.com/crow16384/ksformat)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R package](https://img.shields.io/badge/R-package-276DC3?logo=r)](https://www.r-project.org/)

SAS-style **PROC FORMAT** for R: create and apply value formats, range-based formatting, reverse formatting (invalue), and consistent handling of missing values (NA, NULL, NaN).

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

## Missing value handling

Priority order:

1. **NA, NULL, NaN** → `.missing` label if defined, otherwise NA
2. **Exact match** → value–label mapping
3. **Range match** → range label (numeric formats)
4. **No match** → `.other` label or original value

Options: `keep_na = TRUE`, `na_if`, `include_empty = TRUE`.

## Function reference

| Area | Functions |
|------|-----------|
| **Creation** | `fnew()`, `finput()`, `fnew_bid()`, `fnew_date()`, `fparse()` |
| **Application** | `fput()`, `fputn()`, `fputc()`, `fput_all()`, `fput_df()` |
| **Reverse** | `finputn()`, `finputc()` |
| **Library** | `format_get()`, `fprint()`, `fclear()`, `fexport()`, `fimport()` |
| **Utilities** | `is_missing()`, `range_spec()` |

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

GPL-3. See [LICENSE](LICENSE) for the full text.
