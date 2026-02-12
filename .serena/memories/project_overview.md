# ksformat — Project Overview

## Purpose
R package providing SAS PROC FORMAT-like functionality for R.
Maps values to labels (formats), labels back to values (invalues), with range and missing value support.

## Tech Stack
- **Language**: R (version 4.5.2)
- **Package type**: Standard R package (CRAN-style)
- **Testing**: testthat (>= 3.0.0)
- **Documentation**: roxygen2 (7.3.3)
- **Build tools**: devtools, R CMD
- **OS**: Linux (Debian 13, remote via SSH)

## Version
- Current: 0.1.0
- License: MIT

## Core Concepts
- **Format** (`ks_format` S3 class): value → label mapping (like SAS PUT)
- **Invalue** (`ks_invalue` S3 class): label → value mapping (like SAS INPUT)
- **Format Library**: global environment (`.format_library`) storing named formats
- **Special directives**: `.missing` (NA/NULL/NaN handling), `.other` (fallback)
- **Range support**: numeric range-based formatting via `c(low, high)` syntax

## Key Files
- `R/format_create.R` — `format_create()`, `detect_format_type()`, `validate_mappings()`, `print.ks_format()`
- `R/format_apply.R` — `format_apply()`, `format_apply_df()`, `format_put()`
- `R/format_invalue.R` — `format_invalue()`, `detect_invalue_type()`, `invalue_apply()`, `format_bidirectional()`, `print.ks_invalue()`
- `R/utilities.R` — `is_missing_value()`, `range_spec()`, `in_range()`, format library functions (`format_register`, `format_get`, `format_list`, `format_remove`, `format_clear`, `format_validate`), `.format_library` environment
- `R/ksformat-package.R` — package-level documentation (no symbols)
- `tests/testthat/test-formats.R` — test suite (testthat)

## Exported Functions (17 total)
format_create, format_apply, format_apply_df, format_put, format_invalue,
invalue_apply, format_bidirectional, format_register, format_get, format_list,
format_remove, format_clear, format_validate, is_missing_value, range_spec,
print.ks_format, print.ks_invalue

## Dependencies
- No runtime dependencies (base R only)
- Suggests: testthat (>= 3.0.0)
