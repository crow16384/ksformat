# ksformat — Project Overview

## Purpose
R package providing SAS PROC FORMAT-like functionality for R.
Maps values to labels (formats), labels back to values (invalues), with range, datetime, and missing value support.

## Tech Stack
- **Language**: R (version 4.5.x)
- **Package type**: Standard R package (CRAN-style)
- **Testing**: testthat (>= 3.0.0)
- **Documentation**: roxygen2 (7.3.3)
- **Build tools**: devtools, R CMD
- **OS**: macOS / Linux (cross-platform development)
- **uv**: installed at ~/.local/bin/uv (for Serena MCP)

## Serena MCP Notes
- VS Code MCP entry points to `tools/start_serena.sh` via `.vscode/mcp.json`.
- The launcher now prefers absolute binaries from `~/.local/bin` and sets writable `UV_CACHE_DIR`/`XDG_CACHE_HOME` defaults when the shell environment is restricted.
- Serena startup depends on the global config at `~/.serena/serena_config.yml`; stale registered projects with `.serena/project.yml` missing `languages` can crash startup with `KeyError: 'languages'`.
- During repair, multiple older Serena project configs under `~/Develop/*/.serena/project.yml` were migrated to include `languages`.

## Version\n- Current: 0.8.4\n- License: GPL-3

## Core Concepts
- **Format** (`ks_format` S3 class): value → label mapping (like SAS PUT)
- **Invalue** (`ks_invalue` S3 class): label → value mapping (like SAS INPUT)
- **Datetime format** (`ks_format` with dt_pattern): SAS-style date/time/datetime formatting
- **Format Library**: global environment (`.format_library`) storing named formats
- **Special directives**: `.missing` (NA/NULL/NaN handling), `.other` (fallback)
- **Range support**: numeric range-based formatting with configurable bound inclusivity
- **Expression labels**: labels with `.x1`, `.x2` placeholders evaluated lazily
- **Multilabel**: `fput_all` returns all matching labels per element
- **ignore_case**: case-insensitive matching support
- **Bidirectional**: `fnew_bid` creates both format and invalue simultaneously\n- **Bidirectional nocase (0.8.4)**: `fnew_bid(..., ignore_case = TRUE)` applies case-insensitive matching in both directions

## Dependencies
- **Runtime**: cli
- **Suggests**: knitr, rmarkdown, shiny, testthat (>= 3.0.0)

## Key Files
- `R/format_create.R` — `fnew()`, `detect_format_type()`, `print.ks_format()`
- `R/format_apply.R` — `fput()`, `fputn()`, `fputc()`, `fput_all()`, `fput_df()`
- `R/format_invalue.R` — `finput()`, `.invalue_apply()`, `finputn()`, `finputc()`, `fnew_bid()`, `print.ks_invalue()`
- `R/format_datetime.R` — `fnew_date()`, `.apply_datetime_format()`, SAS datetime format definitions
- `R/format_parse.R` — `fparse()` (text → object parsing only, 874 lines)
- `R/format_serialize.R` — `fexport()`, `fimport()`, text rendering helpers (755 lines)
- `R/utilities.R` — `is_missing()`, `range_spec()`, `in_range()`, library management, expression label helpers
- `R/format_library_app.R` — `format_library_app()`, app data adapters, Shiny UI/server
- `R/ksformat-package.R` — package-level documentation
- `tests/testthat/test-formats.R` — comprehensive test suite (~2162 lines, ~130+ test cases)
- `tests/testthat/test-format-library-app.R` — focused tests for app helper and launcher behavior

## Exported Functions (26 total via NAMESPACE)
e, fclear, fexport, fimport, finput, finputc, finputk, finputn, flist, fmap, fnew,
fnew_bid, fnew_date, format_get, format_library_app, fparse, fprint, fput,
fput_all, fput_df, fputc, fputk, fputn, is_missing, ksformat_cheatsheet,
range_spec
