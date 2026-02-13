# Codebase Architecture

## File Layout

### R/format_create.R
- `fnew` (exported) — Create `ks_format`, auto-register if named
- `detect_format_type` (internal) — Auto-detect "numeric" vs "character"
- `validate_mappings` (internal) — Check labels are character
- `print.ks_format` (S3method) — Pretty-print format

### R/format_apply.R
- `fput` (exported) — Apply format to vector (accepts object or name)
- `fputn` (exported) — Apply numeric format by library name (SAS PUTN)
- `fputc` (exported) — Apply character format by library name (SAS PUTC)
- `format_apply_df` (exported) — Apply formats to data frame columns

### R/format_invalue.R
- `finput` (exported) — Create `ks_invalue`, default numeric, auto-register
- `detect_invalue_type` (internal) — Detect target type
- `invalue_apply` (exported) — Apply invalue (accepts object or name)
- `finputn` (exported) — Apply numeric invalue by name (SAS INPUTN)
- `finputc` (exported) — Apply character invalue by name (SAS INPUTC)
- `format_bidirectional` (exported) — Create format + invalue pair
- `print.ks_invalue` (S3method) — Pretty-print invalue

### R/format_parse.R
- `fparse` (exported) — Parse SAS-like text, always auto-registers
- `format_export` (exported) — Export formats to SAS-like text
- `.parse_blocks`, `.parse_mapping_line`, `.parse_range_bound`, `.unquote` (internal parsers)
- `.block_to_format`, `.block_to_ks_format`, `.block_to_ks_invalue` (internal builders)
- `.format_to_text`, `.invalue_to_text`, `.format_range_bound` (internal exporters)

### R/utilities.R
- `is_missing` (exported) — Check NA/NaN, optionally empty strings
- `range_spec` (exported) — Create range_spec S3 object
- `in_range` (internal) — Test value against range_spec
- `.parse_range_key` (internal) — Parse range key string
- `.format_library` (environment) — Global format store
- `.format_register` (internal) — Store format in library
- `.format_get` (internal) — Retrieve format from library
- `fprint` (exported) — Display format(s), returns invisible(NULL)
- `fclear` (exported) — Remove format(s) from library
- `.format_validate` (internal) — Validate format structure

### R/ksformat-package.R
- Package-level roxygen2 documentation

## S3 Classes
- `ks_format` — value→label mapping (VALUE)
- `ks_invalue` — label→value mapping (INVALUE), default numeric
- `range_spec` — range bound specification

## Key Design Decisions
- Formats auto-register in `.format_library` on creation/parsing
- INVALUE defaults to numeric target_type
- `fput`/`invalue_apply` accept both objects and name strings
- `fprint` is display-only (invisible(NULL)), not for retrieval
- `fclear(name)` replaces both old format_remove and format_clear
