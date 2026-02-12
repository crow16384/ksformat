# Codebase Architecture

## File Layout & Symbol Map

### R/format_create.R (lines 1–163)
Handles format object creation and validation.

| Symbol | Kind | Lines | Exported | Description |
|--------|------|-------|----------|-------------|
| `format_create` | Function | 41–83 | Yes | Creates `ks_format` S3 object from `...` mappings |
| `detect_format_type` | Function | 91–117 | Yes | Auto-detects "numeric" vs "character" type from mapping keys |
| `validate_mappings` | Function | 124–136 | Yes | Checks all labels are character strings |
| `print.ks_format` | S3 method | 143–163 | Yes (S3method) | Pretty-prints a `ks_format` object |

### R/format_apply.R (lines 1–158)
Applies formats to vectors and data frames.

| Symbol | Kind | Lines | Exported | Description |
|--------|------|-------|----------|-------------|
| `format_apply` | Function | 24–91 | Yes | Applies `ks_format` to a vector; handles NA, exact match, range (stub), `.other` |
| `format_apply_df` | Function | 116–143 | Yes | Applies named formats to data frame columns; `suffix`/`replace` modes |
| `format_put` | Function | 155–158 | Yes | **Stub** — intended to apply format by name from library; currently `stop()`s |

### R/format_invalue.R (lines 1–225)
Reverse formatting (label → value) and bidirectional support.

| Symbol | Kind | Lines | Exported | Description |
|--------|------|-------|----------|-------------|
| `format_invalue` | Function | 29–54 | Yes | Creates `ks_invalue` S3 object from label→value mappings |
| `detect_invalue_type` | Function | 61–76 | Yes | Auto-detects target type (character/numeric/integer/logical) |
| `invalue_apply` | Function | 94–153 | Yes | Applies `ks_invalue` to a vector; handles NA, `na_if`, type conversion |
| `format_bidirectional` | Function | 179–202 | Yes | Creates both `ks_format` + `ks_invalue` from one set of mappings |
| `print.ks_invalue` | S3 method | 209–225 | Yes (S3method) | Pretty-prints a `ks_invalue` object |

### R/utilities.R (lines 1–249)
Utility functions and global format library.

| Symbol | Kind | Lines | Exported | Description |
|--------|------|-------|----------|-------------|
| `is_missing_value` | Function | 25–37 | Yes | Checks NA/NaN; optionally empty strings |
| `range_spec` | Function | 55–72 | Yes | Creates `range_spec` S3 object (low, high, label) |
| `in_range` | Function | 82–88 | No (internal) | Tests if value is within a `range_spec` (inclusive both ends) |
| `.format_library` | Environment | 93 | No | `new.env(parent = emptyenv())` — global format store |
| `format_register` | Function | 113–134 | Yes | Stores format/invalue in `.format_library` by name |
| `format_get` | Function | 150–157 | Yes | Retrieves format from library by name |
| `format_list` | Function | 169–171 | Yes | Returns `ls()` of all registered format names |
| `format_remove` | Function | 187–196 | Yes | Removes one format from library |
| `format_clear` | Function | 205–209 | Yes | Removes all formats from library |
| `format_validate` | Function | 225–249 | Yes | Applies format + reports which values matched vs used `.other` |

### R/format_parse.R
Parsing SAS-like text definitions and exporting formats back to text.

| Symbol | Kind | Lines | Exported | Description |
|--------|------|-------|----------|-------------|
| `format_parse` | Function | – | Yes | Parses SAS-like text/file into `ks_format`/`ks_invalue` objects |
| `format_export` | Function | – | Yes | Exports format objects to SAS-like text or file |
| `.parse_blocks` | Function | – | No (internal) | Splits text lines into block structures |
| `.parse_mapping_line` | Function | – | No (internal) | Parses a single `key = value` mapping line |
| `.parse_range_bound` | Function | – | No (internal) | Converts LOW/HIGH/numeric strings to values |
| `.unquote` | Function | – | No (internal) | Strips surrounding quotes |
| `.block_to_format` | Function | – | No (internal) | Dispatches block to ks_format or ks_invalue builder |
| `.block_to_ks_format` | Function | – | No (internal) | Converts VALUE block to ks_format object |
| `.block_to_ks_invalue` | Function | – | No (internal) | Converts INVALUE block to ks_invalue object |
| `.format_to_text` | Function | – | No (internal) | Converts ks_format to SAS-like text |
| `.invalue_to_text` | Function | – | No (internal) | Converts ks_invalue to SAS-like text |
| `.format_range_bound` | Function | – | No (internal) | Formats numeric bound as LOW/HIGH/number string |

### R/ksformat-package.R
Package-level roxygen2 documentation only. No symbols.

## S3 Classes

### `ks_format`
Created by `format_create()`. Structure:
```r
list(
  name          = character(1) | NULL,
  type          = "character" | "numeric",
  mappings      = named list (key=value_string, value=label_string),
  missing_label = character(1) | NULL,   # from .missing directive
  other_label   = character(1) | NULL,   # from .other directive or default param
  created       = POSIXct
)
```

### `ks_invalue`
Created by `format_invalue()`. Structure:
```r
list(
  name          = character(1) | NULL,
  target_type   = "character" | "numeric" | "integer" | "logical",
  mappings      = named list (key=label_string, value=target_value),
  missing_value = NA (default) or custom,
  created       = POSIXct
)
```

### `range_spec`
Created by `range_spec()`. Structure:
```r
list(low = numeric, high = numeric, label = character)
```

## Global State
- `.format_library` (R environment in `R/utilities.R` line 93): stores named formats.
  Managed by `format_register`, `format_get`, `format_list`, `format_remove`, `format_clear`.

## Inter-function Dependencies (call graph)
- `format_create` → `detect_format_type`, `validate_mappings`
- `format_apply` — standalone (uses `ks_format` slots directly)
- `format_apply_df` → `format_apply`
- `format_put` — stub, not functional
- `format_invalue` → `detect_invalue_type`
- `invalue_apply` — standalone (uses `ks_invalue` slots directly)
- `format_bidirectional` → `format_create`, `format_invalue`
- `format_validate` → `format_apply`
- `format_register` → uses `.format_library`
- `format_get` → `format_list`, uses `.format_library`

## SAS-like Text Format Syntax
Supported by `format_parse` and `format_export` (R/format_parse.R):
- `VALUE name (type) ... ;` — defines a ks_format
- `INVALUE name (target_type) ... ;` — defines a ks_invalue
- Discrete: `"key" = "label"` or unquoted `key = label`
- Ranges: `low - high = "label"` (VALUE blocks only)
- Special keywords: `LOW` (-Inf), `HIGH` (Inf)
- Directives: `.missing = "label"`, `.other = "label"`
- Comments: `//`, `/* */`, `*`, `#`
- Type in parentheses is optional (defaults to "auto")

## Known Stubs / Incomplete Areas
1. **`format_put`** (R/format_apply.R:155–158): always `stop()`s — not implemented.
2. **Range matching in `format_apply`** (R/format_apply.R:69–76): the loop body for numeric range matching is empty (no actual range check logic).
3. **`detect_format_type`** (R/format_create.R:93): `has_ranges` is always FALSE due to `sapply(mappings, function(x) FALSE)`.
