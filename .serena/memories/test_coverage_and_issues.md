# Test Coverage & Known Issues

## Test File: tests/testthat/test-formats.R
Single test file, 7 test cases, using `context("Format Creation and Application")`.

### Tests Present

| # | Test name | Functions tested | What's checked |
|---|-----------|-----------------|----------------|
| 1 | `format_create works with discrete values` | `format_create` | S3 class, name, type="character", mapping count |
| 2 | `format_apply handles missing values correctly` | `format_create`, `format_apply` | NA→"Unknown", exact match, unmatched returns original |
| 3 | `format_apply preserves NA when keep_na = TRUE` | `format_create`, `format_apply` | keep_na=TRUE preserves NA |
| 4 | `invalue_apply reverses formatting` | `format_invalue`, `invalue_apply` | label→value, NA mapping |
| 5 | `is_missing_value detects various missing types` | `is_missing_value` | NA, NaN, empty string, include_empty |
| 6 | `format_register and format_get work` | `format_create`, `format_register`, `format_list`, `format_get`, `format_remove` | register/retrieve/remove cycle |
| 7 | `format_bidirectional creates both format and invalue` | `format_bidirectional`, `format_apply`, `invalue_apply` | forward + reverse roundtrip |

### Functions NOT Tested
- `detect_format_type` — called indirectly via format_create, but no explicit edge cases tested
- `detect_invalue_type` — called indirectly, no explicit tests
- `validate_mappings` — called indirectly, no error-path tests
- `format_apply_df` — **not tested at all**
- `format_put` — stub, not tested
- `format_validate` — **not tested at all**
- `format_clear` — **not tested at all**
- `range_spec` — **not tested at all**
- `in_range` — **not tested at all** (internal)
- `print.ks_format` — **not tested at all**
- `print.ks_invalue` — **not tested at all**

### Known Issues / Incomplete Features

1. **Range matching not implemented** in `format_apply` (R/format_apply.R lines 69–76):
   The loop for numeric range matching exists but has an empty body — range-based formats will never match.

2. **`format_put` is a stub** (R/format_apply.R lines 155–158):
   Always calls `stop()`. Intended to look up format by name from library and apply it.

3. **`detect_format_type` dead code** (R/format_create.R line 93):
   `has_ranges` is always FALSE because `sapply(mappings, function(x) FALSE)` is used.

4. **No negative/edge case tests**: No tests for error paths (invalid inputs, duplicate keys, empty vectors, etc.).

5. **`is_missing_value(NULL)` returns `logical(0)`**: This behavior is tested implicitly but may surprise users.

6. **No integration test for data frame workflow**: `format_apply_df` with `suffix`/`replace` modes untested.

7. **`in_range` uses inclusive bounds on both ends** (`>=` and `<=`): This means adjacent ranges like `c(0,18)` and `c(18,65)` will double-match value 18. SAS uses `low <= x < high` by default.

## NAMESPACE
17 exported functions + 2 S3 methods. All match the functions defined in R/ files.
`detect_format_type`, `detect_invalue_type`, `validate_mappings` are exported (arguably should be internal).
`in_range` is NOT exported (correct — internal helper).
