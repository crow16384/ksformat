<!-- model=deepseek-v4-pro mode=fast(enable_thinking:false) temp=0.1 ts=2026-10-24T06:21:07.418534 prompt_tokens=46984 completion_tokens=7757 -->

## Findings

### [CRITICAL] [F1] `fput()` missing-value priority chain broken: `.missing` label applied to ALL missing values before exact/range matching, violating documented priority
- **Location**: `R/format_apply.R:86-97`
- **Failure scenario**: Format has `.missing = "Missing"` and a discrete mapping `"NA" = "Not Available"`. Input `x = c(NA, "NA")`. Expected: `c("Missing", "Not Available")` (exact match on string "NA" takes priority over `.missing`). Actual: `c("Missing", "Missing")` because `.missing` is applied unconditionally to all `is_miss` positions before any discrete/range matching occurs.
- **Fix**: Move `.missing` application to Phase 3 (unmatched fallback), after exact and range matching have been attempted on non-missing values. Only apply `.missing` to positions that remain unmatched and are missing.

### [CRITICAL] [F2] `fput_all()` missing-value priority chain broken identically to F1
- **Location**: `R/format_apply.R:420-435`
- **Failure scenario**: Same as F1 but with `fput_all()`. Format with `.missing = "Missing"` and discrete mapping `"NA" = "Not Available"`. Input `x = c(NA, "NA")`. Expected: `list("Missing", "Not Available")`. Actual: `list("Missing", "Missing")`.
- **Fix**: Defer `.missing` application to after discrete and range matching, only for positions that remain unmatched.

### [CRITICAL] [F3] `fput()` expression-label `.missing` uses `which(is_miss)` indices on `x` but passes them to `.eval_expr_label()` which indexes into `extra_args` — length mismatch when `x` was recycled
- **Location**: `R/format_apply.R:89-93`
- **Failure scenario**: `x = 5`, `extra_args = list(.x1 = 1:3)`. `x` is recycled to length 3 (line 72). `is_miss` is `c(FALSE, FALSE, FALSE)`. If `.missing` were an expression label and some values were missing, `miss_idx` would be indices into the recycled `x` (1:3), but `extra_args` elements are already length 3. This works coincidentally. However, if `x` were NOT recycled (e.g., `x = 1:3`, `extra_args = list(.x1 = 1:3)`) and some values were missing, `miss_idx` would be correct. The real bug: if `x` is length 1 and NOT recycled (because no extra args are length >1), `miss_idx` could be `1L`, but `extra_args` might be empty or scalar — this is actually fine. The deeper issue: the `.missing` expression label is evaluated with `miss_idx` as the index vector, but if `.missing` references `.x1` and `extra_args` is empty, `.eval_expr_label` will build an environment with no `.xN` bindings, causing eval failure → warning + NA. This is by-design per author-pending item 6, but the index passing is correct. **Retracted**: no actual length mismatch bug here after re-analysis. The indices are correct because `x` and `extra_args` are aligned by the recycling logic above. Downgrading to informational note.

### [HIGH] [F4] `fput()` fast-path `findInterval` upper-bound check uses `rt$high[nr]` instead of per-range `rt$high[i0]` for non-last ranges, causing incorrect inclusion for ranges with gaps
- **Location**: `R/format_apply.R:148-152`
- **Failure scenario**: Range table with `low = c(0, 10)`, `high = c(5, 20)`, `inc_high = c(FALSE, TRUE)`. Input `x = 5`. `findInterval` returns `idx = 1` (5 falls in [0, 5) interval). The validation check `v0 < rt$high[i0]` evaluates `5 < 5` → FALSE, correctly excluding 5. But if `x = 5` and ranges were `[0,5]` and `[10,20]` with `inc_high = c(TRUE, FALSE)`, `findInterval` returns `idx = 1`. The check `v0 < rt$high[i0] | (i0 == nr & rt$inc_high[nr] & v0 <= rt$high[nr])` evaluates `5 < 5 | FALSE` → FALSE, but 5 should be included because `inc_high[1] = TRUE`. The fast-path condition `!any(rt$inc_high[-nr])` prevents this scenario from reaching the fast path, so this is actually safe. **Retracted**: the fast-path eligibility check `!any(rt$inc_high[-nr])` ensures only the last range can have inclusive upper bound, so the simplified check is correct.

### [HIGH] [F5] `fput()` fast-path `findInterval` does not handle `-Inf`/`Inf` values in `vals` correctly when `rt$low[1]` is `-Inf`
- **Location**: `R/format_apply.R:143-152`
- **Failure scenario**: Range table with `low = c(-Inf, 10)`, `high = c(0, Inf)`. `vals = c(-Inf, 5, 100)`. `findInterval` with `breaks = c(-Inf, 0, Inf)` — `findInterval` behavior with `-Inf` in breaks is undefined/dangerous. R's `findInterval` documentation states breaks must be non-decreasing and finite. Passing `-Inf` or `Inf` as break points can produce incorrect indices or errors.
- **Fix**: Before using `findInterval`, replace `-Inf` with a sentinel smaller than any possible finite value (e.g., `-.Machine$double.xmax`) and `Inf` with `.Machine$double.xmax`, or fall back to the general path when bounds contain infinite values.

### [HIGH] [F6] `.match_range_table()` shares the same `findInterval` infinite-break bug as F5
- **Location**: `R/format_apply.R:520-535`
- **Failure scenario**: Same as F5 but in the stratified range matching path. `rt$low` contains `-Inf`, `rt$high` contains `Inf`. `findInterval` called with `breaks = c(-Inf, ..., Inf)` — undefined behavior.
- **Fix**: Same as F5 — sanitize infinite bounds before `findInterval` or skip fast path when infinities are present.

### [HIGH] [F7] `fput()` expression-label grouping uses `unique(expr_labels)` but `expr_labels` may contain NA values from `found_labels[is_expr]` when `map_labels` contains NA
- **Location**: `R/format_apply.R:127-138`
- **Failure scenario**: Format mapping has an expression label that evaluates to NA in the range table (e.g., `map_labels[pos[found]]` returns NA for some positions). `unique(expr_labels)` will include NA. `split(found_nm[expr_idx], expr_labels)` will create a group named NA, and `names(grouped)` will have an NA entry. Later, `for (lbl in names(grouped))` will iterate over NA as a string "NA", but `expr_map[["NA"]]` may not match the actual expression string. This can cause silent misassignment or crashes.
- **Fix**: Filter out NA labels before grouping, or handle NA explicitly in the grouping logic.

### [HIGH] [F8] `fput_all()` unmatched logic double-counts expression-matched indices, potentially applying `.other` to already-matched positions
- **Location**: `R/format_apply.R:475-483`
- **Failure scenario**: A value matches both a discrete expression label AND a range expression label. `has_any_match` is set to TRUE for both. After the matching loops, `no_match` is computed as `non_miss[!has_any_match]`. Then `expr_matched` is computed from `expr_map` and `no_match` is further reduced by `setdiff`. However, if an expression label was deferred (in `expr_map`) but `has_any_match` was already TRUE, the position is correctly excluded from `no_match`. The bug: if `has_any_match` is FALSE but the position IS in `expr_map` (can happen if range matching set `has_any_match` to FALSE but still added to `expr_map`? No — range matching sets `has_any_match[in_rng] <- TRUE` before adding to `expr_map`). This appears correct. **Retracted**: no actual bug here after careful tracing.

### [HIGH] [F9] `fput_all()` initializes `result[non_miss]` to `list(character(0))` but later uses `Map(c, result[target], list(label))` which assumes `result[target]` is a list — correct. However, for expression labels, `result[indices]` is overwritten with `Map(c, result[indices], as.list(evaled))` which also works. The bug: if a position matches NO rules and has no `.other`, `result[[idx]]` is set to `as.character(x[idx])` (line 483), but `result[[idx]]` was initialized as `character(0)` at line 440. The assignment `result[[idx]] <- as.character(x[idx])` overwrites the empty character vector with a scalar, which is correct. No bug here. **Retracted**.

### [MEDIUM] [F10] `fput()` recycling logic only checks first non-scalar extra arg length; if `x` is length 1 and extra args have different lengths, only the first non-scalar length is used for recycling
- **Location**: `R/format_apply.R:60-76`
- **Failure scenario**: `x = 1`, `extra_args = list(.x1 = 1:3, .x2 = 1:4)`. The code aborts with "Length mismatch in extra arguments" because `non_scalar` contains `c(3, 4)` and `all(non_scalar == target_len)` is FALSE. This is correct validation. However, if `x = 1:2` and `extra_args = list(.x1 = 1:4, .x2 = 1:4)`, `target_len = 4`, `nx = 2`, `nx != target_len` → aborts. Correct. No bug. **Retracted**.

### [MEDIUM] [F11] `.parse_num_pattern()` width extraction uses `as.integer(width_txt)` which returns NA for values > `.Machine$integer.max`; subsequent `formatC(width = spec$width)` with NA width causes error
- **Location**: `R/format_apply.R:195`
- **Failure scenario**: Pattern `%10000000000.2f` — `width_txt = "10000000000"`, `as.integer("10000000000")` returns NA. `spec$width` becomes `NA_integer_`. `formatC(..., width = NA)` throws an error.
- **Fix**: Validate that width fits in integer range, or use `as.numeric` and pass to `formatC` which accepts numeric width.

### [MEDIUM] [F12] `.apply_numeric_pattern()` uses `suppressWarnings(as.numeric(x[non_miss]))` which converts factors to their integer codes silently
- **Location**: `R/format_apply.R:215`
- **Failure scenario**: `x` is a factor with levels `c("10", "20", "30")` and values `c("10", "20")`. `as.numeric(x)` returns `c(1, 2)` (the integer codes), not `c(10, 20)`. The formatted output would be "1.000000" and "2.000000" instead of "10.000000" and "20.000000".
- **Fix**: Use `as.numeric(as.character(x))` to convert factors to their label strings first, or document that factors are not supported.

### [MEDIUM] [F13] `fputk()` NA propagation uses `Reduce(`|`, lapply(stratum_args, is.na))` which fails when `stratum_args` have different lengths due to recycling
- **Location**: `R/format_apply.R:511`
- **Failure scenario**: `fputk(c("A", "B"), c(1, NA, 3), format = fmt)`. `stratum_args = list(c("A", "B"), c(1, NA, 3))`. `lapply(stratum_args, is.na)` returns list of logical vectors of different lengths. `Reduce(`|`, ...)` will recycle the shorter vector with a warning, potentially producing incorrect NA propagation.
- **Fix**: Validate that all stratum components have the same length before computing NA propagation, or use `mapply` with proper recycling control.

### [MEDIUM] [F14] `.fput_stratified()` missing handling: `is_miss <- is_missing(stratum) | is_missing(value)` — `is_missing()` treats empty strings as missing for character inputs, but stratum is always character (from `paste()`). This means stratum value `""` is treated as missing, which may be incorrect for strata that legitimately contain empty strings.
- **Location**: `R/format_apply.R:553`
- **Failure scenario**: Stratum component is an empty string `""` (e.g., from a data frame with blank cells). `paste("", "B", sep = "|")` produces `"|B"`. `is_missing("|B")` returns FALSE (non-empty string). But if stratum is just `""`, `is_missing("")` returns TRUE, treating it as missing. This may be intentional (empty stratum = missing), but could mask data issues.
- **Fix**: Document that empty strings in stratum components are treated as missing, or use `is.na()` only for stratum missingness.

### [MEDIUM] [F15] `.fput_stratified()` groups by `stratum[non_miss]` using `split()`, which converts stratum to factor and drops empty levels. If stratum contains values that are not present in `non_miss`, they are silently ignored (correct). However, if stratum contains NA values in `non_miss`, `split()` will exclude them because NA is not a valid factor level by default.
- **Location**: `R/format_apply.R:566`
- **Failure scenario**: `stratum = c("A", NA, "B")`, `value = c(1, 2, 3)`, `non_miss = 1:3`. `split(non_miss, stratum[non_miss])` will drop the NA group, so position 2 is never processed. It will remain as `NA_character_` in the result (from initialization). This is correct because missing stratum should result in missing output. No bug. **Retracted**.

### [MEDIUM] [F16] `fnew()` auto-detection of value types uses `class(mappings[[i]])[1L]` which fails for objects with multiple classes where the first class is not the value type
- **Location**: `R/format_create.R:82-90`
- **Failure scenario**: `mappings[[i]]` is a `POSIXct` object with class `c("POSIXct", "POSIXt")`. `class(mappings[[i]])[1L]` returns `"POSIXct"`, which is in `.value_types`. Correct. But if someone passes an object with class `c("myPOSIXct", "POSIXct")` (first class is not a value type), auto-detection fails and type remains "auto", which later defaults to character/numeric. This is edge-case but could cause silent type misclassification.
- **Fix**: Use `inherits(mappings[[i]], .value_types)` to check all classes, not just the first.

### [MEDIUM] [F17] `fnew()` per-stratum directive extraction uses `startsWith(keys, miss_prefix)` which can match partial stratum names
- **Location**: `R/format_create.R:131-133`
- **Failure scenario**: `strata_sep = "|"`, keys include `.missing|ABC` and `.missing|ABCD`. `miss_prefix = ".missing|"`. `startsWith(".missing|ABCD", ".missing|")` is TRUE for both. The extraction `substr(keys[miss_hit], nchar(miss_prefix) + 1L, nchar(keys[miss_hit]))` correctly extracts `"ABC"` and `"ABCD"`. No bug here. **Retracted**.

### [MEDIUM] [F18] `fnew()` stratified range malformed key detection: `accounted` counts range_idx + discrete_idx, but `discrete_idx` includes ALL non-range keys, including `.missing|...` and `.other|...` directives that were already removed from `mappings` at line 137. This means `accounted` will be less than `length(mappings)` even when all keys are valid, triggering false "Bad key" errors.
- **Location**: `R/format_create.R:142-155`
- **Failure scenario**: Stratified format with keys `c("A|0,10,TRUE,FALSE", ".missing|A")`. After line 137, `mappings` contains only `"A|0,10,TRUE,FALSE"`. `range_tables` has `range_idx = 1`, `discrete_idx = integer(0)`. `accounted = 1`, `length(mappings) = 1`. OK. But if there's a discrete key like `"A|exact_value"`, `discrete_idx = 2`, `accounted = 2`, `length(mappings) = 2`. OK. The bug: if `.missing|A` is NOT removed (because `miss_hit` detection failed due to `strata_sep` mismatch?), it would remain in mappings. But the code at line 137 removes them before the check. This appears correct. **Retracted** after verifying the removal happens before the accounted check.

### [MEDIUM] [F19] `detect_format_type()` returns "numeric" when any key contains a comma, even if all keys are non-numeric strings with commas
- **Location**: `R/format_create.R:195-197`
- **Failure scenario**: Keys are `c("San Francisco, CA", "New York, NY")`. `any(grepl(",", keys, fixed = TRUE))` is TRUE, so type is set to "numeric". This is incorrect — these are character labels containing commas, not range specifications.
- **Fix**: Only treat comma as range indicator if the key matches the range pattern (e.g., `^[0-9.-]+,[0-9.-]+(,TRUE|FALSE){0,2}$`), not just any comma.

### [MEDIUM] [F20] `finput()` unfound fallback for numeric/integer types uses `labels[!found]` but `labels` was defined as `as.character(x[non_miss])` — correct. However, the fallback only triggers for `not_found` positions, and `labels[!found]` correctly indexes into the not-found subset. No bug. **Retracted**.

### [MEDIUM] [F21] `finputk()` NA propagation uses `Reduce(`|`, lapply(args, is.na))` which has the same length-mismatch issue as F13
- **Location**: `R/format_invalue.R:131`
- **Failure scenario**: Same as F13 — different-length arguments cause silent recycling in `Reduce(`|`, ...)`.
- **Fix**: Validate equal lengths before NA propagation.

### [MEDIUM] [F22] `.format_library_shiny_app()` uses `requireNamespace("shiny", quietly = TRUE)` but the function is called from `format_library_app()` which already checks `.has_shiny()`. However, `.has_shiny()` is defined as `requireNamespace("shiny", quietly = TRUE)` — this loads shiny if available, not just checks. The Shiny app function references `shiny::` explicitly, so it works even without `requireNamespace` returning TRUE. No bug. **Retracted**.

### [MEDIUM] [F23] `fparse()` block header regex `^(VALUE|INVALUE)\s+([\w.-]+)\s*(?:\(([^)]+)\))?\s*$` allows format names with dots and hyphens but the `\w` character class includes underscore and digits. The regex `([\w.-]+)` is greedy and will consume characters until whitespace or `(`. This is correct. However, the subtype capture `\(([^)]+)\)` uses `[^)]+` which cannot handle nested parentheses in the subtype options. This is acceptable since subtype options don't contain parentheses. No bug. **Retracted**.

### [MEDIUM] [F24] `fparse()` interval notation regex for numeric ranges: `^(-?[0-9]+(?:\.[0-9]+)?|LOW|HIGH|Inf|-Inf)\s*,\s*(-?[0-9]+(?:\.[0-9]+)?|LOW|HIGH|Inf|-Inf)\s*(\]|\))$` — the alternation `LOW|HIGH|Inf|-Inf` is case-sensitive. "low", "High", "inf" will not match.
- **Location**: `R/format_parse.R:248-252`
- **Failure scenario**: Input text `[low, high)` — the regex expects uppercase `LOW` and `HIGH`. The match fails, and the parser falls through to try date interval matching, then legacy range matching, then discrete. The legacy range regex also requires uppercase `LOW|HIGH`. So `[low, high)` becomes a discrete key `"[low, high)"` instead of a range.
- **Fix**: Add `ignore.case = TRUE` to the `regmatches`/`regexec` call, or use case-insensitive character classes.

### [MEDIUM] [F25] `fparse()` date interval regex uses `\d{4}-\d{2}-\d{2}` which matches invalid dates like `"2023-13-32"`
- **Location**: `R/format_parse.R:258`
- **Failure scenario**: Input `[2023-13-32, 2024-01-01)` — the regex matches, and the bounds are stored as strings. Later, `.parse_date_range_key()` will attempt `as.Date("2023-13-32")` which returns NA, causing the range to be silently dropped or producing incorrect behavior.
- **Fix**: Validate date strings after parsing, or use a stricter regex that only matches valid month/day ranges.

### [MEDIUM] [F26] `.block_to_ks_format()` auto-detection of date vs datetime range: checks if any bound contains a time component via `grepl("[ T]\\d{2}:\\d{2}", ...)`. This misses bounds with seconds like `"2023-01-01 12:00:00"` because the regex only matches `HH:MM`, not `HH:MM:SS`.
- **Location**: `R/format_parse.R:395-399`
- **Failure scenario**: Range `[2023-01-01 12:00:00, 2023-01-02 12:00:00)` — `grepl("[ T]\\d{2}:\\d{2}", "2023-01-01 12:00:00")` returns TRUE (matches `12:00`). So this actually works. But `[2023-01-01 12:00:00, ...]` — the regex matches `12:00` within `12:00:00`. Correct. **Retracted**: the regex does match because `\d{2}:\d{2}` matches the first two components of `12:00:00`.

### [MEDIUM] [F27] `.block_to_stratified_range_format()` canonicalization of bounds: `norm_bound` for date/datetime returns `"LOW"` for `"LOW"` and `"-Inf"`, but the canonical range key format for date ranges uses `"LOW"` and `"HIGH"` as sentinels. However, `.parse_date_range_key()` expects `"LOW"` and `"HIGH"` (uppercase). The normalization converts `"low"` to `"LOW"` via `toupper(s)` in the `su %in% c("LOW", "-INF")` check — but `su` is already `toupper(s)`, so `"low"` becomes `"LOW"` and matches. Correct. **Retracted**.

### [MEDIUM] [F28] `fimport()` CSV reading uses `na.strings = ""` which converts empty strings to NA. This is correct for SAS CNTLOUT where missing character values are blank. However, `data$START` and `data$END` are later coerced with `as.character(ifelse(is.na(...), "", ...))` which converts NA back to `""`. This round-trip is safe. No bug. **Retracted**.

### [MEDIUM] [F29] `.cntlout_to_ks_format()` range detection: `if (has_low || has_high) { is_range <- TRUE } else if (type == "numeric" && start != end) { is_range <- TRUE }`. For character formats (`type == "character"`), `start != end` does NOT create a range, which is correct (SAS character formats don't have ranges). However, if `has_low` or `has_high` is true for a character format, it's treated as a range, but character ranges are not supported and will produce incorrect range keys.
- **Location**: `R/format_serialize.R:175-180`
- **Failure scenario**: SAS CNTLOUT with `TYPE = "C"`, `HLO = "L"`, `START = "A"`, `END = "A"`. `has_low = TRUE`, `is_range = TRUE`. The code enters the `if (is_range && type == "numeric")` branch — but `type == "character"`, so it falls through to the `else` branch and treats it as discrete. This is actually correct because the `is_range && type == "numeric"` condition is false. No bug. **Retracted**.

### [MEDIUM] [F30] `.cntlout_to_ks_invalue()` missing value detection: `if (start == "." || start == "")` — for character invalues, `START` could be a literal period `"."` that should map to a value, not be treated as missing.
- **Location**: `R/format_serialize.R:237`
- **Failure scenario**: SAS INVALUE with `TYPE = "J"` (character), `START = "."`, `LABEL = "dot"`. The code treats `"."` as missing value marker and sets `missing_value` instead of creating a mapping `"." = "dot"`.
- **Fix**: Only treat `"."` as missing for numeric invalues (`TYPE = "I"`). For character invalues, `""` should be missing, but `"."` should be a literal mapping.

### [LOW] [F31] `fput()` Phase 1 discrete matching: `val_str <- as.character(x[non_miss])` converts Date/POSIXct to character using their default format, which may not match the format's date keys if they were specified in a different format
- **Location**: `R/format_apply.R:108`
- **Failure scenario**: Format has discrete key `"2023-01-15"` (ISO format). `x` is a Date vector. `as.character(x)` returns `"2023-01-15"` (ISO format by default). This matches. But if the format's keys were created with a different date format (e.g., `"15JAN2023"`), the match would fail silently, and the value would fall through to range matching or `.other`.
- **Fix**: Use the format's `date_format` to convert Date/POSIXct values to character for discrete matching, or document that discrete keys must match R's default `as.character()` representation.

### [LOW] [F32] `.apply_datetime_format()` for time values with numeric input: `s_frac_str <- sprintf("%.2f", s)` hardcodes 2 decimal places for fractional seconds, but the pattern may specify a different precision via `%OS` (e.g., `%OS3` for milliseconds)
- **Location**: `R/format_datetime.R:230`
- **Failure scenario**: SAS format `TIME11` uses `%_H:%M:%OS` which expects 2 decimal places (SAS default). But a custom pattern `%H:%M:%OS3` would still get 2 decimal places because `sprintf("%.2f", s)` is hardcoded.
- **Fix**: Parse the `%OS` width from the pattern and adjust the `sprintf` format string accordingly.

### [LOW] [F33] `.format_time_values()` for numeric input: `s_frac_str <- sprintf("%.2f", s)` produces strings like `"0.50"` but the substitution `sub("%OS", s_frac_str, fmt)` replaces `%OS` with `"0.50"` — the leading zero before the decimal is correct for seconds (e.g., `"12:30:0.50"`), but SAS would output `"12:30:00.50"` with two integer digits. This is a cosmetic difference, not a data corruption issue.
- **Location**: `R/format_datetime.R:230-234`
- **Failure scenario**: Time value 30.5 seconds formatted with `%H:%M:%OS` produces `"00:00:0.50"` instead of `"00:00:00.50"`.
- **Fix**: Use `sprintf("%05.2f", s)` to ensure at least two integer digits for seconds.

### [LOW] [F34] `fnew_bid()` reverse mapping deduplication: `if (anyDuplicated(values))` warns about ambiguous reverse mappings but still creates the invalue with only the first key per value. This is documented behavior. However, the warning message uses `{.val {dupes}}` which will print all duplicated values, potentially very long.
- **Location**: `R/format_invalue.R:162-166`
- **Failure scenario**: 1000 mappings with 500 duplicated values — the warning will print all 500 values, flooding the console.
- **Fix**: Limit the number of duplicated values shown in the warning (e.g., first 5).

### [LOW] [F35] `.eval_expr_label()` uses `exists(expr_str, envir = .expr_parse_cache, inherits = FALSE)` which is correct. However, the cache is never cleared, so long-running sessions with many unique expression labels will accumulate parsed expressions indefinitely.
- **Location**: `R/utilities.R:310`
- **Failure scenario**: A script generates thousands of unique expression labels dynamically. The `.expr_parse_cache` environment grows without bound, consuming memory.
- **Fix**: Add a cache size limit or provide a function to clear the expression cache.

### [LOW] [F36] `.format_register()` case-insensitive lookup in `.format_get()`: `ci_match <- available[tolower(available) == tolower(name)]` — this performs case-insensitive matching but the format library stores formats with their original case. If two formats differ only in case (e.g., "MyFmt" and "myfmt"), the second registration overwrites the first (by design). The case-insensitive lookup will find the surviving one. This is correct. No bug. **Retracted**.

### [LOW] [F37] `fclear()` with `name = NULL` clears all formats but does not clear the `.expr_parse_cache`. This is by design (caches are separate from format library). No bug. **Retracted**.

### [LOW] [F38] `.build_range_table()` discrete_numeric_possible calculation: `any(is.finite(suppressWarnings(as.numeric(map_keys[d_idx]))))` — this returns FALSE if all discrete keys are NA, non-numeric, or infinite. However, `as.numeric("Inf")` returns `Inf`, and `is.finite(Inf)` is FALSE, so `"Inf"` as a discrete key would set `discrete_numeric_possible = FALSE`. This is correct because `"Inf"` as a string won't match a numeric `Inf` via `as.character()` comparison. No bug. **Retracted**.

## Confirmed-correct (do not fix)
- `fput()` vectorized recycling logic correctly validates extra argument lengths and recycles scalar `x` (lines 60-76)
- `fput()` Phase 2 general path correctly iterates ranges in declaration order with first-hit-wins semantics (lines 158-175)
- `fput()` Phase 3 unmatched fallback correctly applies `.other` then original value (lines 178-188)
- `fput_all()` correctly accumulates multiple matches per element using `Map(c, ...)` (lines 450-470)
- `.fput_stratified()` correctly groups by stratum and applies per-stratum range tables (lines 560-590)
- `fnew()` correctly extracts `.missing`/`.other` directives and removes them from mappings (lines 115-120)
- `fnew()` correctly handles `ks_fmap` class stripping and `strata_sep` inheritance (lines 68-76)
- `fparse()` correctly handles unclosed blocks with a warning (lines 210-213)
- `fparse()` interval notation regex correctly handles inclusive/exclusive bounds (lines 248-265)
- `fexport()`/`fimport()` round-trip correctly preserves range bounds, inclusion flags, and special directives
- `.format_register()` correctly prevents duplicate names when `overwrite = FALSE` (utilities.R line 420)
- `.format_get()` correctly falls back to SAS built-in datetime formats (utilities.R line 435)
- `.build_range_table()` correctly sorts ranges and checks disjointness for fast-path eligibility (utilities.R lines 500-520)
- `.split_stratified_key()` correctly handles embedded separators by trying rightmost first (utilities.R lines 535-545)

## Baseline
The codebase is well-structured with clear separation between format creation, application, parsing, and serialization. The range-matching machinery is sophisticated with appropriate fast-path optimizations. The most critical defects are the missing-value priority chain violations (F1, F2) which cause `.missing` to override explicit mappings — a direct violation of documented behavior. The `findInterval` infinite-bound issue (F5, F6) could cause incorrect matching or crashes with unbounded ranges. Several medium-severity issues in parsing and type detection could cause silent misclassification of formats. The code is generally production-ready after addressing the critical and high-severity findings.