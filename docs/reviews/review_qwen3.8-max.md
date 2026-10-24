<!-- model=qwen3.8-max mode=fast(enable_thinking:false) temp=0.1 ts=2026-10-24T06:19:55.715976 prompt_tokens=47218 completion_tokens=3317 -->

## Findings

### [SEVERITY Critical] [ID-1] `fput_all` overwrites instead of accumulating labels for unmatched elements
- Location: R/format_apply.R:370-378
- Failure scenario: For a multilabel format, if an element matches no discrete key and no range but has an `.other` label (or falls through to original value), the code executes `result[[idx]] <- as.character(x[idx])`. Since `result[non_miss]` was initialized to `list(character(0))`, this assignment replaces the empty vector rather than appending. If any prior phase had accumulated labels for that index (e.g., via expression eval), they would be silently destroyed. More critically, the logic contradicts the function's purpose: it should accumulate all matching labels plus the fallback, but for the no-match case it discards the accumulation pattern used everywhere else in the function.
- Fix: Change line 378 from `result[[idx]] <- as.character(x[idx])` to `result[[idx]] <- c(result[[idx]], as.character(x[idx]))`, and similarly wrap the `.other` static-label branch on line 374 with `c(result[[idx]], ...)`.

### [SEVERITY Critical] [ID-2] `is_missing()` treats all empty strings as missing, corrupting intentional `""` mappings
- Location: R/utilities.R:269-273
- Failure scenario: `is_missing()` returns `TRUE` for `""` on all non-numeric types. If a user defines a character format with an explicit mapping for the empty string (`fnew("" = "Blank", "A" = "Alpha")`), calling `fput(c("", "A"), fmt)` will route `""` into the `.missing`/NA path before Phase 1 discrete matching ever runs. The empty-string mapping is silently unreachable, producing NA or the `.missing` label instead of `"Blank"`. This is silent data corruption for clinical datasets where `""` is a valid coded value distinct from NA.
- Fix: Remove `| !nzchar(x)` from `is_missing()`, or gate it behind a format-level flag. Empty strings should only be treated as missing when no explicit `""` key exists in the mappings.

### [SEVERITY High] [ID-3] Range-table fast-path sorts ranges, violating declaration-order-first-match semantics
- Location: R/utilities.R:350-356
- Failure scenario: `.build_range_table()` unconditionally sorts ranges by `(low, high)` and sets `sorted_disjoint = TRUE` when they don't overlap after sorting. In `fput()` (format_apply.R:145-163), the fast path uses this sorted order via `findInterval`. If a user defines overlapping ranges relying on declaration-order precedence (documented as BY DESIGN), e.g., `fnew("0,10,TRUE,FALSE" = "A", "3,5,TRUE,FALSE" = "B")`, the sort reorders them to `[0,10)` then `[3,5)`. `findInterval` assigns values in `[3,5)` to the first interval containing them — which is now `[0,10)` — returning `"A"` instead of the declared-first-match `"A"`. While this specific example happens to match, reversing the declaration order (`"3,5"=B` first, `"0,10"=A` second) would incorrectly return `"A"` for value 4 because the sort destroys the original declaration order that should give `"B"` first-match priority.
- Fix: Only set `sorted_disjoint = TRUE` when ranges are already sorted AND disjoint in their original declaration order. Alternatively, skip the sort entirely and only enable the fast path when the original order is already sorted and disjoint.

### [SEVERITY High] [ID-4] POSIXct timezone drift in value-type range matching
- Location: R/format_apply.R:268-274
- Failure scenario: In `.fput_value_type()` for POSIXct formats, input values are coerced via `as.POSIXct(as.character(x[unmatched_nm]))` without specifying `tz`. This parses in the session's local timezone. However, `.parse_datetime_range_key()` (utilities.R:195-196) always parses range bounds with `tz = "UTC"`. If the session is in US/Eastern (UTC-5), an input of midnight UTC displays as `"2024-01-01 19:00:00"` locally; `as.POSIXct()` re-parses that string as 19:00 Eastern (= midnight+5h UTC internally). The comparison against UTC-based range bounds is shifted by the timezone offset, causing values to fall outside their correct ranges. Silent wrong-answer bug dependent on deployment environment.
- Fix: Extract the timezone from the input `x` (or the format's map_values) and pass it explicitly: `as.POSIXct(..., tz = tz)`.

### [SEVERITY High] [ID-5] `fputn`/`fputc` silently drop elements where `format_name` is NA
- Location: R/format_apply.R:300-301
- Failure scenario: When `format_name` is a vector, `.fput_vectorized()` computes `unique(format_names[!is.na(format_names)])` and iterates only over non-NA format names. Elements where `format_name[i]` is `NA` are never assigned in the loop. Since `result` is initialized to `rep(NA_character_, n)`, these positions silently become `NA_character_` regardless of the input value, the format type, or whether `keep_na` is set. In a clinical pipeline where a per-row format variable has missing entries, those rows produce NA output with no warning.
- Fix: After the loop, identify indices where `is.na(format_names)` and apply appropriate fallback logic (original value or explicit NA handling) rather than leaving them as uninitialized `NA_character_`.

### [SEVERITY High] [ID-6] `fimport` CNTLOUT numeric range detection fails for single-value mappings
- Location: R/format_serialize.R:189-193
- Failure scenario: When importing SAS CNTLOUT CSV data, the code determines `is_range` for numeric types via `start != end`. In SAS PROC FORMAT CNTLOUT, discrete numeric mappings (e.g., `1 = "Yes"`) have identical START and END values, so `is_range` is correctly FALSE. However, if a SAS format maps a single numeric value using a range row where START == END (which SAS does for point values in some CNTLOUT outputs), this works. The real bug: for **character** formats, there is no `start != end` check at all — everything goes to the `else` branch (line 207) as a discrete mapping. But for numeric formats where START and END differ due to floating-point representation in the CSV (e.g., START="1.0", END="1.00"), `start != end` evaluates to TRUE as a string comparison, creating a spurious range `[1, 1]` instead of a discrete mapping for `1`.
- Fix: Compare `suppressWarnings(as.numeric(start)) != suppressWarnings(as.numeric(end))` for numeric types instead of raw string comparison.

### [SEVERITY Medium] [ID-7] `finput` nocase lookup silently drops duplicate case-insensitive keys
- Location: R/format_invalue.R:68-71
- Failure scenario: When `ignore_case = TRUE`, the code does `match(tolower(labels), tolower(map_keys))`. If the invalue was defined with both `"Yes" = 1` and `"yes" = 2`, `tolower(map_keys)` produces `c("yes", "yes")`. R's `match()` always returns the position of the *first* match, so input `"YES"` always resolves to `1`, and the mapping `"yes" = 2` is silently unreachable. No warning is emitted despite the ambiguity.
- Fix: At creation time in `finput()`, when `ignore_case = TRUE`, check for case-insensitive duplicate keys and either warn or error.

### [SEVERITY Medium] [ID-8] `.eval_expr_label` parse cache keyed by raw expression string causes cross-format collision
- Location: R/utilities.R:296-304
- Failure scenario: The `.expr_parse_cache` environment uses the expression string as the key. If format A defines `e(".x1 + 1")` and format B defines `e(".x1 + 1")` but they are intended to be evaluated in different parent environments or with different extra_args semantics, the cached parse tree from format A is reused for format B. While `parse()` output is generally environment-independent, if one expression was parsed under a source encoding or option context that differs, the cached version could produce subtly wrong results. More concretely, if a parse failure occurs for one format (returning NULL and caching nothing), but another format later registers the same string successfully, the cache state depends on call order — introducing non-determinism across sessions.
- Fix: Include the format name or a hash of the parent environment in the cache key, or scope the cache per-format rather than globally.

### [SEVERITY Medium] [ID-9] `fparse` block header regex rejects format names with spaces or special characters
- Location: R/format_parse.R:52-54
- Failure scenario: The regex `^(VALUE|INVALUE)\\s+([\\w.-]+)\\s*...` restricts format names to word characters, dots, and hyphens. SAS allows format names with `$` prefix (character formats) and other characters. If a user tries to parse `VALUE $SEX ...`, the `$` is not matched by `[\\w.-]`, so the block header is not recognized. The parser emits a warning ("Looks like a block header but could not be parsed") and skips the entire block silently. All mappings within are lost.
- Fix: Expand the character class to include `$` and other SAS-valid format name characters, e.g., `[\\w.$-]+`.

### [SEVERITY Medium] [ID-10] `fput` numeric pattern applies `format$missing_label` even when `keep_na = TRUE` for non-missing non-numeric values
- Location: R/format_apply.R:218-220
- Failure scenario: In `.apply_numeric_pattern()`, the `keep_na` parameter correctly gates application of `missing_label` to NA/NaN inputs. However, for non-missing inputs that fail `as.numeric()` coercion (e.g., `"abc"`), lines 241-245 apply `format$other_label` or `as.character()` fallback. The issue is that `is_miss` on line 215 uses `is_missing()` which treats `""` as missing for character input (see ID-2). So `fput("abc", num_fmt, keep_na = TRUE)` works correctly, but `fput("", num_fmt, keep_na = TRUE)` still gets routed through the missing path because `is_missing("")` is TRUE, and `keep_na` only prevents the missing *label* — the value is still excluded from numeric formatting and becomes NA_character_ (from initialization) rather than being preserved as `""`.
- Fix: When `keep_na = TRUE`, preserve the original value for missing elements instead of leaving them as `NA_character_` from the initialized result vector.

### [SEVERITY Low] [ID-11] `_H` leading-zero stripping regex can corrupt formatted output
- Location: R/format_datetime.R:167
- Failure scenario: After formatting time values with `%_H` replaced by `%H`, the code strips leading zeros via `sub("^0(\\d)", "\\1", result)`. If the formatted time string is `"00:30:00"` (midnight-thirty), this produces `"0:30:00"` which is correct. However, if the format includes a date prefix or any text before the hour (e.g., a datetime format that somehow routes through this path), the `^0` anchor would strip a leading zero from the date portion instead of the hour. Additionally, for hours >= 10 like `"10:30"`, the regex doesn't match, which is correct. But for single-digit hours with padding elsewhere, this global substitution approach is fragile.
- Fix: Use a more targeted replacement that specifically addresses the hour component position rather than anchoring to `^`.

### [SEVERITY Low] [ID-12] `fnew_bid` reverse mapping uses first key per duplicated value without deterministic guarantee
- Location: R/format_invalue.R:175-183
- Failure scenario: When multiple keys map to the same value, `fnew_bid` warns about ambiguity and states "only the first key per value will be used." However, the reversal logic iterates `names(mappings)` and builds `reversed_mappings` via `do.call(c, ...)`. When duplicate values exist as names in the reversed list, R's list behavior means the last entry with that name wins during subsequent lookups, not the first. The warning message claims first-key-wins but the implementation gives last-key-wins.
- Fix: Explicitly deduplicate by keeping only the first occurrence: `!duplicated(values)` filter before building the reversed mapping.

## Confirmed-correct (do not fix)
- Declaration-order precedence for overlapping ranges in the general (non-fast) path: correctly implemented via sequential scanning with `matched` tracking.
- `.missing` → exact → range → `.other` → original-value priority chain in `fput()`: correctly sequenced across Phases 1-3.
- Expression label eval failure producing warning + NA per element: correctly handled via `tryCatch` in `.eval_expr_label()`.
- Global registry `.format_library` same-name overwrite by design: `.format_register()` implements this correctly.
- `fputk()` NA propagation through `paste()` with `na_as_string` toggle: correctly restores `NA_character_` after paste coercion.
- `LOW`/`HIGH` sentinel parsing to `-Inf`/`Inf` in `.parse_range_bound()`: correct.
- `findInterval` fast-path boundary validation with per-range upper bound check: correctly handles gaps between ranges.
- Value-type formats ignoring `.missing`/`.other` with warning: correctly implemented in `fnew()`.
- `fexport`/`fparse` round-trip for standard discrete and range formats: serialization grammar matches parser expectations.
- `fclear()` scoping to the package-level `.format_library` environment: correct.

## Baseline
The ksformat package is architecturally sound with a well-structured separation between format creation, application, parsing, and serialization. The core `fput()` priority chain and vectorized matching logic are correctly implemented for the common cases. However, there are two critical defects — the `fput_all` overwrite bug and the `is_missing()` empty-string conflation — that will cause silent data corruption in production clinical pipelines. The range-table sorting bug undermines the documented declaration-order semantics for overlapping ranges, and the POSIXct timezone drift creates environment-dependent wrong answers. These four issues must be resolved before CRAN submission.