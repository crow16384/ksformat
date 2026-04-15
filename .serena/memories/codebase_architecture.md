# ksformat Codebase Architecture (updated 2026-04-15)

## Package Overview
R package providing SAS PROC FORMAT-like functionality. Version 0.4.9.
Depends on: cli. Suggests: testthat (>= 3.0.0).

## File Structure

### R/format_create.R (lines ~126)
- `fnew(...)` — Create format (ks_format). Supports discrete, range, .missing, .other, multilabel, ignore_case. Auto-registers named formats.
- `detect_format_type(keys)` — Auto-detect "character" or "numeric" from key names.
- `print.ks_format(x, ...)` — Print method for ks_format. Shows interval notation for ranges, date patterns.

### R/format_apply.R
- `fput(x, format, ..., keep_na)` — Apply format to vector. Handles missing → exact match → range match → .other. Expression labels (.x1, .x2) evaluated lazily.
- `.fput_vectorized(x, format_names, type_check, ...)` — Per-element format application (different format per element).
- `fputn(x, format_name, ...)` — Apply numeric format by name (like SAS PUTN). Supports vectorized format_name.
- `fputc(x, format_name, ...)` — Apply character format by name (like SAS PUTC). Supports vectorized format_name.
- `fputk(..., format, sep, keep_na)` — Apply format keyed on composite paste key (e.g. USUBJID|VISITNUM). NA in any component propagates as NA.
- `fput_all(x, format, ..., keep_na)` — Multilabel: returns list of all matching labels per element.
- `fput_df(data, ..., suffix, replace)` — Apply formats to data frame columns.

### R/format_invalue.R
- `finput(...)` — Create invalue (ks_invalue) for reverse formatting (label → value).
- `.invalue_apply(x, invalue, na_if)` — Apply invalue to convert labels to values. Vectorized match().
- `finputn(x, invalue_name)` — Apply numeric invalue by name (like SAS INPUTN).
- `finputc(x, invalue_name)` — Apply character invalue by name (like SAS INPUTC).
- `fnew_bid(...)` — Create bidirectional format+invalue pair. Invalue named {name}_inv.
- `print.ks_invalue(x, ...)` — Print method for ks_invalue.

### R/format_datetime.R
- `.sas_datetime_formats` — Named list of all SAS date/time/datetime format definitions.
- `.sas_format_defaults` — Default widths for SAS format names without explicit width.
- `.normalize_sas_format_name(name)` — Strip trailing period, uppercase.
- `.resolve_sas_format_def(name)` — Lookup in .sas_datetime_formats.
- `.is_sas_datetime_format(name)` — Check if name is built-in SAS datetime format.
- `fnew_date(pattern, name, type, .missing)` — Create date/time/datetime format.
- `.apply_datetime_format(x, format, keep_na)` — Apply dt format to vector.
- `.format_date_values(x, pattern, origin)` — Format Date values. Special quarter handling.
- `.format_time_values(x, pattern)` — Format time values (seconds since midnight → HH:MM:SS).
- `.format_datetime_values(x, pattern, origin)` — Format datetime values.
- `.to_r_date(x, origin)` — Convert various inputs to R Date.
- `.to_r_datetime(x, origin)` — Convert various inputs to R POSIXct.

### R/format_parse.R (lines ~1154)
- `fparse(text, file)` — Parse SAS-like text into format/invalue objects. Auto-registers.
- `fexport(..., formats, file)` — Export formats to SAS-like text.
- `fimport(file, register, overwrite)` — Import formats from SAS CNTLOUT CSV file.
- `.cntlout_to_ks_format(...)` — Convert CNTLOUT data to ks_format objects.
- `.cntlout_to_ks_invalue(...)` — Convert CNTLOUT data to ks_invalue objects.
- `.parse_blocks(lines)` — Parse text lines into block structures.
- `.parse_mapping_line(line, line_num)` — Parse single mapping line (LHS = RHS).
- `.parse_range_bound(s, is_low)` — Parse range bound (LOW→-Inf, HIGH→Inf).
- `.unquote(s)` — Remove surrounding quotes.
- `.block_to_format(block)` — Dispatch to ks_format or ks_invalue converter.
- `.block_to_ks_format(block)` — Convert VALUE block to ks_format.
- `.block_to_ks_datetime_format(block)` — Convert datetime VALUE block.
- `.block_to_ks_invalue(block)` — Convert INVALUE block to ks_invalue.
- `.format_to_text(fmt, name)` — Convert ks_format to SAS-like text.
- `.datetime_format_to_text(fmt, name)` — Convert datetime format to text.
- `.invalue_to_text(inv, name)` — Convert ks_invalue to text.
- `.format_range_bound(val, is_low)` — Format numeric bound for text output.

### R/utilities.R
- `.is_expr_label(label)` — Check if label contains .x1, .x2, etc.
- `.eval_expr_label(expr_str, extra_args, indices)` — Evaluate expression label.
- `is_missing(x)` — Check for NA, NaN, "", "NaN". Returns logical vector.
- `range_spec(low, high, label, inc_low, inc_high)` — Create range_spec object.
- `in_range(x, range_spec)` — Check if value falls in range.
- `.parse_range_key(key)` — Parse "low,high,inc_low,inc_high" or "low,high" range key string.
- `.format_library` — Global environment for storing formats.
- `.format_register(format, name, overwrite)` — Register format in library.
- `.format_get(name)` — Retrieve format from library. Case-insensitive fallback. SAS datetime auto-resolve.
- `fprint(name)` — Print/list format(s) from library.
- `fclear(name)` — Remove format(s) from library.
- `.format_validate(format_obj)` — Validate format structure.

### R/ksformat-package.R
- Package-level documentation (empty, roxygen2-generated).

## Key Design Patterns

1. **Format resolution**: `fput` accepts both ks_format objects and string names (resolved via `.format_get`).
2. **Auto-registration**: `fnew`, `finput`, `fnew_date`, `fparse`, `fimport` auto-register named formats in `.format_library`.
3. **SAS datetime auto-resolve**: `.format_get` auto-creates SAS datetime formats on first use.
4. **Vectorized matching**: `fput` uses `match()` for exact lookups, then iterates range entries.
5. **Expression labels**: Labels with `.x1`, `.x2` are deferred and evaluated in batch.
6. **Multilabel**: `fput_all` allows overlapping range matches, returns list of character vectors.
7. **CNTLOUT import**: `fimport` reads SAS format catalogue CSV exports.

## Performance Notes
- `fput` Phase 1: vectorized `match()` for discrete keys (O(n) amortized).
- `fput` Phase 2: iterative range matching — each range entry scanned against unmatched values.
- `fput_all` iterates all keys/ranges against all values (no early exit).
- `.fput_vectorized` groups by format name for efficiency.
