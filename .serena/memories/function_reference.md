# Function Reference — Signatures & Behavior

## format_create.R

### `fnew(..., name = NULL, type = "auto", default = NULL)`
- **Purpose**: Create a `ks_format` object (value→label mapping). Auto-registers in library if named.
- **Params**: `...` named mappings; `.missing` and `.other` are special directives.
  `name` — optional (auto-stores in library); `type` — `"auto"`, `"character"`, or `"numeric"`; `default` — alias for `.other`.
- **Returns**: `ks_format` S3 object.

### `print.ks_format(x, ...)`
- Pretty-prints a `ks_format` object with interval notation for ranges.

---

## format_apply.R

### `fput(x, format, keep_na = FALSE)`
- **Purpose**: Apply `ks_format` to a vector (like SAS PUT).
- `format` can be a `ks_format` object OR a character string (name from library).

### `fputn(x, format_name)` — Apply numeric format by name (SAS PUTN).
### `fputc(x, format_name)` — Apply character format by name (SAS PUTC).
### `fput_df(data, ..., suffix = "_fmt", replace = FALSE)` — Apply formats to data frame columns.

---

## format_invalue.R

### `finput(..., name = NULL, target_type = "numeric", missing_value = NA)`
- **Purpose**: Create `ks_invalue`. Default target is NUMERIC.
- Auto-registers in library if named.

### `.invalue_apply(x, invalue, na_if = NULL)` (internal)
- Convert labels to values. `invalue` can be object or name string.

### `finputn(x, invalue_name)` — Apply numeric invalue by name (like SAS INPUTN).
### `finputc(x, invalue_name)` — Apply character invalue by name (like SAS INPUTC).
### `fnew_bid(..., name, type)` — Creates both ks_format and ks_invalue.
### `print.ks_invalue(x, ...)`

---

## format_datetime.R

### `fnew_date(pattern, name = NULL, type = "auto", .missing = NULL)`
- **Purpose**: Create date/time format using SAS format name or strftime pattern. Auto-registers.
- No `origin` parameter — always uses R epoch (1970-01-01). SAS epoch is NOT supported.
- `type`: "date", "time", "datetime", or "auto" (auto-detect from SAS name).

---

## format_parse.R

### `fparse(text = NULL, file = NULL)` — Parse SAS-like text. Always auto-registers.
### `fexport(..., formats = NULL, file = NULL)` — Export to SAS-like text.

---

## utilities.R

### `is_missing(x, include_empty = TRUE)` — Check for NA/NaN; empty strings treated as missing by default.
### `range_spec(low, high, label, inc_low, inc_high)` — Create range spec.
### `fprint(name = NULL)` — Display format(s). Returns invisible(NULL).
### `fclear(name = NULL)` — Remove one or all formats from library.
### `.format_register(format, name, overwrite)` — Internal: store format.
### `.format_get(name)` — Internal: retrieve format.
### `.format_validate(format_obj)` — Internal: validate format structure.