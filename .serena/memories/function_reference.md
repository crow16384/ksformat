# Function Reference — Signatures & Behavior

## format_create.R

### `format_create(..., name = NULL, type = "auto", default = NULL)`
- **Purpose**: Create a `ks_format` object (value→label mapping).
- **Params**: `...` named mappings (`"M" = "Male"`); `.missing` and `.other` are special directives extracted from `...`.
  `name` — optional format name; `type` — `"auto"`, `"character"`, or `"numeric"`; `default` — alias for `.other`.
- **Returns**: `ks_format` S3 object.
- **Validation**: At least 1 mapping required. Calls `detect_format_type` if auto, then `validate_mappings`.

### `detect_format_type(names, mappings)`
- **Purpose**: Guess whether format is "numeric" or "character".
- **Logic**: Returns "numeric" if any key can be coerced to numeric or if any key name is empty/NULL. Otherwise "character".
- **Note**: `has_ranges` variable is always FALSE (dead code).

### `validate_mappings(mappings, type)`
- **Purpose**: Ensure all labels are characters.
- **Logic**: `unlist(mappings)` → `sapply(is.character)` → `stop()` if any FALSE.

### `print.ks_format(x, ...)`
- Prints name, type, all key→value pairs, `.missing`, `.other`.

---

## format_apply.R

### `format_apply(x, format, keep_na = FALSE)`
- **Purpose**: Apply `ks_format` to a vector, returning character vector of labels.
- **Logic**:
  1. NULL input → `character(0)`.
  2. Missing (NA/NaN) → `missing_label` if set and `keep_na=FALSE`, else NA.
  3. Exact match: `as.character(value) == key` in mappings.
  4. Range match: **not implemented** (loop body is empty).
  5. No match: `other_label` or original value as character.
- **Returns**: `character` vector same length as `x`.

### `format_apply_df(data, ..., suffix = "_fmt", replace = FALSE)`
- **Purpose**: Apply named formats to data frame columns.
- **Params**: `...` named args like `sex = sex_fmt`. `suffix` for new column name. `replace = TRUE` overwrites original.
- **Logic**: Iterates named formats, warns if column missing, calls `format_apply` per column.

### `format_put(x, format_name)`
- **Status**: STUB — always `stop()`s.

---

## format_invalue.R

### `format_invalue(..., name = NULL, target_type = "auto", missing_value = NA)`
- **Purpose**: Create `ks_invalue` object (label→value mapping).
- **Params**: `...` named mappings (`"Male" = "M"`); `target_type` — auto-detected or specified; `missing_value` — value for unmatched.
- **Returns**: `ks_invalue` S3 object.

### `detect_invalue_type(mappings)`
- **Logic**: Checks `unlist(mappings)` — all logical→"logical", all numeric→"integer" or "numeric", else "character".

### `invalue_apply(x, invalue, na_if = NULL)`
- **Purpose**: Convert labels back to values using `ks_invalue`.
- **Logic**:
  1. NULL → empty vector of target type.
  2. Init result with `missing_value`.
  3. NA values + `na_if` matches → stay as `missing_value`.
  4. Non-missing: look up label in `names(invalue$mappings)` → type-convert via `switch`.
  5. Unmatched numeric/integer: try `as.numeric(label)` fallback.
- **Returns**: vector of `target_type`.

### `format_bidirectional(..., name = NULL, type = "auto")`
- **Purpose**: Create both `ks_format` and `ks_invalue` from same mappings.
- **Logic**: Calls `format_create(...)`, then reverses mapping keys/values and calls `format_invalue(...)`.
- **Returns**: `list(format = ks_format, invalue = ks_invalue)`.

### `print.ks_invalue(x, ...)`
- Prints name, target_type, mappings, missing_value (if not NA).

---

## utilities.R

### `is_missing_value(x, include_empty = FALSE)`
- **Purpose**: Element-wise check for NA/NaN; optionally empty strings.
- **Returns**: logical vector. NULL input → `logical(0)`.

### `range_spec(low, high, label)`
- **Purpose**: Create a `range_spec` S3 object.
- **Validation**: low/high must be numeric, low ≤ high.
- **Returns**: `range_spec` (list with low, high, label).

### `in_range(x, range_spec)` *(internal, not exported)*
- **Purpose**: Check `x >= low & x <= high` (inclusive both ends).
- **Returns**: logical.

### `.format_library`
- R environment `new.env(parent = emptyenv())` for storing named formats.

### `format_register(format, name = NULL, overwrite = FALSE)`
- Stores `ks_format` or `ks_invalue` in `.format_library`. Uses `format$name` if `name` not given.
- Errors if name exists and `overwrite=FALSE`.

### `format_get(name)`
- Retrieves from `.format_library`. Errors with available list if not found.

### `format_list()`
- Returns `ls(envir = .format_library)` — character vector of registered names.

### `format_remove(name)`
- Removes from library. Warns if not found. Returns TRUE/FALSE.

### `format_clear()`
- Removes all formats from library.

### `format_validate(x, format)`
- Applies format, checks which values had exact key matches vs `.other`.
- Returns `data.frame(value, matched, label)`.
