# ksformat Function Reference (updated 2026-04-21)

## Exported Functions

| Function | File | Purpose |
|----------|------|---------| 
| `fnew(..., fmap)` | format_create.R | Create ks_format (value→label mapping). `fmap` param controls named-vector direction (NULL=auto, FALSE=never, TRUE=always) |
| `fput(x, format, ..., keep_na)` | format_apply.R | Apply format to vector |
| `fputn(x, format_name, ...)` | format_apply.R | Apply numeric format by name |
| `fputc(x, format_name, ...)` | format_apply.R | Apply character format by name |
| `fputk(..., format, sep, keep_na, na_as_string)` | format_apply.R | Apply format with composite paste key; `na_as_string=TRUE` keeps literal "NA" for paste()/fmap()-built keys |
| `fput_all(x, format, ..., keep_na)` | format_apply.R | Multilabel: all matching labels |
| `fput_df(data, ..., suffix, replace)` | format_apply.R | Apply formats to data frame cols |
| `finput(..., ignore_case)` | format_invalue.R | Create ks_invalue (label→value); `ignore_case=TRUE` enables case-insensitive lookup |
| `finputn(x, invalue_name)` | format_invalue.R | Apply numeric invalue by name |
| `finputc(x, invalue_name)` | format_invalue.R | Apply character invalue by name |
| `finputk(..., invalue_name, sep, na_as_string)` | format_invalue.R | Apply invalue with composite paste label; mirrors `fputk()` on the invalue side |
| `fnew_bid(...)` | format_invalue.R | Create bidirectional format+invalue |
| `fnew_date(pattern, name, type, .missing)` | format_datetime.R | Create date/time/datetime format |
| `format_library_app(port, launch.browser)` | format_library_app.R | Launch Shiny browser for format library |
| `fparse(text, file)` | format_parse.R | Parse SAS-like text definitions |
| `fexport(..., formats, file)` | format_serialize.R | Export formats to SAS-like text |
| `fimport(file, register, overwrite)` | format_serialize.R | Import SAS CNTLOUT CSV |
| `flist()` | utilities.R | Return character vector of registered format names |
| `fprint(name)` | utilities.R | Print/list formats from library |
| `fclear(name)` | utilities.R | Remove format(s) from library |
| `is_missing(x)` | utilities.R | Check for missing values |
| `range_spec(low, high, label)` | utilities.R | Create range specification |
| `franges(fmt)` | utilities.R | Extract range entries from ks_format as data.frame (cols: low, high, inc_low, inc_high, label) |
| `fmap_to_ranges(x, fmt)` | utilities.R | Reverse-lookup range bounds by label; returns data.frame with low/high/inc_low/inc_high per element of x |
| `detect_format_type(keys)` | format_create.R | Auto-detect format type |

## Internal Functions

| Function | File | Purpose |
|----------|------|---------| 
| `.fput_vectorized()` | format_apply.R | Per-element format application |
| `.invalue_apply()` | format_invalue.R | Apply invalue format |
| `.is_expr_label()` | utilities.R | Check expression label |
| `.eval_expr_label()` | utilities.R | Evaluate expression label |
| `.parse_range_key()` | utilities.R | Parse range key string |
| `.format_library` | utilities.R | Global format storage env |
| `.format_register()` | utilities.R | Register format in library |
| `.format_get()` | utilities.R | Retrieve format from library |
| `.format_validate()` | utilities.R | Validate format structure |
| `in_range()` | utilities.R | Check if value in range |
| `.sas_datetime_formats` | format_datetime.R | SAS format definitions |
| `.sas_format_defaults` | format_datetime.R | Default SAS widths |
| `.normalize_sas_format_name()` | format_datetime.R | Normalize SAS name |
| `.resolve_sas_format_def()` | format_datetime.R | Resolve SAS format def |
| `.is_sas_datetime_format()` | format_datetime.R | Check if SAS datetime |
| `.apply_datetime_format()` | format_datetime.R | Apply datetime format |
| `.format_date_values()` | format_datetime.R | Format date values |
| `.format_time_values()` | format_datetime.R | Format time values |
| `.format_datetime_values()` | format_datetime.R | Format datetime values |
| `.to_r_date()` | format_datetime.R | Convert to R Date |
| `.to_r_datetime()` | format_datetime.R | Convert to R POSIXct |
| `.is_eval_label(label, precomputed)` | utilities.R | Combines `.is_expr_label()` + `.has_eval_attr()`; optional precomputed shortcut |
| `.parse_range_key_by_type(key, type, date_format)` | utilities.R | Dispatch to correct range key parser by format type |
| `.format_range_interval(parsed)` | utilities.R | Render parsed range list as `"[low, high)"` string |
| `.parse_blocks()` | format_parse.R | Parse text to blocks |
| `.parse_mapping_line()` | format_parse.R | Parse mapping line |
| `.parse_range_bound()` | format_parse.R | Parse range bound |
| `.unquote()` | format_parse.R | Remove quotes |
| `.block_to_format()` | format_parse.R | Block → format dispatch |
| `.block_to_ks_format()` | format_parse.R | Block → ks_format |
| `.block_to_ks_datetime_format()` | format_parse.R | Block → datetime format |
| `.block_to_ks_invalue()` | format_parse.R | Block → ks_invalue |
| `.format_to_text()` | format_serialize.R | ks_format → text |
| `.datetime_format_to_text()` | format_serialize.R | datetime format → text |
| `.stratified_format_to_text()` | format_serialize.R | stratified_range format → text |
| `.invalue_to_text()` | format_serialize.R | ks_invalue → text |
| `.format_range_bound()` | format_serialize.R | Format numeric range bound for text |
| `.format_date_bound()` | format_serialize.R | Format Date/POSIXct bound for text |
| `.cntlout_to_ks_format()` | format_serialize.R | CNTLOUT → ks_format |
| `.cntlout_to_ks_invalue()` | format_serialize.R | CNTLOUT → ks_invalue |
| `.fput_value_type()` | format_apply.R | Apply format for value types (Date/POSIXct/logical) |
| `.value_types` | utilities.R | Constant: c("Date", "POSIXct", "logical") |
| `.is_value_type()` | utilities.R | Check if type string is a value type |
| `.typed_na()` | utilities.R | Return typed NA for given value type |
| `.typed_map_values()` | utilities.R | Extract typed vector from mapping list |
| `.parse_date_range_key()` | utilities.R | Parse Date range key string |
| `.typed_value_to_string()` | utilities.R | Convert typed value to display string |
| `.parse_typed_value()` | utilities.R | Parse string to typed value |
| `.block_to_value_type_format()` | format_parse.R | Block → value type format |

## Class Hierarchy
- `ks_format` — VALUE format (value→label). Fields: name, type, mappings, missing_label, other_label, multilabel, ignore_case, created. For datetime: dt_pattern, dt_toupper, sas_name. For value types: date_format. Type can be "character", "numeric", "Date", "POSIXct", "logical", or datetime types.
- `ks_invalue` — INVALUE format (label→value). Fields: name, target_type, mappings, missing_value, ignore_case, created.
- `range_spec` — Range specification. Fields: low, high, label, inc_low, inc_high.
