# Apply Numeric Format by Name (like 'SAS' PUTN)

Looks up a numeric VALUE format by name from the global format library
and applies it to the input vector.

## Usage

``` r
fputn(x, format_name, ...)
```

## Arguments

- x:

  Numeric vector of values to format

- format_name:

  Character. Name of a registered numeric format, or a character vector
  of format names (same length as `x`) to apply a different format per
  element (like 'SAS' PUTN with a variable format).

- ...:

  Additional arguments passed to [`fput`](fput.md) for expression labels
  (mapped to `.x1`, `.x2`, etc.).

## Value

Character vector with formatted labels

## Examples

``` r
# Numeric range formatting
fparse(text = '
VALUE age (numeric)
  [0, 18)    = "Child"
  [18, 65)   = "Adult"
  [65, HIGH]  = "Senior"
  .missing   = "Age Unknown"
;
')
fputn(c(5, 25, 70, NA), "age")
#> [1] "Child"       "Adult"       "Senior"      "Age Unknown"
# [1] "Child" "Adult" "Senior" "Age Unknown"

# SAS date format (auto-resolved, no pre-creation needed)
fputn(as.Date("2025-01-15"), "DATE9.")
#> [1] "15JAN2025"
# [1] "15JAN2025"

# Time format (seconds since midnight)
fputn(c(0, 3600, 45000), "TIME8.")
#> [1] "0:00:00"  "1:00:00"  "12:30:00"
# [1] "00:00:00" "01:00:00" "12:30:00"
fclear()
#> All formats cleared from library.
```
