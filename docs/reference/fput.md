# Apply Format to Data (like 'SAS' PUT function)

Applies a format definition to a vector of values, returning formatted
labels. Properly handles NA, NULL, NaN, and other missing values.

## Usage

``` r
fput(x, format, ..., keep_na = FALSE)
```

## Arguments

- x:

  Vector of values to format

- format:

  A `ks_format` object or a character string naming a format in the
  global format library.

- ...:

  Additional arguments for expression labels. Positional arguments are
  mapped to `.x1`, `.x2`, etc. inside expression labels. Can be vectors
  of the same length as `x` or scalars (recycled).

- keep_na:

  Logical. If TRUE, preserve NA in output instead of applying missing
  label.

## Value

Character vector with formatted labels

## Details

The function handles missing values in the following order:

1.  NA, NULL, NaN -\> Uses format's missing_label if defined

2.  Exact matches -\> Uses defined value-label mapping

3.  Range matches (for numeric) -\> Uses range label

4.  No match -\> Uses format's other_label or returns original value

**Expression labels:** If a label string contains `.x1`, `.x2`, etc., it
is evaluated as an R expression at apply-time. Extra data is passed as
positional arguments:

    stat_fmt <- fnew("n" = "sprintf('%s', .x1)",
                     "pct" = "sprintf('%.1f%%', .x1 * 100)")
    fput(c("n", "pct"), stat_fmt, c(42, 0.15))
    # Returns: "42" "15.0%"

**Numeric patterns:** If a numeric format was created with a single
unnamed pattern string (for example `fnew("$%,.2f", type = "numeric")`),
`fput`/`fputn` apply that pattern to numeric values directly, including
grouping marks and literal prefix/suffix text.

**Case-insensitive matching:** When a format has `ignore_case = TRUE`,
key matching is case-insensitive for character formats.

## Examples

``` r
# Basic discrete formatting
fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")
fput(c("M", "F", NA, "X"), "sex")
#> [1] "Male"    "Female"  "Unknown" "X"      
# [1] "Male" "Female" "Unknown" "X"

# Preserve NA instead of applying missing label
sex_f <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
fput(c("M", "F", NA), sex_f, keep_na = TRUE)
#> [1] "Male"   "Female" NA      
# [1] "Male" "Female" NA

# Numeric range formatting
fparse(text = '
VALUE score (numeric)
  (0, 50]  = "Low"
  (50, 100] = "High"
  .other   = "Out of range"
;
')
fput(c(0, 1, 50, 51, 100, 101), "score")
#> [1] "Out of range" "Low"          "Low"          "High"         "High"        
#> [6] "Out of range"
# [1] "Out of range" "Low" "Low" "High" "High" "Out of range"
fclear()
#> All formats cleared from library.
```
