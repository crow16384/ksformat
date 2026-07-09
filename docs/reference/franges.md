# Extract Range Entries from a Format

Returns the range-based mappings of a `ks_format` object as a tidy data
frame. Discrete entries (plain values, `.missing`, `.other`) are
excluded.

## Usage

``` r
franges(fmt)
```

## Arguments

- fmt:

  A `ks_format` object, or a character name of a format registered in
  the global library.

## Value

A `data.frame` with columns `low`, `high`, `inc_low`, `inc_high`, and
`label`. Rows are returned in the order ranges appear in the format. If
the format has no range entries, an empty data frame with the same
columns is returned.

## Details

Range keys are stored internally as strings such as `"0,18,TRUE,FALSE"`.
`franges()` parses these keys back into their numeric bounds and
inclusivity flags, making it easy to inspect, filter, or
programmatically reuse range definitions.

Bounds parsed as `HIGH` or `LOW` appear as `Inf` and `-Inf`
respectively.

## Examples

``` r
fparse(text = '
VALUE age (numeric)
  [0, 18)    = "Child"
  [18, 65)   = "Adult"
  [65, HIGH] = "Senior"
  .missing   = "Unknown"
;
')
franges("age")
#>   low high inc_low inc_high  label
#> 1   0   18    TRUE    FALSE  Child
#> 2  18   65    TRUE    FALSE  Adult
#> 3  65  Inf    TRUE     TRUE Senior
fclear()
#> All formats cleared from library.
```
