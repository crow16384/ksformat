# Reverse-Lookup Range Bounds from Labels

Given a vector of values that match the *labels* of a range-based
format, returns the corresponding `low` / `high` bounds (and inclusivity
flags) for each input. Useful for reconstructing the underlying range
from a coded value.

## Usage

``` r
fmap_to_ranges(x, fmt)
```

## Arguments

- x:

  A vector of values to look up against the format's labels. Coerced to
  character before matching.

- fmt:

  A `ks_format` object, or a character name of a format registered in
  the global library.

## Value

A `data.frame` with one row per element of `x` and columns `low`,
`high`, `inc_low`, `inc_high`. Rows where the input does not match any
range label contain `NA`.

## Details

For `multilabel` formats where the same label maps to several ranges,
only the *first* matching range is returned. For full multi-match
behaviour, call [`franges()`](franges.md) directly and join on `label`.

## See also

[`franges`](franges.md)

## Examples

``` r
fparse(text = '
VALUE visit_ther (numeric)
  [LOW,  1] =  0
  [ 8, 22] =  2
  [22, 36] =  4
  [37, 50] =  6
;
')
fmap_to_ranges(c(0, 2, 4, 6), "visit_ther")
#>    low high inc_low inc_high
#> 1 -Inf    1    TRUE     TRUE
#> 2    8   22    TRUE     TRUE
#> 3   22   36    TRUE     TRUE
#> 4   37   50    TRUE     TRUE
fclear()
#> All formats cleared from library.
```
