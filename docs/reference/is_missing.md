# Check if Value is Missing

Element-wise check for missing values including NA and NaN. Optionally
treats empty strings as missing.

## Usage

``` r
is_missing(x)
```

## Arguments

- x:

  Value to check

## Value

Logical vector. NULL input returns `logical(0)`.

## Examples

``` r
is_missing(NA)          # TRUE
#> [1] TRUE
is_missing(NaN)         # TRUE
#> [1] TRUE
is_missing("")          # TRUE
#> [1] TRUE
is_missing("text")      # FALSE
#> [1] FALSE
is_missing(c(1, NA, NaN)) # FALSE TRUE TRUE
#> [1] FALSE  TRUE  TRUE
```
