# Extract Value/Label Levels from a Format

Returns the mappings of a `ks_format` object as a tidy two-column data
frame with `value` and `label`.

## Usage

``` r
flevels(fmt)
```

## Arguments

- fmt:

  A `ks_format` object, or a character name of a format registered in
  the global library.

## Value

A `data.frame` with columns `value` and `label`. Rows are returned in
the order entries appear in the format. If the format has no mappings,
an empty data frame with the same columns is returned.

## Details

For range-based formats, `value` contains the internal range key
representation (for example, `"0,18,TRUE,FALSE"`). Use
[`franges()`](franges.md) to retrieve parsed range bounds.

## Examples

``` r
fmt <- fnew(
  "PBO" = "Placebo",
  "D50" = "Drug A 50 mg",
  "D100" = "Drug A 100 mg"
)
flevels(fmt)
#>   value         label
#> 1   PBO       Placebo
#> 2   D50  Drug A 50 mg
#> 3  D100 Drug A 100 mg
```
