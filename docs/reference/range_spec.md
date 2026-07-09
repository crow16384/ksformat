# Create Range Specification

Helper function to create range specifications for numeric formats.

## Usage

``` r
range_spec(low, high, label, inc_low = TRUE, inc_high = FALSE)
```

## Arguments

- low:

  Numeric. Lower bound of the range.

- high:

  Numeric. Upper bound of the range.

- label:

  Character. Label for values in this range.

- inc_low:

  Logical. If `TRUE` (default), the lower bound is inclusive (\>=). If
  `FALSE`, exclusive (\>).

- inc_high:

  Logical. If `TRUE`, the upper bound is inclusive (\<=). If `FALSE`
  (default), exclusive (\<).

## Value

A `range_spec` object (list with low, high, label, inc_low, inc_high).

## Details

By default, ranges are half-open: `[low, high)` — the lower bound is
included and the upper bound is excluded. This matches 'SAS' PROC FORMAT
range semantics and prevents overlap between adjacent ranges.

## Examples

``` r
range_spec(0, 18, "Child")          # [0, 18)
#> $low
#> [1] 0
#> 
#> $high
#> [1] 18
#> 
#> $label
#> [1] "Child"
#> 
#> $inc_low
#> [1] TRUE
#> 
#> $inc_high
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "range_spec"
range_spec(18, 65, "Adult")         # [18, 65)
#> $low
#> [1] 18
#> 
#> $high
#> [1] 65
#> 
#> $label
#> [1] "Adult"
#> 
#> $inc_low
#> [1] TRUE
#> 
#> $inc_high
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "range_spec"
range_spec(65, Inf, "Senior", inc_high = TRUE)  # [65, Inf]
#> $low
#> [1] 65
#> 
#> $high
#> [1] Inf
#> 
#> $label
#> [1] "Senior"
#> 
#> $inc_low
#> [1] TRUE
#> 
#> $inc_high
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "range_spec"
```
