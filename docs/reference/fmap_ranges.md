# Build a Vector of Range Mappings

Construct a `ks_fmap`-classed named character vector whose names encode
numeric / Date / POSIXct range bounds and whose values are the
corresponding labels. The result is intended to be passed to
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md) as a
single positional argument (it suppresses the default name reversal).

## Usage

``` r
fmap_ranges(
  low,
  high,
  label,
  inc_low = TRUE,
  inc_high = FALSE,
  date_format = NULL
)
```

## Arguments

- low, high:

  Numeric, `Date`, or `POSIXct` vectors of equal length giving the lower
  / upper bounds of each range.

- label:

  Character vector of labels (same length as `low`).

- inc_low, inc_high:

  Logical, length 1 or `length(low)`. Whether each bound is inclusive.
  Defaults match
  [`range_spec`](https://crow16384.github.io/ksformat/reference/range_spec.md):
  `[low, high)`.

- date_format:

  Optional strptime format string used when formatting `Date`/`POSIXct`
  bounds into key strings.

## Value

A `ks_fmap` object (named character vector) suitable for passing to
[`fnew()`](https://crow16384.github.io/ksformat/reference/fnew.md).

## Details

Bounds are formatted as ISO 8601: `"%Y-%m-%d"` for Date,
`"%Y-%m-%d %H:%M:%S"` (UTC) for POSIXct. Override with `date_format` if
needed.

## See also

[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md),
[`fmap_strata`](https://crow16384.github.io/ksformat/reference/fmap_strata.md),
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)

## Examples

``` r
rng <- fmap_ranges(
  low   = c(0, 18, 65),
  high  = c(18, 65, Inf),
  label = c("Child", "Adult", "Senior"),
  inc_high = c(FALSE, FALSE, TRUE)
)
fnew(rng, type = "numeric", name = "age_groups")
fput(c(5, 25, 90), "age_groups")
#> [1] "Child"  "Adult"  "Senior"
fclear()
#> All formats cleared from library.
```
