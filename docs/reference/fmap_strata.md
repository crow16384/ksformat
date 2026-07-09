# Build a Vector of Stratified Range Mappings

Companion to [`fmap_ranges`](fmap_ranges.md) for the `stratified_range`
format type. Each row pairs a stratum (e.g. study arm, subject id, or a
composite key produced by [`fputk()`](fputk.md)) with a numeric / Date /
POSIXct range and a label. The returned `ks_fmap` vector carries the
chosen `sep` as an attribute so that
[`fnew`](fnew.md)`(type = "stratified_range")` picks it up
automatically.

## Usage

``` r
fmap_strata(
  stratum,
  low,
  high,
  label,
  inc_low = TRUE,
  inc_high = FALSE,
  sep = "|",
  date_format = NULL
)
```

## Arguments

- stratum:

  Character vector of stratum identifiers.

- low, high:

  Range bounds. See [`fmap_ranges`](fmap_ranges.md).

- label:

  Character vector of labels.

- inc_low, inc_high:

  Logical, length 1 or `length(low)`. See
  [`fmap_ranges`](fmap_ranges.md).

- sep:

  Separator inserted between stratum and range key. Must match the `sep`
  subsequently passed to [`fputk`](fputk.md).

- date_format:

  Optional strptime format string.

## Value

A `ks_fmap` object with an attached `"strata_sep"` attribute.

## See also

[`fmap_ranges`](fmap_ranges.md), [`fputk`](fputk.md), [`fnew`](fnew.md)

## Examples

``` r
visits <- fmap_strata(
  stratum  = c("ARM_A", "ARM_A", "ARM_B"),
  low      = c(0, 7, 0),
  high     = c(7, 14, 10),
  label    = c("Baseline", "Week 1", "Baseline")
)
fnew(visits, type = "stratified_range", range_subtype = "numeric",
     name = "visit_window")
fputk(c("ARM_A", "ARM_B"), c(3, 5), format = "visit_window")
#> [1] "Baseline" "Baseline"
fclear()
#> All formats cleared from library.
```
