# Create a Key-Value Mapping for Format Creation

Convenience helper for building data-driven formats with
[`fnew`](fnew.md). Returns a named vector (or list) with class
`"ks_fmap"` that signals [`fnew()`](fnew.md) to use the natural
direction: names are **input keys**, values are **output
labels/objects** — regardless of the format type.

## Usage

``` r
fmap(keys, values)
```

## Arguments

- keys:

  Character vector of input keys (lookup values).

- values:

  Vector of output labels or objects (character, numeric, Date, POSIXct,
  logical, etc.).

## Value

A named vector (or list, for non-atomic values) with class
`c("ks_fmap", <original class>)`. Names are `keys`, values are `values`.

## Details

Without `fmap()`, [`fnew()`](fnew.md) reverses named vectors for
character and numeric types (the
[`factor()`](https://rdrr.io/r/base/factor.html) convention
`c(Label = "Code")`). Wrapping your data in `fmap()` suppresses this
reversal, so `fmap(keys, values)` works identically for character,
numeric, Date, POSIXct, and logical formats.

## See also

[`fnew`](fnew.md) for format creation.

## Examples

``` r
# Character lookup: keys -> labels
fmap(c("M", "F"), c("Male", "Female")) |> fnew(name = "sex")
fput(c("M", "F"), "sex")
#> [1] "Male"   "Female"
fclear()
#> All formats cleared from library.

# Date lookup from a data frame
ids   <- c("SUBJ-001", "SUBJ-002")
dates <- as.Date(c("2023-03-09", "2024-08-13"))
fmap(ids, dates) |> fnew(type = "Date", name = "icdtn")
fput("SUBJ-001", "icdtn")
#> [1] "2023-03-09"
fclear()
#> All formats cleared from library.
```
