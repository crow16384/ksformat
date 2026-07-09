# Apply Invalue Format (Reverse Formatting)

Applies an invalue format to convert formatted labels back to values.

## Usage

``` r
.invalue_apply(x, invalue, na_if = NULL)
```

## Arguments

- x:

  Character vector of labels to convert

- invalue:

  A `ks_invalue` object or a character string naming an invalue format
  in the global format library.

- na_if:

  Character vector. Additional values to treat as NA

## Value

Vector with values (type depends on invalue's `target_type`)
