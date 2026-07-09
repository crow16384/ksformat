# Convert input to R Date

Handles Date, POSIXct, numeric (days since 1970-01-01), and character
inputs.

## Usage

``` r
.to_r_date(x, origin = "1970-01-01")
```

## Arguments

- x:

  Input vector

- origin:

  Character. Origin date for numeric conversion. Always `"1970-01-01"`
  (R epoch).

## Value

Date vector
