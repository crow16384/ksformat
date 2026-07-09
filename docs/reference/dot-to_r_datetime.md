# Convert input to R POSIXct

Handles POSIXct, Date, numeric (seconds since 1970-01-01), and character
inputs.

## Usage

``` r
.to_r_datetime(x, origin = "1970-01-01")
```

## Arguments

- x:

  Input vector

- origin:

  Character. Origin date for numeric conversion. Always `"1970-01-01"`
  (R epoch).

## Value

POSIXct vector
