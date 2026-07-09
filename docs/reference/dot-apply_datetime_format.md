# Apply a date/time format to a vector

Handles Date, POSIXct, numeric (R epoch), and character inputs.

## Usage

``` r
.apply_datetime_format(x, format, keep_na = FALSE)
```

## Arguments

- x:

  Input vector

- format:

  A ks_format object with date/time/datetime type

- keep_na:

  Logical. Preserve NA instead of applying missing label.

## Value

Character vector of formatted values
