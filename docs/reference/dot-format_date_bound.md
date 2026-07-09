# Format a Date / POSIXct range bound for text output

Renders infinite bounds as `LOW`/`HIGH`; otherwise formats the
Date/POSIXct value using `date_format` when supplied, else ISO 8601.
Used for value-type (Date/POSIXct) formats and the date_range /
datetime_range types.

## Usage

``` r
.format_date_bound(val, date_format = NULL, type = NULL, is_low = TRUE)
```
