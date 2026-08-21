# Retrieve a Format from the Library

Returns a format or invalue object by name. Used when you need the
object (e.g. for
[`fput_df`](https://crow16384.github.io/ksformat/reference/fput_df.md)
or
[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md))
rather than applying by name with
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md),
[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md), or
[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md).

## Usage

``` r
format_get(name)
```

## Arguments

- name:

  Character. Name of a registered format or invalue.

## Value

A `ks_format` or `ks_invalue` object.

## Examples

``` r
fnew("M" = "Male", "F" = "Female", name = "sex")
sex_fmt <- format_get("sex")
fput_df(data.frame(sex = c("M", "F")), sex = sex_fmt)
#>   sex sex_fmt
#> 1   M    Male
#> 2   F  Female
fclear()
#> All formats cleared from library.
```
