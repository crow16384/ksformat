# List Format Names from Library

Returns a character vector of all format and invalue names currently
registered in the global format library.

## Usage

``` r
flist()
```

## Value

A character vector of registered format names, sorted alphabetically.
Returns `character(0)` if the library is empty.

## Examples

``` r
fnew("M" = "Male", "F" = "Female", name = "sex")
flist()
#> [1] "sex"
fclear()
#> All formats cleared from library.
```
