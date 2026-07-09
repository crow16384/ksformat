# Remove Format(s) from Library

Removes one or all formats from the global format library. When called
without arguments, clears all formats. When called with a name, removes
only that format.

## Usage

``` r
fclear(name = NULL)
```

## Arguments

- name:

  Character. Optional name of a specific format to remove. If `NULL`
  (default), removes all formats.

## Value

Invisible `NULL`

## Examples

``` r
fnew("M" = "Male", "F" = "Female", name = "sex")
fclear("sex")   # remove one format
#> Format "sex" removed from library.
fclear()        # remove all formats
#> All formats cleared from library.
```
