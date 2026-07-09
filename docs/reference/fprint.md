# Print Format(s) from Library

Displays format information from the global format library. When called
without arguments, lists all registered format names. When called with a
name, displays the full definition of that format.

## Usage

``` r
fprint(name = NULL)
```

## Arguments

- name:

  Character. Optional name of a specific format to display. If `NULL`
  (default), lists all registered formats.

## Value

Invisible `NULL`. This function is for display only.

## See also

[`flist`](flist.md) for a programmatic alternative that returns a
character vector of registered names.

## Examples

``` r
fnew("M" = "Male", "F" = "Female", name = "sex")
flist()          # character vector of names
#> [1] "sex"
fprint()         # list all formats
#> Registered formats:
#>   sex - VALUE (character), 2 mapping(s)
fprint("sex")    # show specific format
#> KS Format:sex
#> Type: character 
#> Mappings:
#>   M => Male
#>   F => Female
fclear()
#> All formats cleared from library.
```
