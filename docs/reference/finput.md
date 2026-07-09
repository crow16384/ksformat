# Create Invalue Format (Reverse Formatting like 'SAS' INVALUE)

Creates an invalue format that converts formatted labels back to values.
This is similar to 'SAS' PROC FORMAT with INVALUE statement. The invalue
is automatically stored in the global format library if `name` is
provided.

## Usage

``` r
finput(
  ...,
  name = NULL,
  target_type = "numeric",
  missing_value = NA,
  ignore_case = FALSE
)
```

## Arguments

- ...:

  Named arguments defining label-value mappings (reverse of
  [`fnew`](fnew.md)), or one or more named vectors/lists using
  `c(Label = value)`. Example: `"Male" = 1, "Female" = 2` or
  `c(Male = 1, Female = 2)`.

- name:

  Character. Optional name for the invalue format. If provided, the
  invalue is automatically registered in the global format library.

- target_type:

  Character. Type to convert to: `"numeric"` (default), `"integer"`,
  `"character"`, or `"logical"`. INVALUE formats produce numeric output
  by default; character-to-character conversion should use a regular
  VALUE format ([`fnew`](fnew.md)) instead.

- missing_value:

  Value to use for missing inputs (default: `NA`)

- ignore_case:

  Logical. If `TRUE`, label matching is case-insensitive (default:
  `FALSE`).

## Value

An object of class `"ks_invalue"` containing the invalue definition. The
object is also stored in the format library if `name` is given.

## Examples

``` r
# Convert text labels to numeric codes
finput(
  "Male" = 1,
  "Female" = 2,
  name = "sex_inv"
)
#> KS Invalue: sex_inv
#> Target Type: numeric 
#> Mappings:
#>   Male => 1
#>   Female => 2

# Apply using finputn (numeric invalue by name)
finputn(c("Male", "Female", "Unknown"), "sex_inv")
#> [1]  1  2 NA
# [1]  1  2 NA
fclear()
#> All formats cleared from library.

# From a named vector
finput(c(Male = 1, Female = 2), name = "sex_inv2")
#> KS Invalue: sex_inv2
#> Target Type: numeric 
#> Mappings:
#>   Male => 1
#>   Female => 2
finputn(c("Male", "Female"), "sex_inv2")
#> [1] 1 2
# [1] 1 2
fclear()
#> All formats cleared from library.
```
