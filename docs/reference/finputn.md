# Apply Numeric Invalue by Name (like 'SAS' INPUTN)

Looks up a numeric INVALUE format by name from the global format library
and applies it to convert labels to numeric values.

## Usage

``` r
finputn(x, invalue_name)
```

## Arguments

- x:

  Character vector of labels to convert

- invalue_name:

  Character. Name of a registered INVALUE format.

## Value

Numeric vector

## Examples

``` r
# Create numeric invalue and apply
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
finputn(c("Male", "Female", "Male", "Unknown", "Female"), "sex_inv")
#> [1]  1  2  1 NA  2
# [1]  1  2  1 NA  2
fclear()
#> All formats cleared from library.

# Parse invalue from text and apply
fparse(text = '
INVALUE race_inv
  "White" = 1
  "Black" = 2
  "Asian" = 3
;
')
finputn(c("White", "Black"), "race_inv")
#> [1] 1 2
# [1] 1 2
fclear()
#> All formats cleared from library.
```
