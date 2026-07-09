# Create Bidirectional Format

Creates both a format and its corresponding invalue for bidirectional
conversion. Both are automatically stored in the global format library
if `name` is provided.

## Usage

``` r
fnew_bid(..., name = NULL, type = "auto")
```

## Arguments

- ...:

  Named arguments for format mappings

- name:

  Character. Base name for both formats. The invalue will be named
  `paste0(name, "_inv")`.

- type:

  Character. Format type

## Value

List with `format` (ks_format) and `invalue` (ks_invalue) components.

## Examples

``` r
# Bidirectional status format
status_bi <- fnew_bid(
  "A" = "Active",
  "I" = "Inactive",
  "P" = "Pending",
  name = "status"
)

# Forward: code -> label
fputc(c("A", "I", "P", "A"), "status")
#> [1] "Active"   "Inactive" "Pending"  "Active"  
# [1] "Active" "Inactive" "Pending" "Active"

# Reverse: label -> code
finputc(c("Active", "Pending", "Inactive"), "status_inv")
#> [1] "A" "P" "I"
# [1] "A" "P" "I"
fclear()
#> All formats cleared from library.

# From a named vector (Label = Code convention, same as fnew)
fnew_bid(c(Male = "M", Female = "F"), name = "sex_bid")
#> $format
#> KS Format:sex_bid
#> Type: character 
#> Mappings:
#>   M => Male
#>   F => Female
#> 
#> $invalue
#> KS Invalue: sex_bid_inv
#> Target Type: character 
#> Mappings:
#>   Male => M
#>   Female => F
#> 
fputc(c("M", "F"), "sex_bid")
#> [1] "Male"   "Female"
finputc(c("Male", "Female"), "sex_bid_inv")
#> [1] "M" "F"
fclear()
#> All formats cleared from library.
```
