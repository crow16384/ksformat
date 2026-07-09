# Apply Character Invalue by Name (like 'SAS' INPUTC)

Looks up an INVALUE format by name from the global format library and
applies it to convert labels to character values.

## Usage

``` r
finputc(x, invalue_name)
```

## Arguments

- x:

  Character vector of labels to convert

- invalue_name:

  Character. Name of a registered INVALUE format.

## Value

Character vector

## Examples

``` r
# Bidirectional: use finputc for reverse direction
fnew_bid(
  "A" = "Active",
  "I" = "Inactive",
  "P" = "Pending",
  name = "status"
)
#> $format
#> KS Format:status
#> Type: character 
#> Mappings:
#>   A => Active
#>   I => Inactive
#>   P => Pending
#> 
#> $invalue
#> KS Invalue: status_inv
#> Target Type: character 
#> Mappings:
#>   Active => A
#>   Inactive => I
#>   Pending => P
#> 

# Forward: code -> label
fputc(c("A", "I", "P"), "status")
#> [1] "Active"   "Inactive" "Pending" 
# [1] "Active" "Inactive" "Pending"

# Reverse: label -> code
finputc(c("Active", "Pending", "Inactive"), "status_inv")
#> [1] "A" "P" "I"
# [1] "A" "P" "I"
fclear()
#> All formats cleared from library.
```
