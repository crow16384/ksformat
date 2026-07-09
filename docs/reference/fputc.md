# Apply Character Format by Name (like 'SAS' PUTC)

Looks up a character VALUE format by name from the global format library
and applies it to the input vector.

## Usage

``` r
fputc(x, format_name, ...)
```

## Arguments

- x:

  Character vector of values to format

- format_name:

  Character. Name of a registered character format, or a character
  vector of format names (same length as `x`) to apply a different
  format per element (like 'SAS' PUTC with a variable format).

- ...:

  Additional arguments passed to [`fput`](fput.md) for expression labels
  (mapped to `.x1`, `.x2`, etc.).

## Value

Character vector with formatted labels

## Examples

``` r
# Apply character format by name
fnew("M" = "Male", "F" = "Female", name = "sex")
fputc(c("M", "F"), "sex")
#> [1] "Male"   "Female"
# [1] "Male" "Female"

# Bidirectional: forward direction
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
fputc(c("A", "I", "P", "A"), "status")
#> [1] "Active"   "Inactive" "Pending"  "Active"  
# [1] "Active" "Inactive" "Pending" "Active"
fclear()
#> All formats cleared from library.
```
