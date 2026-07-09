# Apply Format and Return All Matches (Multilabel)

For multilabel formats, returns all matching labels for each input
value. Regular [`fput`](fput.md) returns only the first match; this
function returns all matches as a list of character vectors.

## Usage

``` r
fput_all(x, format, ..., keep_na = FALSE)
```

## Arguments

- x:

  Vector of values to format

- format:

  A `ks_format` object or a character string naming a format in the
  global format library.

- ...:

  Additional arguments for expression labels (mapped to `.x1`, `.x2`,
  etc.).

- keep_na:

  Logical. If TRUE, preserve NA in output.

## Value

A list of character vectors. Each element contains all matching labels
for the corresponding input value.

## Examples

``` r
# Basic multilabel: a value can match multiple labels
age_ml <- fnew(
  "0,5,TRUE,TRUE" = "Infant",
  "6,11,TRUE,TRUE" = "Child",
  "12,17,TRUE,TRUE" = "Teen",
  "0,17,TRUE,TRUE" = "Minor",
  "18,64,TRUE,TRUE" = "Adult",
  "65,Inf,TRUE,TRUE" = "Senior",
  name = "age_ml", type = "numeric", multilabel = TRUE
)

fput_all(c(3, 15, 25), age_ml)
#> [[1]]
#> [1] "Infant" "Minor" 
#> 
#> [[2]]
#> [1] "Minor" "Teen" 
#> 
#> [[3]]
#> [1] "Adult"
#> 
# [[1]] "Infant" "Minor"
# [[2]] "Teen" "Minor"
# [[3]] "Adult"

# Multilabel with .missing and .other
fnew(
  "0,100,TRUE,TRUE"  = "Valid Score",
  "0,49,TRUE,TRUE"   = "Below Average",
  "50,100,TRUE,TRUE" = "Above Average",
  "90,100,TRUE,TRUE" = "Excellent",
  .missing = "No Score",
  .other = "Out of Range",
  name = "score_ml", type = "numeric", multilabel = TRUE
)
fput_all(c(95, 45, NA, 150), "score_ml")
#> [[1]]
#> [1] "Valid Score"   "Above Average" "Excellent"    
#> 
#> [[2]]
#> [1] "Below Average" "Valid Score"  
#> 
#> [[3]]
#> [1] "No Score"
#> 
#> [[4]]
#> [1] "Out of Range"
#> 
# [[1]] "Valid Score" "Above Average" "Excellent"
# [[2]] "Valid Score" "Below Average"
# [[3]] "No Score"
# [[4]] "Out of Range"

# Parse multilabel from text
fparse(text = '
VALUE risk (numeric, multilabel)
  [0, 3]  = "Low Risk"
  [0, 7]  = "Monitored"
  (3, 7]  = "Medium Risk"
  (7, 10] = "High Risk"
;
')
fput_all(c(2, 5, 9), "risk")
#> [[1]]
#> [1] "Low Risk"  "Monitored"
#> 
#> [[2]]
#> [1] "Monitored"   "Medium Risk"
#> 
#> [[3]]
#> [1] "High Risk"
#> 
# [[1]] "Low Risk" "Monitored"
# [[2]] "Monitored" "Medium Risk"
# [[3]] "High Risk"
fclear()
#> All formats cleared from library.
```
