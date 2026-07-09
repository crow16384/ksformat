# Export Formats to 'SAS'-like Text

Converts `ks_format` and/or `ks_invalue` objects to human-readable
'SAS'-like text representation.

## Usage

``` r
fexport(..., formats = NULL, file = NULL)
```

## Arguments

- ...:

  Named `ks_format` or `ks_invalue` objects to export.

- formats:

  A named list of format objects. Alternative to `...`.

- file:

  Optional file path to write the output to. If `NULL`, returns the text
  as a character string.

## Value

If `file` is `NULL`, returns a character string with the 'SAS'-like
text. If `file` is specified, writes to the file and returns the path
invisibly.

## Examples

``` r
# Export a character format
sex_fmt <- fnew("M" = "Male", "F" = "Female",
                .missing = "Unknown", name = "sex")
cat(fexport(sex = sex_fmt))
#> VALUE sex (character)
#>   "M" = "Male"
#>   "F" = "Female"
#>   .missing = "Unknown"
#> ;

# Export a numeric range format
fparse(text = '
VALUE bmi (numeric)
  [0, 18.5)  = "Underweight"
  [18.5, 25) = "Normal"
  [25, 30)   = "Overweight"
  [30, HIGH] = "Obese"
  .missing   = "No data"
;
')
bmi_fmt <- format_get("bmi")
cat(fexport(bmi = bmi_fmt))
#> VALUE bmi (numeric)
#>   [0, 18.5) = "Underweight"
#>   [18.5, 25) = "Normal"
#>   [25, 30) = "Overweight"
#>   [30, HIGH] = "Obese"
#>   .missing = "No data"
#> ;

# Export a multilabel format
risk_fmt <- fnew(
  "0,3,TRUE,TRUE" = "Low Risk",
  "0,7,TRUE,TRUE" = "Monitored",
  "3,7,FALSE,TRUE" = "Medium Risk",
  "7,10,FALSE,TRUE" = "High Risk",
  name = "risk", type = "numeric", multilabel = TRUE
)
cat(fexport(risk = risk_fmt))
#> VALUE risk (numeric, multilabel)
#>   [0, 3] = "Low Risk"
#>   [0, 7] = "Monitored"
#>   (3, 7] = "Medium Risk"
#>   (7, 10] = "High Risk"
#> ;

# Export a date format
enrl_fmt <- fnew_date("DATE9.", name = "enrldt", .missing = "Not Enrolled")
cat(fexport(enrldt = enrl_fmt))
#> VALUE enrldt (date)
#>   pattern = "DATE9."
#>   .missing = "Not Enrolled"
#> ;
fclear()
#> All formats cleared from library.
```
