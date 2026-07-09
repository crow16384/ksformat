# Apply Format to Data Frame Columns

Applies formats to one or more columns in a data frame.

## Usage

``` r
fput_df(data, ..., suffix = "_fmt", replace = FALSE)
```

## Arguments

- data:

  Data frame

- ...:

  Named format specifications: `column_name = format_object_or_name`

- suffix:

  Character. Suffix to add to formatted column names (default: "\_fmt")

- replace:

  Logical. If TRUE, replace original columns; if FALSE, create new
  columns

## Value

Data frame with formatted columns

## Examples

``` r
# Apply formats to multiple columns
df <- data.frame(
  id = 1:6,
  sex = c("M", "F", "M", "F", NA, "X"),
  age = c(15, 25, 45, 70, 35, NA),
  stringsAsFactors = FALSE
)

sex_f <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
fparse(text = '
VALUE age (numeric)
  [0, 18)   = "Child"
  [18, 65)  = "Adult"
  [65, HIGH] = "Senior"
  .missing  = "Age Unknown"
;
')
age_f <- format_get("age")

fput_df(df, sex = sex_f, age = age_f, suffix = "_label")
#>   id  sex age sex_label   age_label
#> 1  1    M  15      Male       Child
#> 2  2    F  25    Female       Adult
#> 3  3    M  45      Male       Adult
#> 4  4    F  70    Female      Senior
#> 5  5 <NA>  35   Unknown       Adult
#> 6  6    X  NA         X Age Unknown

# Date formatting in data frames
patients <- data.frame(
  id = 1:4,
  visit_date = as.Date(c("2025-01-10", "2025-02-15", "2025-03-20", NA)),
  stringsAsFactors = FALSE
)
visit_fmt <- fnew_date("DATE9.", name = "visit_fmt", .missing = "NOT RECORDED")
fput_df(patients, visit_date = visit_fmt)
#>   id visit_date visit_date_fmt
#> 1  1 2025-01-10      10JAN2025
#> 2  2 2025-02-15      15FEB2025
#> 3  3 2025-03-20      20MAR2025
#> 4  4       <NA>   NOT RECORDED
fclear()
#> All formats cleared from library.
```
