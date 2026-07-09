# Parse Format Definitions from 'SAS'-like Text

Reads format definitions written in a human-friendly 'SAS'-like syntax
and returns a list of `ks_format` and/or `ks_invalue` objects. All
parsed formats are automatically stored in the global format library.

## Usage

``` r
fparse(text = NULL, file = NULL, verbose = FALSE)
```

## Arguments

- text:

  Character string or character vector containing format definitions. If
  a character vector, lines are concatenated with newlines.

- file:

  Path to a text file containing format definitions. Exactly one of
  `text` or `file` must be provided.

- verbose:

  Logical. If `TRUE`, the parsed formats are printed to the console.
  Default is `FALSE` to suppress output (the result is returned
  invisibly).

## Value

A named list of `ks_format` and/or `ks_invalue` objects. Names
correspond to the format names defined in the text. All formats are
automatically registered in the global format library.

## Details

The syntax supports two block types:

**VALUE** blocks define formats (value -\> label):

    VALUE name (type)
      "value1" = "Label 1"
      "value2" = "Label 2"
      [low, high) = "Range Label (half-open)"
      (low, high] = "Range Label (open-low, closed-high)"
      pattern = "..."  # date/time/datetime patterns, or numeric display patterns
      .missing = "Missing Label"
      .other = "Other Label"
    ;

**INVALUE** blocks define reverse formats (label -\> numeric value):

    INVALUE name
      "Label 1" = 1
      "Label 2" = 2
    ;

**Syntax rules:**

- Blocks start with `VALUE` or `INVALUE` keyword and end with `;`

- The type in parentheses is optional; defaults to `"auto"` for VALUE,
  `"numeric"` for INVALUE \item Use `pattern = "..."` for
  date/time/datetime display patterns and `pattern: "..."` in the block
  header for numeric display patterns

- Values can be quoted or unquoted

- Ranges use interval notation with explicit bounds

- Legacy range syntax `low - high` is also supported

- Special range keywords: `LOW` (-Inf) and `HIGH` (Inf)

- `.missing` and `.other` are special directives

- Lines starting with `/*`, `*`, `//`, or `#` are comments

**Block options:**

Comma-separated options can be placed inside the parentheses after the
type:

- `nocase` — enables case-insensitive key matching (equivalent to
  `ignore_case = TRUE` in [`fnew`](fnew.md)).

- `multilabel` — allows overlapping ranges where a single value matches
  multiple labels (used with [`fput_all`](fput_all.md)).

Options can be combined: `VALUE name (character, nocase, multilabel)`.

## Examples

``` r
# Parse multiple format definitions from text
fparse(text = '
VALUE sex (character)
  "M" = "Male"
  "F" = "Female"
  .missing = "Unknown"
;

VALUE age (numeric)
  [0, 18)    = "Child"
  [18, 65)   = "Adult"
  [65, HIGH]  = "Senior"
  .missing   = "Age Unknown"
;

// Invalue block
INVALUE race_inv
  "White" = 1
  "Black" = 2
  "Asian" = 3
;
')

fput(c("M", "F", NA), "sex")
#> [1] "Male"    "Female"  "Unknown"
fputn(c(5, 25, 70, NA), "age")
#> [1] "Child"       "Adult"       "Senior"      "Age Unknown"
finputn(c("White", "Black"), "race_inv")
#> [1] 1 2
flist()
#> [1] "age"      "race_inv" "sex"     
fprint()
#> Registered formats:
#>   age - VALUE (numeric), 3 mapping(s)
#>   race_inv - INVALUE (numeric), 3 mapping(s)
#>   sex - VALUE (character), 2 mapping(s)
fclear()
#> All formats cleared from library.

# Parse date/time/datetime format definitions
fparse(text = '
VALUE enrldt (date)
  pattern = "DATE9."
  .missing = "Not Enrolled"
;

VALUE visit_time (time)
  pattern = "TIME8."
;

VALUE stamp (datetime)
  pattern = "DATETIME20."
;
')

fput(as.Date("2025-03-01"), "enrldt")
#> [1] "01MAR2025"
fput(36000, "visit_time")
#> [1] "10:00:00"
fput(as.POSIXct("2025-03-01 10:00:00", tz = "UTC"), "stamp")
#> [1] "01MAR2025:10:00:00"
fclear()
#> All formats cleared from library.

# Numeric display pattern format
fparse(text = '
VALUE currency (numeric, pattern: "$%,.2f")
  .missing = "NO DATA"
;
')
fputn(c(1234.56, -7890.12, NA), "currency")
#> [1] "$1,234.56"  "-$7,890.12" "NO DATA"   
fclear()
#> All formats cleared from library.

# Case-insensitive format (nocase option)
fparse(text = '
VALUE yesno (character, nocase)
  "Y" = "Yes"
  "N" = "No"
  .other = "Unknown"
;
')
fput(c("y", "N", "YES"), "yesno")
#> [1] "Yes"     "No"      "Unknown"
# [1] "Yes" "No" "Unknown"
fclear()
#> All formats cleared from library.

# Parse multilabel format
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
fclear()
#> All formats cleared from library.
```
