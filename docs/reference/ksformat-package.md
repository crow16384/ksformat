# ksformat: 'SAS'-Style 'PROC FORMAT' for R

Provides 'SAS' 'PROC FORMAT'-like functionality for creating and
applying value formats in R. The package supports mapping values to
labels, range-based formatting, reverse formatting (invalue),
date/time/datetime formatting, and proper handling of missing values
(NA, NULL, NaN).

## Details

**Format creation:**

- [`fnew`](fnew.md) — create value-to-label mappings (formats)

- [`finput`](finput.md) — create reverse mappings (label-to-value
  invalues)

- [`fnew_bid`](fnew_bid.md) — create both format and invalue
  simultaneously

- [`fnew_date`](fnew_date.md) — create date/time/datetime formats
  ('SAS'-style or custom `strftime` patterns)

- [`fparse`](fparse.md) — parse 'SAS'-like format definitions from text
  or file

- [`fimport`](fimport.md) — import formats from a 'SAS' CNTLOUT CSV file

- [`e`](e.md) — mark a label for expression evaluation at apply-time

**Format application:**

- [`fput`](fput.md) — apply a format to a vector (value to label)

- [`fputn`](fputn.md) — apply a numeric format by name (like 'SAS' PUTN)

- [`fputc`](fputc.md) — apply a character format by name (like 'SAS'
  PUTC)

- [`fput_all`](fput_all.md) — apply a multilabel format returning all
  matching labels

- [`fput_df`](fput_df.md) — apply formats to data frame columns

**Reverse formatting:**

- [`finputn`](finputn.md) — apply a numeric invalue by name (like 'SAS'
  INPUTN)

- [`finputc`](finputc.md) — apply a character invalue by name (like
  'SAS' INPUTC)

**Format library:**

- [`format_get`](format_get.md) — retrieve a format from the global
  library

- [`fprint`](fprint.md) — list or display registered formats

- [`fclear`](fclear.md) — remove one or all formats from the library

- [`format_library_app`](format_library_app.md) — open interactive
  library browser (Shiny)

- [`fexport`](fexport.md) — export formats to 'SAS'-like text

**Utilities:**

- [`is_missing`](is_missing.md) — check for NA, NaN, and empty strings

- [`range_spec`](range_spec.md) — create a range specification object

**Key features:**

- *Discrete and range-based* numeric formatting with configurable
  inclusive/exclusive bounds

- *Multilabel* formats — a value can match multiple labels
  (`multilabel = TRUE` in [`fnew`](fnew.md), retrieved with
  [`fput_all`](fput_all.md))

- *Case-insensitive matching* (`ignore_case = TRUE` in
  [`fnew`](fnew.md))

- *Expression labels* — labels containing `.x1`, `.x2`, etc. are
  evaluated at apply-time; see also [`e`](e.md)

- *Date/time/datetime* formatting with built-in 'SAS' format names
  (auto-resolved) or custom `strftime` patterns

- *Global format library* with auto-registration and case-insensitive
  name lookup

- *CNTLOUT import* — read format catalogues exported from 'SAS'

Cheat sheet: run [`ksformat_cheatsheet()`](ksformat_cheatsheet.md) to
open the HTML version in your browser, or see the files in
`system.file("doc", package = "ksformat")`.

## See also

Source repository and issue tracker:
<https://github.com/crow16384/ksformat>

## Author

**Maintainer**: Vladimir Larchenko
<vladimir.larchenko@keystatsolutions.com>

Authors:

- Vladimir Larchenko <vladimir.larchenko@keystatsolutions.com>

- Igor Aleschenkov <igor.aleschenkov@keystatsolutions.com>

## Examples

``` r
# Discrete format
fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")
fput(c("M", "F", NA), "sex")
#> [1] "Male"    "Female"  "Unknown"

# Numeric range format (parsed from text)
fparse(text = '
VALUE age (numeric)
  [0, 18)  = "Child"
  [18, 65) = "Adult"
;
')
fputn(c(5, 25), "age")
#> [1] "Child" "Adult"

# Bidirectional format + invalue
fnew_bid("A" = "Active", "I" = "Inactive", name = "status")
#> $format
#> KS Format:status
#> Type: character 
#> Mappings:
#>   A => Active
#>   I => Inactive
#> 
#> $invalue
#> KS Invalue: status_inv
#> Target Type: character 
#> Mappings:
#>   Active => A
#>   Inactive => I
#> 
fputc("A", "status")
#> [1] "Active"
finputc("Active", "status_inv")
#> [1] "A"

# Multilabel format
ml <- fnew(
  "0,17,TRUE,TRUE" = "Pediatric",
  "18,Inf,TRUE,TRUE" = "Adult",
  "0,Inf,TRUE,TRUE" = "Any Age",
  name = "agegrp", type = "numeric", multilabel = TRUE
)
fput_all(c(10, 30), ml)
#> [[1]]
#> [1] "Pediatric" "Any Age"  
#> 
#> [[2]]
#> [1] "Any Age" "Adult"  
#> 

# Date format (SAS-style, auto-resolved)
fputn(Sys.Date(), "DATE9.")
#> [1] "09JUL2026"

# Export and library management
cat(fexport(sex = format_get("sex")))
#> VALUE sex (character)
#>   "M" = "Male"
#>   "F" = "Female"
#>   .missing = "Unknown"
#> ;
flist()           # character vector of registered names
#> [1] "DATE9."     "age"        "agegrp"     "sex"        "status"    
#> [6] "status_inv"
fprint()
#> Registered formats:
#>   DATE9. - VALUE (date, DATE9.), 0 mapping(s)
#>   age - VALUE (numeric), 2 mapping(s)
#>   agegrp - VALUE (numeric), 3 mapping(s)
#>   sex - VALUE (character), 2 mapping(s)
#>   status - VALUE (character), 2 mapping(s)
#>   status_inv - INVALUE (character), 2 mapping(s)
fclear()
#> All formats cleared from library.
```
