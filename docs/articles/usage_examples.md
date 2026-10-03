# ksformat Usage Examples

![ksformat logo](figures/ksformat-logo-hero.gif)

The **ksformat** package provides SAS PROC FORMAT-like functionality for
R. This vignette walks through the most common use cases.

## Example 1: Basic Discrete Formatting

Create a format for gender codes (auto-stored in library as “sex”):

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"M"`` ``=`` ``"Male"``,`\
`  ``"F"`` ``=`` ``"Female"``,`\
`  .missing ``=`` ``"Unknown"``,`\
`  .other ``=`` ``"Other Gender"``,`\
`  name ``=`` ``"sex"`\
`)`\
\
`gender_codes`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``"M"``, ``NA``, ``"X"``, ``"F"``)`\
`formatted_genders`` ``<-`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``gender_codes``, ``"sex"``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  code ``=`` ``gender_codes``,`\
`  label ``=`` ``formatted_genders`\
`)`\
`#>   code        label`\
`#> 1    M         Male`\
`#> 2    F       Female`\
`#> 3    M         Male`\
`#> 4 <NA>      Unknown`\
`#> 5    X Other Gender`\
`#> 6    F       Female`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"sex"``)`\
`#> KS Format:sex`\
`#> Type: character `\
`#> Mappings:`\
`#>   M => Male`\
`#>   F => Female`\
`#>   .missing => Unknown`\
`#>   .other => Other Gender`

## Example 2: Numeric Range Formatting

Define formats in SAS-like text (auto-registered):

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE age (numeric)`\
`  [0, 18)     = "Child"`\
`  [18, 65)    = "Adult"`\
`  [65, HIGH]  = "Senior"`\
`  .missing    = "Age Unknown"`\
`;`\
`'``)`\
\
`ages`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``15.3``, ``17.9``, ``18``, ``45``, ``64.99``, ``65``, ``85``, ``NA``)`\
`age_groups`` ``<-`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``ages``, ``"age"``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  age ``=`` ``ages``,`\
`  group ``=`` ``age_groups`\
`)`\
`#>     age       group`\
`#> 1  5.00       Child`\
`#> 2 15.30       Child`\
`#> 3 17.90       Child`\
`#> 4 18.00       Adult`\
`#> 5 45.00       Adult`\
`#> 6 64.99       Adult`\
`#> 7 65.00      Senior`\
`#> 8 85.00      Senior`\
`#> 9    NA Age Unknown`

## Example 3: Decimal Ranges (BMI Categories)

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE bmi (numeric)`\
`  [0, 18.5)    = "Underweight"`\
`  [18.5, 25)   = "Normal"`\
`  [25, 30)     = "Overweight"`\
`  [30, HIGH]   = "Obese"`\
`  .missing     = "No data"`\
`;`\
`'``)`\
\
`bmi_values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``16.2``, ``18.5``, ``22.7``, ``25``, ``29.9``, ``35.1``, ``NA``)`\
`bmi_labels`` ``<-`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``bmi_values``, ``"bmi"``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  bmi ``=`` ``bmi_values``,`\
`  category ``=`` ``bmi_labels`\
`)`\
`#>    bmi    category`\
`#> 1 16.2 Underweight`\
`#> 2 18.5      Normal`\
`#> 3 22.7      Normal`\
`#> 4 25.0  Overweight`\
`#> 5 29.9  Overweight`\
`#> 6 35.1       Obese`\
`#> 7   NA     No data`

## Example 4: Exclusive/Inclusive Bounds

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE score (numeric)`\
`  (0, 50]    = "Low"`\
`  (50, 100]  = "High"`\
`  .other     = "Out of range"`\
`;`\
`'``)`\
\
`scores`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``1``, ``50``, ``51``, ``100``, ``101``)`\
`score_labels`` ``<-`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``scores``, ``"score"``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  score ``=`` ``scores``,`\
`  label ``=`` ``score_labels`\
`)`\
`#>   score        label`\
`#> 1     0 Out of range`\
`#> 2     1          Low`\
`#> 3    50          Low`\
`#> 4    51         High`\
`#> 5   100         High`\
`#> 6   101 Out of range`

## Example 5: Reverse Formatting with Invalue

Invalues convert labels back to values. The default `target_type` is
`"numeric"`:

\
[`finput`](https://crow16384.github.io/ksformat/reference/finput.md)`(`\
`  ``"Male"`` ``=`` ``1``,`\
`  ``"Female"`` ``=`` ``2``,`\
`  name ``=`` ``"sex_inv"`\
`)`\
`#> KS Invalue: sex_inv`\
`#> Target Type: numeric `\
`#> Mappings:`\
`#>   Male => 1`\
`#>   Female => 2`\
\
`labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Male"``, ``"Female"``, ``"Male"``, ``"Unknown"``, ``"Female"``)`\
`codes`` ``<-`` `[`finputn`](https://crow16384.github.io/ksformat/reference/finputn.md)`(``labels``, ``"sex_inv"``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  label ``=`` ``labels``,`\
`  code ``=`` ``codes`\
`)`\
`#>     label code`\
`#> 1    Male    1`\
`#> 2  Female    2`\
`#> 3    Male    1`\
`#> 4 Unknown   NA`\
`#> 5  Female    2`

## Example 6: Bidirectional Formatting

[`fnew_bid()`](https://crow16384.github.io/ksformat/reference/fnew_bid.md)
creates both a format and an invalue at once:

\
`status_bi`` ``<-`` `[`fnew_bid`](https://crow16384.github.io/ksformat/reference/fnew_bid.md)`(`\
`  ``"A"`` ``=`` ``"Active"``,`\
`  ``"I"`` ``=`` ``"Inactive"``,`\
`  ``"P"`` ``=`` ``"Pending"``,`\
`  name ``=`` ``"status"`\
`)`\
\
`# Forward: code -> label`\
`status_codes`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"A"``, ``"I"``, ``"P"``, ``"A"``)`\
`status_labels`` ``<-`` `[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md)`(``status_codes``, ``"status"``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``code ``=`` ``status_codes``, label ``=`` ``status_labels``)`\
`#>   code    label`\
`#> 1    A   Active`\
`#> 2    I Inactive`\
`#> 3    P  Pending`\
`#> 4    A   Active`\
\
`# Reverse: label -> code`\
`test_labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Active"``, ``"Pending"``, ``"Inactive"``)`\
`test_codes`` ``<-`` `[`finputc`](https://crow16384.github.io/ksformat/reference/finputc.md)`(``test_labels``, ``"status_inv"``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``label ``=`` ``test_labels``, code ``=`` ``test_codes``)`\
`#>      label code`\
`#> 1   Active    A`\
`#> 2  Pending    P`\
`#> 3 Inactive    I`

## Example 7: Parse Multiple Formats from Text

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`// Study format definitions`\
\
`VALUE race (character)`\
`  "W" = "White"`\
`  "B" = "Black"`\
`  "A" = "Asian"`\
`  .missing = "Unknown"`\
`;`\
\
`INVALUE race_inv`\
`  "White" = 1`\
`  "Black" = 2`\
`  "Asian" = 3`\
`;`\
`'``)`\
\
[`flist`](https://crow16384.github.io/ksformat/reference/flist.md)`(``)``   ``# character vector of names`\
`#> [1] "age"        "bmi"        "race"       "race_inv"   "score"     `\
`#> [6] "sex"        "sex_inv"    "status"     "status_inv"`\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``)`\
`#> Registered formats:`\
`#>   age - VALUE (numeric), 3 mapping(s)`\
`#>   bmi - VALUE (numeric), 4 mapping(s)`\
`#>   race - VALUE (character), 3 mapping(s)`\
`#>   race_inv - INVALUE (numeric), 3 mapping(s)`\
`#>   score - VALUE (numeric), 2 mapping(s)`\
`#>   sex - VALUE (character), 2 mapping(s)`\
`#>   sex_inv - INVALUE (numeric), 2 mapping(s)`\
`#>   status - VALUE (character), 3 mapping(s)`\
`#>   status_inv - INVALUE (character), 3 mapping(s)`

## Example 8: Export Formats Back to Text

\
`bmi_fmt`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"bmi"``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``bmi ``=`` ``bmi_fmt``)``)`\
`#> VALUE bmi (numeric)`\
`#>   [0, 18.5) = "Underweight"`\
`#>   [18.5, 25) = "Normal"`\
`#>   [25, 30) = "Overweight"`\
`#>   [30, HIGH] = "Obese"`\
`#>   .missing = "No data"`\
`#> ;`

## Example 9: SAS-like PUT/INPUT Functions

\
`# fputn — apply numeric format by name`\
[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``30``, ``70``)``, ``"age"``)`\
`#> [1] "Child"  "Adult"  "Senior"`\
\
`# fputc — apply character format by name`\
[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``)``, ``"sex"``)`\
`#> [1] "Male"   "Female"`\
\
`# finputn — apply numeric invalue by name`\
[`finputn`](https://crow16384.github.io/ksformat/reference/finputn.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"White"``, ``"Black"``)``, ``"race_inv"``)`\
`#> [1] 1 2`

## Example 10: Data Frame Formatting

\
`df`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  id ``=`` ``1``:``6``,`\
`  sex ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``"M"``, ``"F"``, ``NA``, ``"X"``)``,`\
`  age ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``15``, ``25``, ``45``, ``70``, ``35``, ``NA``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`sex_f`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"sex"``)`\
`age_f`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"age"``)`\
\
`df_formatted`` ``<-`` `[`fput_df`](https://crow16384.github.io/ksformat/reference/fput_df.md)`(`\
`  ``df``,`\
`  sex ``=`` ``sex_f``,`\
`  age ``=`` ``age_f``,`\
`  suffix ``=`` ``"_label"`\
`)`\
\
`df_formatted`\
`#>   id  sex age    sex_label   age_label`\
`#> 1  1    M  15         Male       Child`\
`#> 2  2    F  25       Female       Adult`\
`#> 3  3    M  45         Male       Adult`\
`#> 4  4    F  70       Female      Senior`\
`#> 5  5 <NA>  35      Unknown       Adult`\
`#> 6  6    X  NA Other Gender Age Unknown`

## Example 11: Missing Value Handling

\
`# With .missing label`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``NA``)``, ``"sex"``)`\
`#> [1] "Male"    "Female"  "Unknown"`\
\
`# With keep_na = TRUE`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``NA``)``, ``sex_f``, keep_na ``=`` ``TRUE``)`\
`#> [1] "Male"   "Female" NA`\
\
`# is_missing() checks`\
[`is_missing`](https://crow16384.github.io/ksformat/reference/is_missing.md)`(``NA``)`\
`#> [1] TRUE`\
[`is_missing`](https://crow16384.github.io/ksformat/reference/is_missing.md)`(``NaN``)`\
`#> [1] TRUE`\
[`is_missing`](https://crow16384.github.io/ksformat/reference/is_missing.md)`(``""``)``   ``# TRUE — empty strings are treated as missing`\
`#> [1] TRUE`

## Example 12: Date/Time Formats (SAS-style)

### SAS Date Formats

SAS date format names are auto-resolved — no pre-creation needed:

\
`today`` ``<-`` `[`Sys.Date`](https://rdrr.io/r/base/Sys.time.html)`(``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  format ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"DATE9."``, ``"MMDDYY10."``, ``"DDMMYY10."``, ``"YYMMDD10."``,`\
`             ``"MONYY7."``, ``"WORDDATE."``, ``"YEAR4."``, ``"QTR."``)``,`\
`  result ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"DATE9."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"MMDDYY10."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"DDMMYY10."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"YYMMDD10."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"MONYY7."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"WORDDATE."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"YEAR4."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``today``, ``"QTR."``)`\
`  ``)`\
`)`\
`#>      format           result`\
`#> 1    DATE9.        03OCT2026`\
`#> 2 MMDDYY10.       10/03/2026`\
`#> 3 DDMMYY10.       03/10/2026`\
`#> 4 YYMMDD10.       2026-10-03`\
`#> 5   MONYY7.          OCT2026`\
`#> 6 WORDDATE. October 03, 2026`\
`#> 7    YEAR4.             2026`\
`#> 8      QTR.                4`\
\
`# Multiple dates`\
`dates`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2020-01-15"``, ``"2020-06-30"``, ``"2020-12-25"``)``)`\
[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``dates``, ``"DATE9."``)`\
`#> [1] "15JAN2020" "30JUN2020" "25DEC2020"`

### R Numeric Dates (Days Since 1970-01-01)

\
`r_days`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2025-01-01"``)``)`\
`r_days`\
`#> [1] 20089`\
[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``r_days``, ``"DATE9."``)`\
`#> [1] "01JAN2025"`\
[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``r_days``, ``"MMDDYY10."``)`\
`#> [1] "01/01/2025"`

### Time Formats

Time is represented as seconds since midnight:

\
`seconds`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``3600``, ``45000``, ``86399``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  seconds ``=`` ``seconds``,`\
`  TIME8 ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``seconds``, ``"TIME8."``)``,`\
`  TIME5 ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``seconds``, ``"TIME5."``)``,`\
`  HHMM ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``seconds``, ``"HHMM."``)`\
`)`\
`#>   seconds    TIME8 TIME5  HHMM`\
`#> 1       0  0:00:00  0:00 00:00`\
`#> 2    3600  1:00:00  1:00 01:00`\
`#> 3   45000 12:30:00 12:30 12:30`\
`#> 4   86399 23:59:59 23:59 23:59`

### Datetime Formats

\
`now`` ``<-`` `[`Sys.time`](https://rdrr.io/r/base/Sys.time.html)`(``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  format ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"DATETIME20."``, ``"DATETIME13."``, ``"DTDATE."``, ``"DTYYMMDD."``)``,`\
`  result ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``now``, ``"DATETIME20."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``now``, ``"DATETIME13."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``now``, ``"DTDATE."``)``,`\
`    `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``now``, ``"DTYYMMDD."``)`\
`  ``)`\
`)`\
`#>        format             result`\
`#> 1 DATETIME20. 03OCT2026:20:44:45`\
`#> 2 DATETIME13.      03OCT26:20:44`\
`#> 3     DTDATE.          03OCT2026`\
`#> 4   DTYYMMDD.         2026-10-03`\
\
`# From numeric R-epoch seconds`\
`r_secs`` ``<-`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)`(``"2025-06-15 14:30:00"``, tz ``=`` ``"UTC"``)``)`\
[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``r_secs``, ``"DATETIME20."``)`\
`#> [1] "15JUN2025:14:30:00"`

### Custom Date Formats with `fnew_date()`

\
`# SAS-named format`\
[`fnew_date`](https://crow16384.github.io/ksformat/reference/fnew_date.md)`(``"DATE9."``, name ``=`` ``"bday_fmt"``)`\
`#> KS Format:bday_fmt`\
`#> Type: date `\
`#> Pattern: %d%b%Y (DATE9.)`\
`birthdays`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"1990-03-25"``, ``"1985-11-03"``, ``"2000-07-14"``)``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``birthdays``, ``"bday_fmt"``)`\
`#> [1] "25MAR1990" "03NOV1985" "14JUL2000"`\
\
`# Custom strftime pattern (e.g. DD.MM.YYYY)`\
[`fnew_date`](https://crow16384.github.io/ksformat/reference/fnew_date.md)`(``"%d.%m.%Y"``, name ``=`` ``"ru_date"``, type ``=`` ``"date"``)`\
`#> KS Format:ru_date`\
`#> Type: date `\
`#> Pattern: %d.%m.%Y`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``birthdays``, ``"ru_date"``)`\
`#> [1] "25.03.1990" "03.11.1985" "14.07.2000"`\
\
`# Custom pattern with missing label`\
[`fnew_date`](https://crow16384.github.io/ksformat/reference/fnew_date.md)`(``"MMDDYY10."``, name ``=`` ``"us_date"``, .missing ``=`` ``"NO DATE"``)`\
`#> KS Format:us_date`\
`#> Type: date `\
`#> Pattern: %m/%d/%Y (MMDDYY10.) `\
`#>   .missing => NO DATE`\
`mixed`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2025-01-01"``)``, ``NA``, `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2025-12-31"``)``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``mixed``, ``"us_date"``)`\
`#> [1] "01/01/2025" "NO DATE"    "12/31/2025"`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"bday_fmt"``)`\
`#> KS Format:bday_fmt`\
`#> Type: date `\
`#> Pattern: %d%b%Y (DATE9.)`

### Date Formats in Data Frames

\
`patients`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  id ``=`` ``1``:``4``,`\
`  visit_date ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2025-01-10"``, ``"2025-02-15"``, ``"2025-03-20"``, ``NA``)``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`visit_fmt`` ``<-`` `[`fnew_date`](https://crow16384.github.io/ksformat/reference/fnew_date.md)`(``"DATE9."``, name ``=`` ``"visit_fmt"``, .missing ``=`` ``"NOT RECORDED"``)`\
[`fput_df`](https://crow16384.github.io/ksformat/reference/fput_df.md)`(``patients``, visit_date ``=`` ``visit_fmt``)`\
`#>   id visit_date visit_date_fmt`\
`#> 1  1 2025-01-10      10JAN2025`\
`#> 2  2 2025-02-15      15FEB2025`\
`#> 3  3 2025-03-20      20MAR2025`\
`#> 4  4       <NA>   NOT RECORDED`

### Parse Date Formats from Text

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE enrldt (date)`\
`  pattern = "DATE9."`\
`  .missing = "Not Enrolled"`\
`;`\
\
`VALUE visit_time (time)`\
`  pattern = "TIME8."`\
`;`\
\
`VALUE stamp (datetime)`\
`  pattern = "DATETIME20."`\
`;`\
`'``)`\
\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``"2025-03-01"``)``, ``"enrldt"``)`\
`#> [1] "01MAR2025"`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``36000``, ``"visit_time"``)`\
`#> [1] "10:00:00"`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)`(``"2025-03-01 10:00:00"``, tz ``=`` ``"UTC"``)``, ``"stamp"``)`\
`#> [1] "01MAR2025:10:00:00"`\
\
`# Export back to text`\
`enrl_obj`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"enrldt"``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``enrldt ``=`` ``enrl_obj``)``)`\
`#> VALUE enrldt (date)`\
`#>   pattern = "DATE9."`\
`#>   .missing = "Not Enrolled"`\
`#> ;`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 13: Multilabel Formats

### Overlapping Age Categories

With multilabel formats, a single value can match multiple labels:

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"0,5,TRUE,TRUE"``    ``=`` ``"Infant"``,`\
`  ``"6,11,TRUE,TRUE"``   ``=`` ``"Child"``,`\
`  ``"12,17,TRUE,TRUE"``  ``=`` ``"Adolescent"``,`\
`  ``"0,17,TRUE,TRUE"``   ``=`` ``"Pediatric"``,`\
`  ``"18,64,TRUE,TRUE"``  ``=`` ``"Adult"``,`\
`  ``"65,Inf,TRUE,TRUE"`` ``=`` ``"Elderly"``,`\
`  ``"18,Inf,TRUE,TRUE"`` ``=`` ``"Non-Pediatric"``,`\
`  name ``=`` ``"age_categories"``,`\
`  type ``=`` ``"numeric"``,`\
`  multilabel ``=`` ``TRUE`\
`)`\
\
`ages`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``3``, ``14``, ``25``, ``70``)`\
\
`# fput returns first match only`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``ages``, ``"age_categories"``)`\
`#> [1] "Infant"        "Pediatric"     "Adult"         "Non-Pediatric"`\
\
`# fput_all returns ALL matching labels`\
`all_labels`` ``<-`` `[`fput_all`](https://crow16384.github.io/ksformat/reference/fput_all.md)`(``ages``, ``"age_categories"``)`\
`for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``ages``)``)`` ``{`\
`  `[`cat`](https://rdrr.io/r/base/cat.html)`(``"Age"``, ``ages``[``i``]``, ``"->"``, `[`paste`](https://rdrr.io/r/base/paste.html)`(``all_labels``[[``i``]``]``, collapse ``=`` ``", "``)``, ``"\n"``)`\
`}`\
`#> Age 3 -> Infant, Pediatric `\
`#> Age 14 -> Pediatric, Adolescent `\
`#> Age 25 -> Adult, Non-Pediatric `\
`#> Age 70 -> Non-Pediatric, Elderly`

### Multilabel with Missing Values

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"0,100,TRUE,TRUE"``   ``=`` ``"Valid Score"``,`\
`  ``"0,49,TRUE,TRUE"``    ``=`` ``"Below Average"``,`\
`  ``"50,100,TRUE,TRUE"``  ``=`` ``"Above Average"``,`\
`  ``"90,100,TRUE,TRUE"``  ``=`` ``"Excellent"``,`\
`  .missing ``=`` ``"No Score"``,`\
`  .other ``=`` ``"Out of Range"``,`\
`  name ``=`` ``"score_ml"``,`\
`  type ``=`` ``"numeric"``,`\
`  multilabel ``=`` ``TRUE`\
`)`\
\
`scores`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``95``, ``45``, ``NA``, ``150``)`\
`ml_result`` ``<-`` `[`fput_all`](https://crow16384.github.io/ksformat/reference/fput_all.md)`(``scores``, ``"score_ml"``)`\
\
`for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``scores``)``)`` ``{`\
`  `[`cat`](https://rdrr.io/r/base/cat.html)`(``"Score"``, `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(`[`is.na`](https://rdrr.io/r/base/NA.html)`(``scores``[``i``]``)``, ``"NA"``, ``scores``[``i``]``)``,`\
`      ``"->"``, `[`paste`](https://rdrr.io/r/base/paste.html)`(``ml_result``[[``i``]``]``, collapse ``=`` ``", "``)``, ``"\n"``)`\
`}`\
`#> Score 95 -> Valid Score, Above Average, Excellent `\
`#> Score 45 -> Below Average, Valid Score `\
`#> Score NA -> No Score `\
`#> Score 150 -> Out of Range`

### Parse Multilabel from Text

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE risk (numeric, multilabel)`\
`  [0, 3]   = "Low Risk"`\
`  [0, 7]   = "Monitored"`\
`  (3, 7]   = "Medium Risk"`\
`  (7, 10]  = "High Risk"`\
`;`\
`'``)`\
\
`risk_scores`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``5``, ``9``)`\
`risk_labels`` ``<-`` `[`fput_all`](https://crow16384.github.io/ksformat/reference/fput_all.md)`(``risk_scores``, ``"risk"``)`\
`for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``risk_scores``)``)`` ``{`\
`  `[`cat`](https://rdrr.io/r/base/cat.html)`(``"Score"``, ``risk_scores``[``i``]``, ``"->"``,`\
`      `[`paste`](https://rdrr.io/r/base/paste.html)`(``risk_labels``[[``i``]``]``, collapse ``=`` ``" | "``)``, ``"\n"``)`\
`}`\
`#> Score 2 -> Low Risk | Monitored `\
`#> Score 5 -> Monitored | Medium Risk `\
`#> Score 9 -> High Risk`

### Multilabel Export

\
`risk_obj`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"risk"``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``risk ``=`` ``risk_obj``)``)`\
`#> VALUE risk (numeric, multilabel)`\
`#>   [0, 3] = "Low Risk"`\
`#>   [0, 7] = "Monitored"`\
`#>   (3, 7] = "Medium Risk"`\
`#>   (7, 10] = "High Risk"`\
`#> ;`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"risk"``)`\
`#> KS Format:risk (multilabel)`\
`#> Type: numeric `\
`#> Mappings:`\
`#>   [0, 3] => Low Risk`\
`#>   [0, 7] => Monitored`\
`#>   (3, 7] => Medium Risk`\
`#>   (7, 10] => High Risk`

### Practical Example: Adverse Event Severity Grading

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"1,1,TRUE,TRUE"`` ``=`` ``"Mild"``,`\
`  ``"2,2,TRUE,TRUE"`` ``=`` ``"Moderate"``,`\
`  ``"3,3,TRUE,TRUE"`` ``=`` ``"Severe"``,`\
`  ``"4,4,TRUE,TRUE"`` ``=`` ``"Life-threatening"``,`\
`  ``"5,5,TRUE,TRUE"`` ``=`` ``"Fatal"``,`\
`  ``"3,5,TRUE,TRUE"`` ``=`` ``"Serious"``,`\
`  ``"1,2,TRUE,TRUE"`` ``=`` ``"Non-serious"``,`\
`  name ``=`` ``"ae_grade"``,`\
`  type ``=`` ``"numeric"``,`\
`  multilabel ``=`` ``TRUE`\
`)`\
\
`grades`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``3``, ``4``, ``5``)`\
`ae_labels`` ``<-`` `[`fput_all`](https://crow16384.github.io/ksformat/reference/fput_all.md)`(``grades``, ``"ae_grade"``)`\
`for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``grades``)``)`` ``{`\
`  `[`cat`](https://rdrr.io/r/base/cat.html)`(``"Grade"``, ``grades``[``i``]``, ``":"``,`\
`      `[`paste`](https://rdrr.io/r/base/paste.html)`(``ae_labels``[[``i``]``]``, collapse ``=`` ``" + "``)``, ``"\n"``)`\
`}`\
`#> Grade 1 : Mild + Non-serious `\
`#> Grade 2 : Non-serious + Moderate `\
`#> Grade 3 : Severe + Serious `\
`#> Grade 4 : Serious + Life-threatening `\
`#> Grade 5 : Serious + Fatal`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 14: Case-Insensitive Matching

\
`sex_nc`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"M"`` ``=`` ``"Male"``,`\
`  ``"F"`` ``=`` ``"Female"``,`\
`  .missing ``=`` ``"Unknown"``,`\
`  name ``=`` ``"sex_nc"``,`\
`  type ``=`` ``"character"``,`\
`  ignore_case ``=`` ``TRUE`\
`)`\
\
`input`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"m"``, ``"F"``, ``"M"``, ``"f"``, ``NA``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``input``, ``sex_nc``)`\
`#> [1] "Male"    "Female"  "Male"    "Female"  "Unknown"`\
\
`# Note the [nocase] flag`\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"sex_nc"``)`\
`#> KS Format:sex_nc (nocase)`\
`#> Type: character `\
`#> Mappings:`\
`#>   M => Male`\
`#>   F => Female`\
`#>   .missing => Unknown`\
\
`# Also works with fputc`\
[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md)`(``"m"``, ``"sex_nc"``)`\
`#> [1] "Male"`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 15: Expression Labels in Formats

Expression labels contain `.x1`, `.x2`, etc., which reference extra
arguments passed to
[`fput()`](https://crow16384.github.io/ksformat/reference/fput.md). This
lets you compute labels dynamically.

### Simple `sprintf` Expression

\
`stat_fmt`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"n"``   ``=`` ``"sprintf('%s', .x1)"``,`\
`  ``"pct"`` ``=`` ``"sprintf('%.1f%%', .x1 * 100)"``,`\
`  name ``=`` ``"stat"``,`\
`  type ``=`` ``"character"`\
`)`\
\
`types``  ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"n"``,  ``"pct"``,  ``"n"``,   ``"pct"``)`\
`values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``42``,   ``0.053``,  ``100``,   ``0.255``)`\
\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``types``, ``stat_fmt``, ``values``)`\
`#> [1] "42"    "5.3%"  "100"   "25.5%"`

### Two Extra Arguments (`.x1`, `.x2`)

\
`ratio_fmt`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"ratio"`` ``=`` ``"sprintf('%s/%s', .x1, .x2)"``,`\
`  name ``=`` ``"ratio"``,`\
`  type ``=`` ``"character"`\
`)`\
\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``"ratio"``, ``ratio_fmt``, ``3``, ``10``)`\
`#> [1] "3/10"`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"ratio"``, ``"ratio"``)``, ``ratio_fmt``, `[`c`](https://rdrr.io/r/base/c.html)`(``3``, ``7``)``, `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``20``)``)`\
`#> [1] "3/10" "7/20"`

### `ifelse` Expression

\
`sign_fmt`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"val"`` ``=`` ``"ifelse(.x1 > 0, paste0('+', .x1), as.character(.x1))"``,`\
`  name ``=`` ``"sign"``,`\
`  type ``=`` ``"character"`\
`)`\
\
`nums`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``0``, ``-``3``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``"val"``, ``3``)``, ``sign_fmt``, ``nums``)`\
`#> [1] "+5" "0"  "-3"`

### Mixed Static and Expression Labels

\
`mixed_fmt`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"header"`` ``=`` ``"HEADER"``,`\
`  ``"n"``      ``=`` ``"sprintf('N=%s', .x1)"``,`\
`  ``"pct"``    ``=`` ``"sprintf('%.1f%%', .x1 * 100)"``,`\
`  name ``=`` ``"mixed"``,`\
`  type ``=`` ``"character"`\
`)`\
\
`keys`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"header"``, ``"n"``, ``"pct"``, ``"header"``, ``"n"``)`\
`vals`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``,        ``42``,  ``0.15``,  ``0``,        ``100``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``keys``, ``mixed_fmt``, ``vals``)`\
`#> [1] "HEADER" "N=42"   "15.0%"  "HEADER" "N=100"`

### Expression in `.other` Fallback

\
`known_fmt`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"ok"`` ``=`` ``"OK"``,`\
`  .other ``=`` ``"sprintf('Error(%s)', .x1)"``,`\
`  name ``=`` ``"err_fmt"``,`\
`  type ``=`` ``"character"`\
`)`\
\
`codes``   ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ok"``, ``"E01"``, ``"ok"``, ``"E99"``)`\
`details`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``""``,   ``"timeout"``, ``""``, ``"overflow"``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``codes``, ``known_fmt``, ``details``)`\
`#> [1] "OK"              "Error(timeout)"  "OK"              "Error(overflow)"`

### Scalar Recycling

\
`label_fmt`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"val"`` ``=`` ``"sprintf('%s (N=%s)', .x1, .x2)"``,`\
`  name ``=`` ``"recycle"``,`\
`  type ``=`` ``"character"`\
`)`\
\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"val"``, ``"val"``)``, ``label_fmt``, `[`c`](https://rdrr.io/r/base/c.html)`(``42``, ``55``)``, ``100``)`\
`#> [1] "42 (N=100)" "55 (N=100)"`

### Statistical Table Format with Computed Labels

A realistic clinical-trial example:
[`e()`](https://crow16384.github.io/ksformat/reference/e.md) marks
labels as expressions evaluated at apply-time, `.x1` references the
extra argument, and multiline
[`dplyr::case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
shows complex conditional formatting.

\
`# Population counts used as denominators`\
`n.trt`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``pop ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"fas"``,``"pps"``,``"saf"``)``, ntot ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``34``, ``30``, ``36``)``)`\
`get_n`` ``<-`` ``function``(``pop``)`` ``{`\
`  ``n.trt``$``ntot``[``n.trt``$``pop`` ``==`` ``pop``]`\
`}`\
\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"n_fas"`` ``=`` `[`e`](https://crow16384.github.io/ksformat/reference/e.md)`(``"get_n('fas')"``)``,`\
`  ``"n_pps"`` ``=`` `[`e`](https://crow16384.github.io/ksformat/reference/e.md)`(``"get_n('pps')"``)``,`\
`  ``"n_saf"`` ``=`` `[`e`](https://crow16384.github.io/ksformat/reference/e.md)`(``"get_n('saf')"``)``,`\
`  ``"n"``   ``=`` ``"sprintf('%d', .x1)"``,`\
`  ``"n_pct_fas"`` ``=`` ``"sprintf('%d (%5.1f%%)', .x1, .x1 * 100 / get_n('fas'))"``,`\
`  ``"n_pct_pps"`` ``=`` ``"sprintf('%d (%5.1f%%)', .x1, .x1 * 100 / get_n('pps'))"``,`\
`  ``"n_pct_saf"`` ``=`` ``"sprintf('%d (%5.1f%%)', .x1, .x1 * 100 / get_n('saf'))"``,`\
`  ``"pct"`` ``=`` ``"dplyr::case_when(`\
`               .x1>0 & .x1<0.1 ~ sprintf('%5s', ' <0.1%'),`\
`               .x1>=0.1 | .x1==0 ~ sprintf(paste0('%5.', 1 ,'f%%'), .x1)`\
`           )"``,`\
`  ``"pval"`` ``=`` ``"dplyr::case_when(`\
`                .x1>=0 & .x1<0.001 ~ sprintf('%s', '<0.001'),`\
`                .x1>=0.001 & .x1<=0.999 ~ sprintf(paste0('%.', 3 ,'f'), .x1),`\
`                .x1>0.999 ~ sprintf('%s', '>0.999'), .default = '--'`\
`           )"``,`\
`  name ``=`` ``"stat"``,`\
`  type ``=`` ``"character"`\
`)`

The same format can be created via
[`fparse()`](https://crow16384.github.io/ksformat/reference/fparse.md).
Note that multiline expressions must be collapsed to single lines in the
text block, and `(eval)` marks evaluated labels:

\
`fmt`` ``<-`` ``'`\
`  VALUE stat_01 (character)`\
`     "n_fas" = "get_n(\'fas\')" (eval)`\
`     "n_pps" = "get_n(\'pps\')" (eval)`\
`     "n_saf" = "get_n(\'saf\')" (eval)`\
`     "n"     = "sprintf(\'%d\', .x1)"`\
`     "pct"   = "dplyr::case_when(.x1>0 & .x1<0.1 ~ sprintf(\'%5s\', \' <0.1%\'), .x1>=0.1 | .x1==0 ~ sprintf(paste0(\'%5.\', 1 ,\'f%%\'), .x1))"`\
`     "n_pct_fas" = "sprintf(\'%d (%5.1f%%)\', .x1, .x1 * 100 / get_n(\'fas\'))"`\
`     "n_pct_pps" = "sprintf(\'%d (%5.1f%%)\', .x1, .x1 * 100 / get_n(\'pps\'))"`\
`     "n_pct_saf" = "sprintf(\'%d (%5.1f%%)\', .x1, .x1 * 100 / get_n(\'saf\'))"`\
`     "pval"  = "dplyr::case_when(.x1>=0 & .x1<0.001 ~ sprintf(\'%s\', \'<0.001\'), .x1>=0.001 & .x1<=0.999 ~ sprintf(paste0(\'%.\', 3 ,\'f\'), .x1), .x1>0.999 ~ sprintf(\'%s\', \'>0.999\'), .default = \'--\')"`\
`;'`\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``fmt``)`

Both `stat` (via `fnew`) and `stat_01` (via `fparse`) produce identical
results:

\
`df`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  types ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"n_fas"``, ``"n_pps"``, ``"n_saf"``, ``"n"``, ``"pct"``, ``"pct"``, ``"n"``, ``"pval"``, ``"pval"``,`\
`            ``"n_pct_fas"``, ``"n_pct_pps"``, ``"n_pct_saf"``)``,`\
`  values ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``NA``, ``NA``, ``NA``, ``42``, ``0.053``, ``0.0008``, ``100``, ``0.255``, ``0.0003``, ``22``, ``22``, ``22``)`\
`)`\
\
`df``$``fmt``    ``<-`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``df``$``types``, ``"stat"``,    ``df``$``values``)`\
`df``$``fmt_01`` ``<-`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``df``$``types``, ``"stat_01"``, ``df``$``values``)`\
[`print`](https://rdrr.io/r/base/print.html)`(``df``)`\
`#>        types   values         fmt      fmt_01`\
`#> 1      n_fas       NA          34          34`\
`#> 2      n_pps       NA          30          30`\
`#> 3      n_saf       NA          36          36`\
`#> 4          n  42.0000          42          42`\
`#> 5        pct   0.0530       <0.1%       <0.1%`\
`#> 6        pct   0.0008       <0.1%       <0.1%`\
`#> 7          n 100.0000         100         100`\
`#> 8       pval   0.2550       0.255       0.255`\
`#> 9       pval   0.0003      <0.001      <0.001`\
`#> 10 n_pct_fas  22.0000 22 ( 64.7%) 22 ( 64.7%)`\
`#> 11 n_pct_pps  22.0000 22 ( 73.3%) 22 ( 73.3%)`\
`#> 12 n_pct_saf  22.0000 22 ( 61.1%) 22 ( 61.1%)`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 16: Vectorized Format Names (SAS PUTC-style)

Each element can use a different format, determined by a vector of
format names:

\
`# Dispatch format: maps type code to format name`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"1"`` ``=`` ``"groupx"``, ``"2"`` ``=`` ``"groupy"``, ``"3"`` ``=`` ``"groupz"``,`\
`     name ``=`` ``"typefmt"``, type ``=`` ``"numeric"``)`\
\
`# Per-group character formats`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"positive"`` ``=`` ``"agree"``,  ``"negative"`` ``=`` ``"disagree"``, ``"neutral"`` ``=`` ``"notsure"``,`\
`     name ``=`` ``"groupx"``, type ``=`` ``"character"``)`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"positive"`` ``=`` ``"accept"``, ``"negative"`` ``=`` ``"reject"``,   ``"neutral"`` ``=`` ``"possible"``,`\
`     name ``=`` ``"groupy"``, type ``=`` ``"character"``)`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"positive"`` ``=`` ``"pass"``,   ``"negative"`` ``=`` ``"fail"``,     ``"neutral"`` ``=`` ``"retest"``,`\
`     name ``=`` ``"groupz"``, type ``=`` ``"character"``)`\
\
`type``     ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``1``, ``1``, ``2``, ``2``, ``2``, ``3``, ``3``, ``3``)`\
`response`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"positive"``, ``"negative"``, ``"neutral"``,`\
`              ``"positive"``, ``"negative"``, ``"neutral"``,`\
`              ``"positive"``, ``"negative"``, ``"neutral"``)`\
\
`# Step 1: map type -> format name`\
`respfmt`` ``<-`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``type``, ``"typefmt"``)`\
\
`# Step 2: apply per-element format`\
`word`` ``<-`` `[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md)`(``response``, ``respfmt``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``type ``=`` ``type``, response ``=`` ``response``, respfmt ``=`` ``respfmt``, word ``=`` ``word``)`\
`#>   type response respfmt     word`\
`#> 1    1 positive  groupx    agree`\
`#> 2    1 negative  groupx disagree`\
`#> 3    1  neutral  groupx  notsure`\
`#> 4    2 positive  groupy   accept`\
`#> 5    2 negative  groupy   reject`\
`#> 6    2  neutral  groupy possible`\
`#> 7    3 positive  groupz     pass`\
`#> 8    3 negative  groupz     fail`\
`#> 9    3  neutral  groupz   retest`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 17: Working with Dates and Formats — PUTN

A SAS-style workflow where format names are looked up dynamically per
observation:

\
`# Format that maps key codes to date format names`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"1"`` ``=`` ``"date9."``, ``"2"`` ``=`` ``"mmddyy10."``,`\
`     name ``=`` ``"writfmt"``, type ``=`` ``"numeric"``)`\
\
[`fnew_date`](https://crow16384.github.io/ksformat/reference/fnew_date.md)`(``"date9."``)`\
`#> KS Format:DATE9.`\
`#> Type: date `\
`#> Pattern: %d%b%Y (DATE9.)`\
[`fnew_date`](https://crow16384.github.io/ksformat/reference/fnew_date.md)`(``"mmddyy10."``)`\
`#> KS Format:MMDDYY10.`\
`#> Type: date `\
`#> Pattern: %m/%d/%Y (MMDDYY10.)`\
\
`# Input data (R date numbers = days since 1970-01-01)`\
`number`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``12103``, ``10899``)`\
`key``    ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``)`\
\
`# Look up format name per observation`\
`datefmt`` ``<-`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``key``, ``"writfmt"``)`\
\
`# Apply per-element date format`\
`date`` ``<-`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``number``, ``datefmt``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``number ``=`` ``number``, key ``=`` ``key``, datefmt ``=`` ``datefmt``, date ``=`` ``date``)`\
`#>   number key   datefmt       date`\
`#> 1  12103   1    date9.  20FEB2003`\
`#> 2  10899   2 mmddyy10. 11/04/1999`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 18: Import SAS Formats from CNTLOUT CSV

The
[`fimport()`](https://crow16384.github.io/ksformat/reference/fimport.md)
function reads a CSV file exported from a SAS format catalogue
(`PROC FORMAT ... CNTLOUT=`):

\
`csv_path`` ``<-`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"extdata"``, ``"test_cntlout.csv"``, package ``=`` ``"ksformat"``)`

\
`imported`` ``<-`` `[`fimport`](https://crow16384.github.io/ksformat/reference/fimport.md)`(``csv_path``)`\
`#> Warning: Skipping PICTURE format: ``"PICFMT"`\
`#> ``ℹ`` TYPE=``"P"`` is not supported by ksformat.`\
`#> Warning: Skipped incompatible entry in format ``"SMISSING"``:`\
`#> ``✖`` SAS special missing value '.A' (HLO='S') has no R equivalent.`\
`#> Warning: Skipped incompatible entry in format ``"SMISSING"``:`\
`#> ``✖`` SAS special missing value '.B' (HLO='S') has no R equivalent.`\
`#> ``✔`` Imported 4 formats and 1 invalue from`\
`#>   ``/private/var/folders/rn/3s0h46m118j426j_fmjr1z8m0000gn/T/Rtmpiew0So/temp_libpath67243bbc49ff/ksformat/extdata/test_cntlout.csv``.`\
[`names`](https://rdrr.io/r/base/names.html)`(``imported``)`\
`#> [1] "AGEGRP"   "BMICAT"   "GENDER"   "RACEIN"   "SMISSING"`\
\
[`flist`](https://crow16384.github.io/ksformat/reference/flist.md)`(``)`\
`#> [1] "AGEGRP"   "BMICAT"   "GENDER"   "RACEIN"   "SMISSING"`\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``)`\
`#> Registered formats:`\
`#>   AGEGRP - VALUE (numeric), 3 mapping(s)`\
`#>   BMICAT - VALUE (numeric), 4 mapping(s)`\
`#>   GENDER - VALUE (character), 2 mapping(s)`\
`#>   RACEIN - INVALUE (numeric), 3 mapping(s)`\
`#>   SMISSING - VALUE (numeric), 1 mapping(s)`

### Use Imported Formats

\
`# Character format (GENDER)`\
`gender_codes`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``NA``, ``"X"``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  code ``=`` ``gender_codes``,`\
`  label ``=`` `[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md)`(``gender_codes``, ``"GENDER"``)`\
`)`\
`#>   code   label`\
`#> 1    M    Male`\
`#> 2    F  Female`\
`#> 3 <NA> Unknown`\
`#> 4    X       X`\
\
`# Numeric format (AGEGRP)`\
`ages`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``17``, ``18``, ``45``, ``65``, ``100``, ``NA``, ``-``1``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  age ``=`` ``ages``,`\
`  group ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``ages``, ``"AGEGRP"``)`\
`)`\
`#>   age       group`\
`#> 1   5       Child`\
`#> 2  17       Child`\
`#> 3  18       Adult`\
`#> 4  45       Adult`\
`#> 5  65      Senior`\
`#> 6 100      Senior`\
`#> 7  NA Missing Age`\
`#> 8  -1       Other`\
\
`# Numeric format (BMICAT)`\
`bmi_values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``15.0``, ``18.5``, ``22.3``, ``25.0``, ``28.7``, ``30.0``, ``35.5``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  bmi ``=`` ``bmi_values``,`\
`  category ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``bmi_values``, ``"BMICAT"``)`\
`)`\
`#>    bmi    category`\
`#> 1 15.0 Underweight`\
`#> 2 18.5      Normal`\
`#> 3 22.3      Normal`\
`#> 4 25.0  Overweight`\
`#> 5 28.7  Overweight`\
`#> 6 30.0       Obese`\
`#> 7 35.5       Obese`\
\
`# Invalue (RACEIN)`\
`race_labels`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"White"``, ``"Black"``, ``"Asian"``, ``"Other"``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  label ``=`` ``race_labels``,`\
`  code ``=`` `[`finputn`](https://crow16384.github.io/ksformat/reference/finputn.md)`(``race_labels``, ``"RACEIN"``)`\
`)`\
`#>   label code`\
`#> 1 White    1`\
`#> 2 Black    2`\
`#> 3 Asian    3`\
`#> 4 Other   NA`

### Apply to Data Frame

\
`df`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  id ``=`` ``1``:``5``,`\
`  sex ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``"M"``, ``NA``, ``"F"``)``,`\
`  age ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``10``, ``30``, ``70``, ``NA``, ``50``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`gender_fmt`` ``<-`` ``imported``[[``"GENDER"``]``]`\
`age_fmt``    ``<-`` ``imported``[[``"AGEGRP"``]``]`\
\
[`fput_df`](https://crow16384.github.io/ksformat/reference/fput_df.md)`(``df``, sex ``=`` ``gender_fmt``, age ``=`` ``age_fmt``, suffix ``=`` ``"_label"``)`\
`#>   id  sex age sex_label   age_label`\
`#> 1  1    M  10      Male       Child`\
`#> 2  2    F  30    Female       Adult`\
`#> 3  3    M  70      Male      Senior`\
`#> 4  4 <NA>  NA   Unknown Missing Age`\
`#> 5  5    F  50    Female       Adult`

### Export Imported Format

\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``AGEGRP ``=`` ``age_fmt``)``)`\
`#> VALUE AGEGRP (numeric)`\
`#>   [0, 17] = "Child"`\
`#>   [18, 64] = "Adult"`\
`#>   [65, HIGH] = "Senior"`\
`#>   .missing = "Missing Age"`\
`#>   .other = "Other"`\
`#> ;`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``GENDER ``=`` ``gender_fmt``)``)`\
`#> VALUE GENDER (character)`\
`#>   "M" = "Male"`\
`#>   "F" = "Female"`\
`#>   .missing = "Unknown"`\
`#> ;`

### Selective Import (No Auto-register)

\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`\
\
`manual`` ``<-`` `[`fimport`](https://crow16384.github.io/ksformat/reference/fimport.md)`(``csv_path``, register ``=`` ``FALSE``)`\
`#> Warning: Skipping PICTURE format: ``"PICFMT"`\
`#> ``ℹ`` TYPE=``"P"`` is not supported by ksformat.`\
`#> Warning: Skipped incompatible entry in format ``"SMISSING"``:`\
`#> ``✖`` SAS special missing value '.A' (HLO='S') has no R equivalent.`\
`#> Warning: Skipped incompatible entry in format ``"SMISSING"``:`\
`#> ``✖`` SAS special missing value '.B' (HLO='S') has no R equivalent.`\
`#> ``✔`` Imported 4 formats and 1 invalue from`\
`#>   ``/private/var/folders/rn/3s0h46m118j426j_fmjr1z8m0000gn/T/Rtmpiew0So/temp_libpath67243bbc49ff/ksformat/extdata/test_cntlout.csv``.`\
\
`# Library should be empty`\
[`flist`](https://crow16384.github.io/ksformat/reference/flist.md)`(``)`\
`#> character(0)`\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``)`\
`#> Format library is empty`\
\
`# Use directly from returned list`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``)``, ``manual``[[``"GENDER"``]``]``)`\
`#> [1] "Male"   "Female"`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 19: Bilingual Format

Expression labels can select between languages at apply-time using an
extra argument:

\
`# Single format, language selected via .x1 extra argument`\
`sex_bi`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"M"`` ``=`` ``"ifelse(.x1 == 'en', 'Male', 'Homme')"``,`\
`  ``"F"`` ``=`` ``"ifelse(.x1 == 'en', 'Female', 'Femme')"``,`\
`  .missing ``=`` ``"Unknown"``,`\
`  name ``=`` ``"sex_bi"`\
`)`\
\
`# .x1 = language code per observation`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``"M"``)``, ``sex_bi``, `[`c`](https://rdrr.io/r/base/c.html)`(``"en"``, ``"fr"``, ``"en"``)``)`\
`#> [1] "Male"  "Femme" "Male"`\
`# -> "Male" "Femme" "Male"`\
\
`# Alternative: one format per language, selected at apply-time`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"M"`` ``=`` ``"Male"``,  ``"F"`` ``=`` ``"Female"``,  .missing ``=`` ``"Unknown"``, name ``=`` ``"sex_en"``)`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"M"`` ``=`` ``"Homme"``, ``"F"`` ``=`` ``"Femme"``,   .missing ``=`` ``"Inconnu"``, name ``=`` ``"sex_fr"``)`\
\
`lang`` ``<-`` ``"fr"`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``, ``NA``)``, `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"sex_"``, ``lang``)``)`\
`#> [1] "Homme"   "Femme"   "Inconnu"`\
`# -> "Homme" "Femme" "Inconnu"`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 20: Composite Key Lookup with `fputk()`

[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md)
pastes multiple vectors into a composite key before format lookup. This
is useful when a format is keyed on the combination of several columns,
a common pattern in clinical data (e.g., looking up a visit date by
subject + visit number).

\
`# Simulate a Subject Visits (SV) domain`\
`SV`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  USUBJID  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SUBJ-001"``, ``"SUBJ-001"``, ``"SUBJ-001"``, ``"SUBJ-002"``, ``"SUBJ-002"``)``,`\
`  VISITNUM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``3``, ``1``, ``2``)``,`\
`  SVSTDTC  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"2025-01-15"``, ``"2025-02-20"``, ``"2025-03-10"``,`\
`               ``"2025-01-18"``, ``"2025-02-25"``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`# Simulate a Questionnaires (QS) domain`\
`QS`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  USUBJID  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SUBJ-001"``, ``"SUBJ-001"``, ``"SUBJ-002"``, ``"SUBJ-002"``, ``"SUBJ-002"``)``,`\
`  VISITNUM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``1``, ``2``, ``3``)``,`\
`  QSTESTCD ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SCORE1"``, ``"SCORE1"``, ``"SCORE1"``, ``"SCORE1"``, ``"SCORE1"``)``,`\
`  QSSTRESN ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``85``, ``90``, ``72``, ``78``, ``NA``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`SV`\
`#>    USUBJID VISITNUM    SVSTDTC`\
`#> 1 SUBJ-001        1 2025-01-15`\
`#> 2 SUBJ-001        2 2025-02-20`\
`#> 3 SUBJ-001        3 2025-03-10`\
`#> 4 SUBJ-002        1 2025-01-18`\
`#> 5 SUBJ-002        2 2025-02-25`\
`QS`\
`#>    USUBJID VISITNUM QSTESTCD QSSTRESN`\
`#> 1 SUBJ-001        1   SCORE1       85`\
`#> 2 SUBJ-001        2   SCORE1       90`\
`#> 3 SUBJ-002        1   SCORE1       72`\
`#> 4 SUBJ-002        2   SCORE1       78`\
`#> 5 SUBJ-002        3   SCORE1       NA`

### Character lookup (returns character strings)

Register a format keyed on `USUBJID|VISITNUM` with values being the
visit start date (`SVSTDTC`) as character strings:

\
`# Create composite key -> date string mapping from SV`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``SV``$``USUBJID``, ``SV``$``VISITNUM``, sep ``=`` ``"|"``)``, ``SV``$``SVSTDTC``)``,`\
`  .other  ``=`` ``"NOT FOUND"``,`\
`  name    ``=`` ``"svdtc"``,`\
`  type    ``=`` ``"character"``,`\
`  ignore_case ``=`` ``TRUE`\
`)`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"svdtc"``)`\
`#> KS Format:svdtc (nocase)`\
`#> Type: character `\
`#> Mappings:`\
`#>   SUBJ-001|1 => 2025-01-15`\
`#>   SUBJ-001|2 => 2025-02-20`\
`#>   SUBJ-001|3 => 2025-03-10`\
`#>   SUBJ-002|1 => 2025-01-18`\
`#>   SUBJ-002|2 => 2025-02-25`\
`#>   .other => NOT FOUND`

Now look up visit dates in the QS domain using
[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md):

\
`QS``$``SVSTDTC`` ``<-`` `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``QS``$``USUBJID``, ``QS``$``VISITNUM``, format ``=`` ``"svdtc"``)`\
`QS`\
`#>    USUBJID VISITNUM QSTESTCD QSSTRESN    SVSTDTC`\
`#> 1 SUBJ-001        1   SCORE1       85 2025-01-15`\
`#> 2 SUBJ-001        2   SCORE1       90 2025-02-20`\
`#> 3 SUBJ-002        1   SCORE1       72 2025-01-18`\
`#> 4 SUBJ-002        2   SCORE1       78 2025-02-25`\
`#> 5 SUBJ-002        3   SCORE1       NA  NOT FOUND`\
[`class`](https://rdrr.io/r/base/class.html)`(``QS``$``SVSTDTC``)``  ``# character`\
`#> [1] "character"`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

### Native Date lookup (returns Date objects)

Using `type = "Date"`, values are stored as native R `Date` objects and
[`fput()`](https://crow16384.github.io/ksformat/reference/fput.md)/[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md)
return them directly — no string conversion needed:

\
`# Create composite key -> Date mapping from SV`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(`\
`    `[`paste`](https://rdrr.io/r/base/paste.html)`(``SV``$``USUBJID``, ``SV``$``VISITNUM``, sep ``=`` ``"|"``)``,`\
`    `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``SV``$``SVSTDTC``, format ``=`` ``"%Y-%m-%d"``)`\
`  ``)``,`\
`  .other  ``=`` ``NA``,`\
`  name    ``=`` ``"svdtn"``,`\
`  type    ``=`` ``"Date"``,`\
`  ignore_case ``=`` ``TRUE`\
`)`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"svdtn"``)`\
`#> KS Format:svdtn (nocase)`\
`#> Type: Date `\
`#> Mappings:`\
`#>   SUBJ-001|1 => 2025-01-15`\
`#>   SUBJ-001|2 => 2025-02-20`\
`#>   SUBJ-001|3 => 2025-03-10`\
`#>   SUBJ-002|1 => 2025-01-18`\
`#>   SUBJ-002|2 => 2025-02-25`

\
`QS``$``SVSTDTC_DT`` ``<-`` `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``QS``$``USUBJID``, ``QS``$``VISITNUM``, format ``=`` ``"svdtn"``)`\
`QS`\
`#>    USUBJID VISITNUM QSTESTCD QSSTRESN    SVSTDTC SVSTDTC_DT`\
`#> 1 SUBJ-001        1   SCORE1       85 2025-01-15 2025-01-15`\
`#> 2 SUBJ-001        2   SCORE1       90 2025-02-20 2025-02-20`\
`#> 3 SUBJ-002        1   SCORE1       72 2025-01-18 2025-01-18`\
`#> 4 SUBJ-002        2   SCORE1       78 2025-02-25 2025-02-25`\
`#> 5 SUBJ-002        3   SCORE1       NA  NOT FOUND       <NA>`\
[`class`](https://rdrr.io/r/base/class.html)`(``QS``$``SVSTDTC_DT``)``  ``# Date`\
`#> [1] "Date"`\
\
`# Typed NA for unmatched keys (SUBJ-002 Visit 3 not in SV)`\
[`is.na`](https://rdrr.io/r/base/NA.html)`(``QS``$``SVSTDTC_DT``[``5``]``)`\
`#> [1] TRUE`\
\
`# Date arithmetic works directly`\
`QS``$``SVSTDTC_DT`` ``+`` ``7``  ``# add 7 days`\
`#> [1] "2025-01-22" "2025-02-27" "2025-01-25" "2025-03-04" NA`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 21: Consistent Data-Driven Formats with `fmap()`

When building formats from data (e.g., a data frame with 1000+ rows),
you need a named vector mapping keys to values. By default,
[`fnew()`](https://crow16384.github.io/ksformat/reference/fnew.md)
treats named vectors differently depending on the output type:

- **Value types** (`Date`, `POSIXct`, `logical`): `c(key = value)` —
  natural direction, no reversal.
- **Character / numeric**: `c(Label = "Code")` — R convention, names and
  values are **swapped** internally.

This inconsistency is confusing for data-driven formats. The
[`fmap()`](https://crow16384.github.io/ksformat/reference/fmap.md)
helper solves it: `fmap(keys, values)` works identically for **all**
types.

### Clinical-data example

Suppose we have a demographics dataset and need two lookup formats from
the same data — one returning Date objects, one returning character
strings:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ksformat`](https://crow16384.github.io/ksformat/)`)`\
\
`dm`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  USUBJID ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SUBJ-001"``, ``"SUBJ-002"``, ``"SUBJ-003"``)``,`\
`  SUBJID  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"001"``, ``"002"``, ``"003"``)``,`\
`  RFICDTC ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"2023-03-09T08:45"``, ``"2024-08-13T09:53"``, ``"2025-06-17T09:03"``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`# Composite key for both formats`\
`keys`` ``<-`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``dm``$``USUBJID``, ``dm``$``SUBJID``, sep ``=`` ``"|"``)`

### Same `fmap(keys, values)` pattern for both types

Both formats use the **identical** calling style — `fmap(keys, values)`
where keys are input lookup values and values are output objects:

\
`# Date lookup`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(``keys``, `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(``dm``$``RFICDTC``, format ``=`` ``"%Y-%m-%d"``)``)``,`\
`  .other      ``=`` ``NA``,`\
`  type        ``=`` ``"Date"``,`\
`  ignore_case ``=`` ``TRUE``,`\
`  name        ``=`` ``"icdtn"`\
`)`\
\
`# Character lookup — same fmap(keys, values) pattern!`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(``keys``, ``dm``$``RFICDTC``)``,`\
`  .other      ``=`` ``"NOT FOUND"``,`\
`  type        ``=`` ``"character"``,`\
`  ignore_case ``=`` ``TRUE``,`\
`  name        ``=`` ``"icdtc"`\
`)`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"icdtn"``)`\
`#> KS Format:icdtn (nocase)`\
`#> Type: Date `\
`#> Mappings:`\
`#>   SUBJ-001|001 => 2023-03-09`\
`#>   SUBJ-002|002 => 2024-08-13`\
`#>   SUBJ-003|003 => 2025-06-17`\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"icdtc"``)`\
`#> KS Format:icdtc (nocase)`\
`#> Type: character `\
`#> Mappings:`\
`#>   SUBJ-001|001 => 2023-03-09T08:45`\
`#>   SUBJ-002|002 => 2024-08-13T09:53`\
`#>   SUBJ-003|003 => 2025-06-17T09:03`\
`#>   .other => NOT FOUND`

\
`# Both return the expected results`\
[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``"SUBJ-001"``, ``"001"``, format ``=`` ``"icdtn"``)`\
`#> [1] "2023-03-09"`\
[`class`](https://rdrr.io/r/base/class.html)`(`[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``"SUBJ-001"``, ``"001"``, format ``=`` ``"icdtn"``)``)`\
`#> [1] "Date"`\
\
[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``"SUBJ-001"``, ``"001"``, format ``=`` ``"icdtc"``)`\
`#> [1] "2023-03-09T08:45"`\
[`class`](https://rdrr.io/r/base/class.html)`(`[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``"SUBJ-001"``, ``"001"``, format ``=`` ``"icdtc"``)``)`\
`#> [1] "character"`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

No extra parameters needed —
[`fmap()`](https://crow16384.github.io/ksformat/reference/fmap.md) tells
[`fnew()`](https://crow16384.github.io/ksformat/reference/fnew.md) to
use the natural direction for all types.

### When to use the default (reversal on)

The default auto-reversal preserves the standard R convention where
`c(Label = "Code")` maps `Code -> Label`. This is natural for
hand-written formats:

\
`# These are equivalent — both map "M" -> "Male"`\
`fmt_a`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``Male ``=`` ``"M"``, Female ``=`` ``"F"``)``)`\
`fmt_b`` ``<-`` `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"M"`` ``=`` ``"Male"``, ``"F"`` ``=`` ``"Female"``)`\
\
[`identical`](https://rdrr.io/r/base/identical.html)`(`[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``)``, ``fmt_a``)``, `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"M"``, ``"F"``)``, ``fmt_b``)``)`\
`#> [1] TRUE`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

### Summary

| Use case | Style | Reversal |
|:---|:---|:---|
| Data-driven (any type) | `fmap(keys, values)` | Suppressed |
| Hand-written (char/num) | `c(Label = "Code")` or `"Code" = "Label"` | Auto (default) |
| Value types (`Date`, etc.) | `fmap(keys, values)` or `setNames(values, keys)` | No reversal (default) |

## Example 22: Date Lookup via `fparse()` and `fputk()`

Examples 20–21 built composite-key formats programmatically with
[`fnew()`](https://crow16384.github.io/ksformat/reference/fnew.md) and
[`fmap()`](https://crow16384.github.io/ksformat/reference/fmap.md). When
the mapping is **small and known in advance** (e.g., a study-specific
visit schedule), you can define the same lookup entirely in text with
[`fparse()`](https://crow16384.github.io/ksformat/reference/fparse.md).

### Character date lookup

The simplest approach: store dates as character strings using a regular
`character` format.

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE svdtc (character, nocase)`\
`  "SUBJ-001|1" = "2025-01-15"`\
`  "SUBJ-001|2" = "2025-02-20"`\
`  "SUBJ-001|3" = "2025-03-10"`\
`  "SUBJ-002|1" = "2025-01-18"`\
`  "SUBJ-002|2" = "2025-02-25"`\
`  .other       = "NOT FOUND"`\
`;`\
`'``)`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"svdtc"``)`\
`#> KS Format:svdtc (nocase)`\
`#> Type: character `\
`#> Mappings:`\
`#>   SUBJ-001|1 => 2025-01-15`\
`#>   SUBJ-001|2 => 2025-02-20`\
`#>   SUBJ-001|3 => 2025-03-10`\
`#>   SUBJ-002|1 => 2025-01-18`\
`#>   SUBJ-002|2 => 2025-02-25`\
`#>   .other => NOT FOUND`

Apply with
[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md) to
look up visit dates from a questionnaire domain:

\
`QS`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  USUBJID  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SUBJ-001"``, ``"SUBJ-001"``, ``"SUBJ-002"``, ``"SUBJ-002"``, ``"SUBJ-002"``)``,`\
`  VISITNUM ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``2``, ``1``, ``2``, ``3``)``,`\
`  QSSTRESN ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``85``, ``90``, ``72``, ``78``, ``NA``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
\
`QS``$``SVSTDTC`` ``<-`` `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``QS``$``USUBJID``, ``QS``$``VISITNUM``, format ``=`` ``"svdtc"``)`\
`QS`\
`#>    USUBJID VISITNUM QSSTRESN    SVSTDTC`\
`#> 1 SUBJ-001        1       85 2025-01-15`\
`#> 2 SUBJ-001        2       90 2025-02-20`\
`#> 3 SUBJ-002        1       72 2025-01-18`\
`#> 4 SUBJ-002        2       78 2025-02-25`\
`#> 5 SUBJ-002        3       NA  NOT FOUND`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

### Native Date lookup

Use the `Date` value type with `format:` to store dates as native R
`Date` objects. The `format:` parameter tells
[`fparse()`](https://crow16384.github.io/ksformat/reference/fparse.md)
how to parse the date strings in the text block:

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE svdtn (Date, format: %Y-%m-%d, nocase)`\
`  "SUBJ-001|1" = "2025-01-15"`\
`  "SUBJ-001|2" = "2025-02-20"`\
`  "SUBJ-001|3" = "2025-03-10"`\
`  "SUBJ-002|1" = "2025-01-18"`\
`  "SUBJ-002|2" = "2025-02-25"`\
`;`\
`'``)`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"svdtn"``)`\
`#> KS Format:svdtn (nocase)`\
`#> Type: Date `\
`#> Mappings:`\
`#>   SUBJ-001|1 => 2025-01-15`\
`#>   SUBJ-001|2 => 2025-02-20`\
`#>   SUBJ-001|3 => 2025-03-10`\
`#>   SUBJ-002|1 => 2025-01-18`\
`#>   SUBJ-002|2 => 2025-02-25`

Now [`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md)
returns real `Date` objects — arithmetic and comparison work directly:

\
`QS``$``SVSTDTC_DT`` ``<-`` `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``QS``$``USUBJID``, ``QS``$``VISITNUM``, format ``=`` ``"svdtn"``)`\
`QS`\
`#>    USUBJID VISITNUM QSSTRESN    SVSTDTC SVSTDTC_DT`\
`#> 1 SUBJ-001        1       85 2025-01-15 2025-01-15`\
`#> 2 SUBJ-001        2       90 2025-02-20 2025-02-20`\
`#> 3 SUBJ-002        1       72 2025-01-18 2025-01-18`\
`#> 4 SUBJ-002        2       78 2025-02-25 2025-02-25`\
`#> 5 SUBJ-002        3       NA  NOT FOUND       <NA>`\
\
[`class`](https://rdrr.io/r/base/class.html)`(``QS``$``SVSTDTC_DT``)``         ``# Date`\
`#> [1] "Date"`\
[`is.na`](https://rdrr.io/r/base/NA.html)`(``QS``$``SVSTDTC_DT``[``5``]``)``      ``# TRUE — no match for SUBJ-002 Visit 3`\
`#> [1] TRUE`\
\
`# Date arithmetic works directly`\
`QS``$``SVSTDTC_DT`` ``+`` ``7`\
`#> [1] "2025-01-22" "2025-02-27" "2025-01-25" "2025-03-04" NA`

### Round-trip: export and re-import

Formats created with
[`fparse()`](https://crow16384.github.io/ksformat/reference/fparse.md)
can be exported back to text with
[`fexport()`](https://crow16384.github.io/ksformat/reference/fexport.md)
and re-parsed — useful for version-controlled format definitions:

\
`fmt_obj`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"svdtn"``)`\
`txt`` ``<-`` `[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``svdtn ``=`` ``fmt_obj``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(``txt``)`\
`#> VALUE svdtn (Date, nocase, format: %Y-%m-%d)`\
`#>   "SUBJ-001|1" = "2025-01-15"`\
`#>   "SUBJ-001|2" = "2025-02-20"`\
`#>   "SUBJ-001|3" = "2025-03-10"`\
`#>   "SUBJ-002|1" = "2025-01-18"`\
`#>   "SUBJ-002|2" = "2025-02-25"`\
`#> ;`

\
`# Re-parse the exported text`\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``txt``)`\
\
`# Verify it still works`\
[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``"SUBJ-001"``, ``2``, format ``=`` ``"svdtn"``)`\
`#> [1] "2025-02-20"`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 23: Inspecting Range Rules with `franges()`

[`franges()`](https://crow16384.github.io/ksformat/reference/franges.md)
extracts all range-based mappings from a format and returns them as a
tidy `data.frame` — useful for auditing, documentation, or downstream
processing.

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE age (numeric)`\
`  [0, 18)    = "Child"`\
`  [18, 65)   = "Adult"`\
`  [65, HIGH] = "Senior"`\
`  .missing   = "Unknown"`\
`;`\
`'``)`\
\
[`franges`](https://crow16384.github.io/ksformat/reference/franges.md)`(``"age"``)`\
`#>   low high inc_low inc_high  label`\
`#> 1   0   18    TRUE    FALSE  Child`\
`#> 2  18   65    TRUE    FALSE  Adult`\
`#> 3  65  Inf    TRUE     TRUE Senior`

You can use the result like any data frame — filter, display, or feed
into further calculations:

\
`df`` ``<-`` `[`franges`](https://crow16384.github.io/ksformat/reference/franges.md)`(``"age"``)`\
\
`# Which ranges have a finite upper bound?`\
`df``[`[`is.finite`](https://rdrr.io/r/base/is.finite.html)`(``df``$``high``)``, ``]`\
`#>   low high inc_low inc_high label`\
`#> 1   0   18    TRUE    FALSE Child`\
`#> 2  18   65    TRUE    FALSE Adult`

[`franges()`](https://crow16384.github.io/ksformat/reference/franges.md)
silently excludes discrete entries (`.missing`, `.other`, plain string
keys) — only range rows appear. It returns an empty `data.frame` with
the same columns when the format contains no ranges.

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"M"`` ``=`` ``"Male"``, ``"F"`` ``=`` ``"Female"``, .missing ``=`` ``"Unknown"``, name ``=`` ``"sex"``)`\
[`franges`](https://crow16384.github.io/ksformat/reference/franges.md)`(``"sex"``)``   ``# 0 rows`\
`#> [1] low      high     inc_low  inc_high label   `\
`#> <0 rows> (or 0-length row.names)`

### Completing a Summary Against Format Labels

You can also use the labels returned by
[`franges()`](https://crow16384.github.io/ksformat/reference/franges.md)
as the full category set for
[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html),
without converting the column to a factor first.

\
`age_levels`` ``<-`` `[`franges`](https://crow16384.github.io/ksformat/reference/franges.md)`(``"age"``)``$``label`\
\
`ages`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``17``, ``18``, ``42``)`\
\
`dplyr``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`  age_group ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``ages``, ``"age"``)`\
`)`` ``|>`\
`  ``dplyr``::`[`count`](https://dplyr.tidyverse.org/reference/count.html)`(``age_group``, name ``=`` ``"n"``)`` ``|>`\
`  ``tidyr``::`[`complete`](https://tidyr.tidyverse.org/reference/complete.html)`(``age_group ``=`` ``age_levels``, fill ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``n ``=`` ``0``)``)`\
`#> ``# A tibble: 3 × 2`\
`#>   age_group     n`\
`#>   ``<chr>``     ``<int>`\
`#> ``1`` Adult         2`\
`#> ``2`` Child         2`\
`#> ``3`` Senior        0`

### Getting Unique Labels from a Format

If a format is already defined, you can inspect its label values
directly from the stored mappings. For this `aesev` example, the labels
are plain character values, so
[`unique()`](https://rdrr.io/r/base/unique.html) gives the full set
without needing [`factor()`](https://rdrr.io/r/base/factor.html).

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(`\
`    ``1``:``5``,`\
`    `[`c`](https://rdrr.io/r/base/c.html)`(``"Mild"``, ``"Moderate"``, ``"Severe"``, ``"Life-threatening"``, ``"Death"``)`\
`  ``)``,`\
`  type ``=`` ``"numeric"``,`\
`  name ``=`` ``"aesev"`\
`)`\
\
`aesev_labels`` ``<-`` `[`unique`](https://rdrr.io/r/base/unique.html)`(`[`unlist`](https://rdrr.io/r/base/unlist.html)`(`[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"aesev"``)``$``mappings``, use.names ``=`` ``FALSE``)``)`\
`aesev_labels`\
`#> [1] "Mild"             "Moderate"         "Severe"           "Life-threatening"`\
`#> [5] "Death"`

## Example 24: Reverse Range Lookup with `fmap_to_ranges()`

When a range format stores **numeric codes** as its labels (e.g. visit
windows coded as weeks),
[`fmap_to_ranges()`](https://crow16384.github.io/ksformat/reference/fmap_to_ranges.md)
turns a vector of those codes back into the original `[low, high]`
bounds — one row per input value.

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE visit_ther (numeric)`\
`  [LOW,  1] =  0`\
`  [ 8, 22] =  2`\
`  [22, 36] =  4`\
`  [37, 50] =  6`\
`  [51, 63] =  8`\
`  [64, 78] = 10`\
`  [79, 91] = 12`\
`;`\
`'``)`\
\
`coded_weeks`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``2``, ``4``, ``6``, ``8``, ``10``, ``12``)`\
[`fmap_to_ranges`](https://crow16384.github.io/ksformat/reference/fmap_to_ranges.md)`(``coded_weeks``, ``"visit_ther"``)`\
`#>    low high inc_low inc_high`\
`#> 1 -Inf    1    TRUE     TRUE`\
`#> 2    8   22    TRUE     TRUE`\
`#> 3   22   36    TRUE     TRUE`\
`#> 4   37   50    TRUE     TRUE`\
`#> 5   51   63    TRUE     TRUE`\
`#> 6   64   78    TRUE     TRUE`\
`#> 7   79   91    TRUE     TRUE`

Unmatched values produce `NA` rows, making it safe to pass arbitrary
vectors:

\
[`fmap_to_ranges`](https://crow16384.github.io/ksformat/reference/fmap_to_ranges.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``2``, ``99``, ``4``)``, ``"visit_ther"``)`\
`#>   low high inc_low inc_high`\
`#> 1   8   22    TRUE     TRUE`\
`#> 2  NA   NA      NA       NA`\
`#> 3  22   36    TRUE     TRUE`

## Example 25: Date Range Bucketing

`date_range` and `datetime_range` formats bucket `Date` or `POSIXct`
input into character labels using ISO date/datetime interval bounds.
They reuse the same range-table engine as numeric ranges, so the
[`findInterval()`](https://rdrr.io/r/base/findInterval.html) fast path
is active for sorted, disjoint buckets.

### Fiscal-year bucketing

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"2023-01-01,2024-01-01,TRUE,FALSE"`` ``=`` ``"FY23"``,`\
`  ``"2024-01-01,2025-01-01,TRUE,FALSE"`` ``=`` ``"FY24"``,`\
`  ``"2025-01-01,2026-01-01,TRUE,FALSE"`` ``=`` ``"FY25"``,`\
`  type ``=`` ``"date_range"``,`\
`  name ``=`` ``"fiscal_year"`\
`)`\
\
`dates`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2023-06-15"``, ``"2024-03-01"``, ``"2024-12-31"``,`\
`                   ``"2025-07-04"``, ``"2022-01-01"``, ``NA``)``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  date  ``=`` ``dates``,`\
`  fy    ``=`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``dates``, ``"fiscal_year"``)`\
`)`\
`#>         date         fy`\
`#> 1 2023-06-15       FY23`\
`#> 2 2024-03-01       FY24`\
`#> 3 2024-12-31       FY24`\
`#> 4 2025-07-04       FY25`\
`#> 5 2022-01-01 2022-01-01`\
`#> 6       <NA>       <NA>`

### Define from text with `fparse()`

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE quarter (date_range)`\
`  [2024-01-01, 2024-04-01) = "Q1-2024"`\
`  [2024-04-01, 2024-07-01) = "Q2-2024"`\
`  [2024-07-01, 2024-10-01) = "Q3-2024"`\
`  [2024-10-01, 2025-01-01) = "Q4-2024"`\
`  .other                   = "Outside 2024"`\
`;`\
`'``)`\
\
`sample_dates`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-02-14"``, ``"2024-05-20"``, ``"2024-08-08"``,`\
`                          ``"2024-11-30"``, ``"2025-03-01"``)``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  date    ``=`` ``sample_dates``,`\
`  quarter ``=`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``sample_dates``, ``"quarter"``)`\
`)`\
`#>         date      quarter`\
`#> 1 2024-02-14      Q1-2024`\
`#> 2 2024-05-20      Q2-2024`\
`#> 3 2024-08-08      Q3-2024`\
`#> 4 2024-11-30      Q4-2024`\
`#> 5 2025-03-01 Outside 2024`

### `LOW` / `HIGH` open-ended bounds

`LOW` and `HIGH` represent $`-\infty`$ and $`+\infty`$ — any date before
or after a cutpoint falls in the open arm.

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE era (date_range)`\
`  [LOW,        2000-01-01) = "Pre-2000"`\
`  [2000-01-01, 2010-01-01) = "2000s"`\
`  [2010-01-01, 2020-01-01) = "2010s"`\
`  [2020-01-01, HIGH]       = "2020+"`\
`;`\
`'``)`\
\
`event_dates`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"1985-07-04"``, ``"2005-12-25"``,`\
`                         ``"2015-06-01"``, ``"2023-11-11"``)``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  date ``=`` ``event_dates``,`\
`  era  ``=`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``event_dates``, ``"era"``)`\
`)`\
`#>         date      era`\
`#> 1 1985-07-04 Pre-2000`\
`#> 2 2005-12-25    2000s`\
`#> 3 2015-06-01    2010s`\
`#> 4 2023-11-11    2020+`

### Export and roundtrip

Formats export with ISO date bounds and re-parse without loss:

\
`q_obj`` ``<-`` `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"quarter"``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(`[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``quarter ``=`` ``q_obj``)``)`\
`#> VALUE quarter (date_range)`\
`#>   [2024-01-01, 2024-04-01) = "Q1-2024"`\
`#>   [2024-04-01, 2024-07-01) = "Q2-2024"`\
`#>   [2024-07-01, 2024-10-01) = "Q3-2024"`\
`#>   [2024-10-01, 2025-01-01) = "Q4-2024"`\
`#>   .other = "Outside 2024"`\
`#> ;`

\
`# Re-parse the exported text`\
`txt`` ``<-`` `[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(``quarter ``=`` ``q_obj``)`\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``txt``)`\
\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-02-14"``, ``"2024-08-08"``)``)``, ``"quarter"``)`\
`#> [1] "Q1-2024" "Q3-2024"`

### Overlapping buckets with `multilabel` and `fput_all()`

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE study_window (date_range, multilabel)`\
`  [2024-01-01, 2024-07-01) = "First Half"`\
`  [2024-04-01, 2024-10-01) = "Mid-Year"`\
`  [2024-07-01, 2025-01-01) = "Second Half"`\
`;`\
`'``)`\
\
`checkup_dates`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-02-15"``, ``"2024-05-20"``, ``"2024-09-01"``)``)`\
`all_windows``   ``<-`` `[`fput_all`](https://crow16384.github.io/ksformat/reference/fput_all.md)`(``checkup_dates``, ``"study_window"``)`\
\
`for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``checkup_dates``)``)`` ``{`\
`  `[`cat`](https://rdrr.io/r/base/cat.html)`(`[`format`](https://rdrr.io/r/base/format.html)`(``checkup_dates``[``i``]``)``, ``"->"``,`\
`      `[`paste`](https://rdrr.io/r/base/paste.html)`(``all_windows``[[``i``]``]``, collapse ``=`` ``" | "``)``, ``"\n"``)`\
`}`\
`#> 2024-02-15 -> First Half `\
`#> 2024-05-20 -> First Half | Mid-Year `\
`#> 2024-09-01 -> Mid-Year | Second Half`

### Auto-detection of type

When no explicit type is given,
[`fparse()`](https://crow16384.github.io/ksformat/reference/fparse.md)
infers `date_range` from ISO date bounds and `datetime_range` when
bounds include a time component:

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE auto_fy`\
`  [2024-01-01, 2025-01-01) = "2024"`\
`;`\
\
`VALUE auto_shift`\
`  [2024-01-15 08:00, 2024-01-15 16:00) = "Day shift"`\
`;`\
`'``)`\
\
[`cat`](https://rdrr.io/r/base/cat.html)`(``"auto_fy type   :"``, `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"auto_fy"``)``$``type``, ``"\n"``)`\
`#> auto_fy type   : date_range`\
[`cat`](https://rdrr.io/r/base/cat.html)`(``"auto_shift type:"``, `[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"auto_shift"``)``$``type``, ``"\n"``)`\
`#> auto_shift type: datetime_range`

### Datetime range bucketing

`datetime_range` works identically to `date_range` but matches against
POSIXct values. Bounds are expressed as `YYYY-MM-DD HH:MM[:SS]` strings.

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE shift (datetime_range)`\
`  [2024-01-15 00:00, 2024-01-15 08:00) = "Night"`\
`  [2024-01-15 08:00, 2024-01-15 16:00) = "Day"`\
`  [2024-01-15 16:00, 2024-01-16 00:00) = "Evening"`\
`;`\
`'``)`\
\
`timestamps`` ``<-`` `[`as.POSIXct`](https://rdrr.io/r/base/as.POSIXlt.html)`(`\
`  `[`c`](https://rdrr.io/r/base/c.html)`(``"2024-01-15 03:22:00"``, ``"2024-01-15 11:45:00"``,`\
`    ``"2024-01-15 19:00:00"``)``,`\
`  tz ``=`` ``"UTC"`\
`)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  ts    ``=`` `[`format`](https://rdrr.io/r/base/format.html)`(``timestamps``, tz ``=`` ``"UTC"``)``,`\
`  shift ``=`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``timestamps``, ``"shift"``)`\
`)`\
`#>                    ts   shift`\
`#> 1 2024-01-15 03:22:00   Night`\
`#> 2 2024-01-15 11:45:00     Day`\
`#> 3 2024-01-15 19:00:00 Evening`

## Example 26: Stratified Range Lookup with `fputk()`

The `stratified_range` type combines a discrete stratum (such as a study
arm, subject id, or any composite key) with a numeric / Date / POSIXct
range. Each stratum has its own bucket boundaries, and
[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md)
dispatches to the right bucket for each row.

### Programmatic construction with `fmap_strata()`

\
`visits`` ``<-`` `[`fmap_strata`](https://crow16384.github.io/ksformat/reference/fmap_strata.md)`(`\
`  stratum ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM_A"``, ``"ARM_A"``, ``"ARM_A"``, ``"ARM_B"``, ``"ARM_B"``)``,`\
`  low     ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``,        ``7``,       ``28``,      ``0``,       ``14``)``,`\
`  high    ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``7``,        ``28``,      ``Inf``,     ``14``,      ``Inf``)``,`\
`  label   ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Baseline"``, ``"Wk1-3"``, ``"Wk4+"``, ``"Baseline"``, ``"Wk2+"``)``,`\
`  inc_high ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``FALSE``, ``FALSE``, ``TRUE``, ``FALSE``, ``TRUE``)`\
`)`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``visits``, type ``=`` ``"stratified_range"``,`\
`     ``".other|ARM_A"`` ``=`` ``"A_outside"``,`\
`     .other ``=`` ``"outside_window"``,`\
`     name ``=`` ``"vw"``)`\
\
`df`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  arm ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ARM_A"``, ``"ARM_A"``, ``"ARM_B"``, ``"ARM_B"``, ``"ARM_C"``)``,`\
`  day ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``3``,        ``35``,      ``5``,       ``40``,      ``10``)`\
`)`\
`df``$``visit`` ``<-`` `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``df``$``arm``, ``df``$``day``, format ``=`` ``"vw"``)`\
`df`\
`#>     arm day          visit`\
`#> 1 ARM_A   3       Baseline`\
`#> 2 ARM_A  35           Wk4+`\
`#> 3 ARM_B   5       Baseline`\
`#> 4 ARM_B  40           Wk2+`\
`#> 5 ARM_C  10 outside_window`

### Text definition with `fparse()`

\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``'`\
`VALUE vw_text (stratified_range, range_subtype: numeric)`\
`  "ARM_A"|[0, 7)    = "Baseline"`\
`  "ARM_A"|[7, 28)   = "Wk1-3"`\
`  "ARM_A"|[28, HIGH]= "Wk4+"`\
`  "ARM_B"|[0, 14)   = "Baseline"`\
`  "ARM_B"|[14, HIGH]= "Wk2+"`\
`  ".other|ARM_A"    = "A_outside"`\
`  .other            = "outside_window"`\
`  ;`\
`'``)`\
[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``df``$``arm``, ``df``$``day``, format ``=`` ``"vw_text"``)`\
`#> [1] "Baseline"       "Wk4+"           "Baseline"       "Wk2+"          `\
`#> [5] "outside_window"`

### Date subtype: per-subject windows

\
`windows`` ``<-`` `[`fmap_strata`](https://crow16384.github.io/ksformat/reference/fmap_strata.md)`(`\
`  stratum ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"S001"``, ``"S001"``, ``"S002"``, ``"S002"``)``,`\
`  low     ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-01-01"``, ``"2024-01-15"``,`\
`                       ``"2024-02-01"``, ``"2024-02-20"``)``)``,`\
`  high    ``=`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-01-15"``, ``"2024-02-01"``,`\
`                       ``"2024-02-20"``, ``"2024-03-10"``)``)``,`\
`  label   ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Screen"``, ``"Treat"``, ``"Screen"``, ``"Treat"``)`\
`)`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``windows``, type ``=`` ``"stratified_range"``, range_subtype ``=`` ``"date"``,`\
`     .other ``=`` ``"off-window"``, name ``=`` ``"win"``)`\
\
`subj``   ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"S001"``, ``"S001"``, ``"S002"``, ``"S002"``, ``"S003"``)`\
`visits`` ``<-`` `[`as.Date`](https://rdrr.io/r/base/as.Date.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"2024-01-05"``, ``"2024-01-20"``,`\
`                     ``"2024-02-10"``, ``"2024-03-01"``, ``"2024-01-01"``)``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  subj  ``=`` ``subj``,`\
`  date  ``=`` ``visits``,`\
`  phase ``=`` `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``subj``, ``visits``, format ``=`` ``"win"``)`\
`)`\
`#>   subj       date      phase`\
`#> 1 S001 2024-01-05     Screen`\
`#> 2 S001 2024-01-20      Treat`\
`#> 3 S002 2024-02-10     Screen`\
`#> 4 S002 2024-03-01      Treat`\
`#> 5 S003 2024-01-01 off-window`

### Roundtrip via `fexport()` / `fparse()`

\
`txt`` ``<-`` `[`fexport`](https://crow16384.github.io/ksformat/reference/fexport.md)`(`[`format_get`](https://crow16384.github.io/ksformat/reference/format_get.md)`(``"vw"``)``)`\
[`cat`](https://rdrr.io/r/base/cat.html)`(``txt``, ``"\n"``)`\
`#> VALUE vw (stratified_range, range_subtype: numeric, strata_sep: |)`\
`#>   "ARM_A"|[0, 7) = "Baseline"`\
`#>   "ARM_A"|[7, 28) = "Wk1-3"`\
`#>   "ARM_A"|[28, HIGH] = "Wk4+"`\
`#>   ".other|ARM_A" = "A_outside"`\
`#>   "ARM_B"|[0, 14) = "Baseline"`\
`#>   "ARM_B"|[14, HIGH] = "Wk2+"`\
`#>   .other = "outside_window"`\
`#> ;`\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`\
[`fparse`](https://crow16384.github.io/ksformat/reference/fparse.md)`(``text ``=`` ``txt``)`\
[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``df``$``arm``, ``df``$``day``, format ``=`` ``"vw"``)`\
`#> [1] "Baseline"       "Wk4+"           "Baseline"       "Wk2+"          `\
`#> [5] "outside_window"`

## Example 27: Plain Range Lookup with `fmap_ranges()`

For non-stratified numeric / Date ranges,
[`fmap_ranges()`](https://crow16384.github.io/ksformat/reference/fmap_ranges.md)
saves you from hand-crafting canonical keys.

\
`age_groups`` ``<-`` `[`fmap_ranges`](https://crow16384.github.io/ksformat/reference/fmap_ranges.md)`(`\
`  low   ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``18``, ``65``)``,`\
`  high  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``18``, ``65``, ``Inf``)``,`\
`  label ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Child"``, ``"Adult"``, ``"Senior"``)``,`\
`  inc_high ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``FALSE``, ``FALSE``, ``TRUE``)`\
`)`\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``age_groups``, type ``=`` ``"numeric"``, name ``=`` ``"ag"``)`\
[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``25``, ``90``)``, ``"ag"``)`\
`#> [1] "Child"  "Adult"  "Senior"`\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

## Example 28: Composite Key Lookup with NA Components (`na_as_string`)

When building a format from data using
`fmap(paste(..., sep = "|"), values)`, base R’s
[`paste()`](https://rdrr.io/r/base/paste.html) converts any `NA`
component to the **literal string** `"NA"`. The resulting composite key
is therefore `"CAT|TEST|NA"`, not a missing value.

By default,
[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md)
restores `NA_character_` wherever any component is `NA` before the
lookup — so the key `"CAT|TEST|NA"` is never reached and the row falls
through to `.other` / `.missing`.

Setting `na_as_string = TRUE` keeps
[`paste()`](https://rdrr.io/r/base/paste.html)’s literal `"NA"`, making
the round-trip consistent.

### Clinical example — LB parameter derivation

A common ADaM task: derive `PARAMCD` from a combination of `LBCAT`,
`LBSPEC`, `LBTESTCD`, and `LBSTRESU`, where some rows have
`LBSTRESU = NA` (dimensionless tests such as INR).

\
`# Source lab mapping (as received from a specification)`\
`lb_map`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  LBCAT    ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"BLOOD CHEMISTRY"``, ``"COAGULOGRAM"``, ``"COAGULATION PANEL"``, ``"COAGULOGRAM"``)``,`\
`  LBSPEC   ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"BLOOD"``,           ``"BLOOD"``,        ``"BLOOD"``,             ``"BLOOD"``)``,`\
`  LBTESTCD ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ALB"``,             ``"FIBRINO"``,      ``"INR"``,               ``"INR"``)``,`\
`  LBSTRESU ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"g/L"``,             ``"g/L"``,           ``NA``,                  ``NA``)``,`\
`  PARAMCD  ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ALB"``,             ``"FIBRINO"``,       ``"INR"``,               ``"INR"``)``,`\
`  stringsAsFactors ``=`` ``FALSE`\
`)`\
`lb_map`\
`#>               LBCAT LBSPEC LBTESTCD LBSTRESU PARAMCD`\
`#> 1   BLOOD CHEMISTRY  BLOOD      ALB      g/L     ALB`\
`#> 2       COAGULOGRAM  BLOOD  FIBRINO      g/L FIBRINO`\
`#> 3 COAGULATION PANEL  BLOOD      INR     <NA>     INR`\
`#> 4       COAGULOGRAM  BLOOD      INR     <NA>     INR`

Build the format with `fmap(paste(...), PARAMCD)`.\
[`paste()`](https://rdrr.io/r/base/paste.html) converts `NA` in
`LBSTRESU` to `"NA"`, so the stored keys for INR rows are
`"COAGULATION PANEL|BLOOD|INR|NA"` and `"COAGULOGRAM|BLOOD|INR|NA"`.

\
[`with`](https://rdrr.io/r/base/with.html)`(``lb_map``,`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(``LBCAT``, ``LBSPEC``, ``LBTESTCD``, ``LBSTRESU``, sep ``=`` ``"|"``)``, ``PARAMCD``)`\
`)`` ``|>`\
`  `[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``ignore_case ``=`` ``TRUE``, .other ``=`` ``NA``,`\
`       type ``=`` ``"character"``, name ``=`` ``"lb_param"``)`\
\
[`fprint`](https://crow16384.github.io/ksformat/reference/fprint.md)`(``"lb_param"``)`\
`#> KS Format:lb_param (nocase)`\
`#> Type: character `\
`#> Mappings:`\
`#>   BLOOD CHEMISTRY|BLOOD|ALB|g/L => ALB`\
`#>   COAGULOGRAM|BLOOD|FIBRINO|g/L => FIBRINO`\
`#>   COAGULATION PANEL|BLOOD|INR|NA => INR`\
`#>   COAGULOGRAM|BLOOD|INR|NA => INR`\
`#>   .other => NA`

Now apply the format.\
With the default `na_as_string = FALSE`, the INR rows get `NA` (no
match):

\
`lb_map``$``PARAMCD_default`` ``<-`` `[`with`](https://rdrr.io/r/base/with.html)`(``lb_map``,`\
`  `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``LBCAT``, ``LBSPEC``, ``LBTESTCD``, ``LBSTRESU``, format ``=`` ``"lb_param"``)`\
`)`\
`lb_map``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"LBTESTCD"``, ``"LBSTRESU"``, ``"PARAMCD"``, ``"PARAMCD_default"``)``]`\
`#>   LBTESTCD LBSTRESU PARAMCD PARAMCD_default`\
`#> 1      ALB      g/L     ALB             ALB`\
`#> 2  FIBRINO      g/L FIBRINO         FIBRINO`\
`#> 3      INR     <NA>     INR            <NA>`\
`#> 4      INR     <NA>     INR            <NA>`

With `na_as_string = TRUE`,
[`paste()`](https://rdrr.io/r/base/paste.html) also converts the
lookup-side `NA` to `"NA"`, so the keys match:

\
`lb_map``$``PARAMCD_back`` ``<-`` `[`with`](https://rdrr.io/r/base/with.html)`(``lb_map``,`\
`  `[`fputk`](https://crow16384.github.io/ksformat/reference/fputk.md)`(``LBCAT``, ``LBSPEC``, ``LBTESTCD``, ``LBSTRESU``,`\
`        format ``=`` ``"lb_param"``, na_as_string ``=`` ``TRUE``)`\
`)`\
`lb_map``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"LBTESTCD"``, ``"LBSTRESU"``, ``"PARAMCD"``, ``"PARAMCD_back"``)``]`\
`#>   LBTESTCD LBSTRESU PARAMCD PARAMCD_back`\
`#> 1      ALB      g/L     ALB          ALB`\
`#> 2  FIBRINO      g/L FIBRINO      FIBRINO`\
`#> 3      INR     <NA>     INR          INR`\
`#> 4      INR     <NA>     INR          INR`

### The rule of thumb

> Use `na_as_string = TRUE` whenever the format was built with\
> `fmap(paste(...), values)` **and** any key column can contain `NA`.

If the format keys were set by hand
(`fnew("CAT|TEST|g/L" = "ALB", ...)`), `NA` components should still go
through `.missing` — keep the default `na_as_string = FALSE`.

## Example 29: Composite Label Invalue Lookup with `finputk()`

[`finputk()`](https://crow16384.github.io/ksformat/reference/finputk.md)
is the invalue-side mirror of
[`fputk()`](https://crow16384.github.io/ksformat/reference/fputk.md): it
pastes multiple columns into a composite label and reverse-looks it up
in a `ks_invalue` format. The same `na_as_string` argument applies.

### Basic usage

\
`# Build an INVALUE from two-column composite labels`\
[`finput`](https://crow16384.github.io/ksformat/reference/finput.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"BLOOD CHEMISTRY"``, ``"COAGULOGRAM"``, ``"COAGULATION PANEL"``)``,`\
`             `[`c`](https://rdrr.io/r/base/c.html)`(``"ALB"``,             ``"FIBRINO"``,      ``"INR"``)``,`\
`             sep ``=`` ``"|"``)``,`\
`       `[`c`](https://rdrr.io/r/base/c.html)`(``1L``, ``2L``, ``3L``)``)``,`\
`  target_type ``=`` ``"integer"``,`\
`  name ``=`` ``"lb_code_inv"`\
`)`\
`#> KS Invalue: lb_code_inv`\
`#> Target Type: integer `\
`#> Mappings:`\
`#>   BLOOD CHEMISTRY|ALB => 1`\
`#>   COAGULOGRAM|FIBRINO => 2`\
`#>   COAGULATION PANEL|INR => 3`\
\
`# Reverse lookup: two separate columns → integer code`\
`cat_vec``  ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"BLOOD CHEMISTRY"``, ``"COAGULOGRAM"``, ``"COAGULATION PANEL"``, ``"OTHER"``)`\
`test_vec`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"ALB"``,              ``"FIBRINO"``,     ``"INR"``,               ``"X"``)`\
\
[`finputk`](https://crow16384.github.io/ksformat/reference/finputk.md)`(``cat_vec``, ``test_vec``, invalue_name ``=`` ``"lb_code_inv"``)`\
`#> [1]  1  2  3 NA`\
`# BLOOD CHEMISTRY|ALB → 1, COAGULOGRAM|FIBRINO → 2,`\
`# COAGULATION PANEL|INR → 3, OTHER|X → NA (no match → missing_value)`\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

### With NA components (`na_as_string = TRUE`)

When the INVALUE was built from data containing `NA` columns, use
`na_as_string = TRUE` on both the build side (`fmap(paste(...), ...)`)
and the lookup side (`finputk(..., na_as_string = TRUE)`).

\
`# INVALUE where LBSTRESU can be NA (like INR)`\
[`finput`](https://crow16384.github.io/ksformat/reference/finput.md)`(`\
`  `[`fmap`](https://crow16384.github.io/ksformat/reference/fmap.md)`(`\
`    `[`paste`](https://rdrr.io/r/base/paste.html)`(``lb_map``$``LBCAT``, ``lb_map``$``LBTESTCD``, ``lb_map``$``LBSTRESU``, sep ``=`` ``"|"``)``,`\
`    `[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``lb_map``)``)`\
`  ``)``,`\
`  target_type ``=`` ``"integer"``,`\
`  name ``=`` ``"lb_row_inv"`\
`)`\
`#> KS Invalue: lb_row_inv`\
`#> Target Type: integer `\
`#> Mappings:`\
`#>   BLOOD CHEMISTRY|ALB|g/L => 1`\
`#>   COAGULOGRAM|FIBRINO|g/L => 2`\
`#>   COAGULATION PANEL|INR|NA => 3`\
`#>   COAGULOGRAM|INR|NA => 4`\
\
`# Reconstruct lb_map row indices — works even when LBSTRESU is NA`\
[`finputk`](https://crow16384.github.io/ksformat/reference/finputk.md)`(``lb_map``$``LBCAT``, ``lb_map``$``LBTESTCD``, ``lb_map``$``LBSTRESU``,`\
`        invalue_name ``=`` ``"lb_row_inv"``, na_as_string ``=`` ``TRUE``)`\
`#> [1] 1 2 3 4`\
\
[`fclear`](https://crow16384.github.io/ksformat/reference/fclear.md)`(``)`\
`#> All formats cleared from library.`

The output type is always determined by the stored invalue’s
`target_type` (here `integer`). For character output create the invalue
with `target_type = "character"` and
[`finputk()`](https://crow16384.github.io/ksformat/reference/finputk.md)
returns a character vector.

## Example 30: Extract Discrete Levels with `flevels()`

[`flevels()`](https://crow16384.github.io/ksformat/reference/flevels.md)
returns a tidy two-column lookup (`value`, `label`) from a `ks_format`
object (or a registered format name). This is handy when you want to
inspect, join, or export the exact mapping table used by a format.

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"PBO"``  ``=`` ``"Placebo"``,`\
`  ``"D50"``  ``=`` ``"Drug A 50 mg"``,`\
`  ``"D100"`` ``=`` ``"Drug A 100 mg"``,`\
`  name ``=`` ``"trt"`\
`)`\
\
[`flevels`](https://crow16384.github.io/ksformat/reference/flevels.md)`(``"trt"``)`\
`#>   value         label`\
`#> 1   PBO       Placebo`\
`#> 2   D50  Drug A 50 mg`\
`#> 3  D100 Drug A 100 mg`

Because the result is a regular `data.frame`, you can filter/select with
your preferred tools:

\
`lvls`` ``<-`` `[`flevels`](https://crow16384.github.io/ksformat/reference/flevels.md)`(``"trt"``)`\
`lvls``[``lvls``$``value`` ``!=`` ``"PBO"``, , drop ``=`` ``FALSE``]`\
`#>   value         label`\
`#> 2   D50  Drug A 50 mg`\
`#> 3  D100 Drug A 100 mg`

## Example 31: Numeric Pattern Formatting

Numeric pattern formats are useful for continuous values such as
currency, percentages, and fixed-precision displays.

Define a currency format with grouping and two decimals:

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(`\
`  ``"$%,.2f"``,`\
`  .missing ``=`` ``"NO DATA"``,`\
`  .other ``=`` ``"INVALID"``,`\
`  type ``=`` ``"numeric"``,`\
`  name ``=`` ``"currency"`\
`)`\
\
`amounts`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``1234.56``, ``-``7890.12``, ``0``, ``NA``, ``"oops"``)`\
\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  raw ``=`` ``amounts``,`\
`  formatted ``=`` `[`fput`](https://crow16384.github.io/ksformat/reference/fput.md)`(``amounts``, ``"currency"``)`\
`)`\
`#>        raw  formatted`\
`#> 1  1234.56  $1,234.56`\
`#> 2 -7890.12 -$7,890.12`\
`#> 3        0      $0.00`\
`#> 4     <NA>    NO DATA`\
`#> 5     oops    INVALID`

Percent-like suffixes are supported via literal text around the numeric
token:

\
[`fnew`](https://crow16384.github.io/ksformat/reference/fnew.md)`(``"%.1f%%"``, type ``=`` ``"numeric"``, name ``=`` ``"pct"``)`\
\
`values`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``12.345``, ``-``7.8``)`\
[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`  raw ``=`` ``values``,`\
`  pct ``=`` `[`fputn`](https://crow16384.github.io/ksformat/reference/fputn.md)`(``values``, ``"pct"``)`\
`)`\
`#>      raw   pct`\
`#> 1  0.000  0.0%`\
`#> 2 12.345 12.3%`\
`#> 3 -7.800 -7.8%`

## Example 32: Bidirectional `nocase` for Character Types

[`fnew_bid()`](https://crow16384.github.io/ksformat/reference/fnew_bid.md)
can now set `ignore_case = TRUE` so that both directions are
case-insensitive:

- forward lookup: code -\> label (`fput*`)
- reverse lookup: label -\> code (`finput*`)

\
[`fnew_bid`](https://crow16384.github.io/ksformat/reference/fnew_bid.md)`(`\
`  ``"Y"`` ``=`` ``"Yes"``,`\
`  ``"N"`` ``=`` ``"No"``,`\
`  name ``=`` ``"yesno_nc"``,`\
`  ignore_case ``=`` ``TRUE`\
`)`\
`#> $format`\
`#> KS Format:yesno_nc (nocase)`\
`#> Type: character `\
`#> Mappings:`\
`#>   Y => Yes`\
`#>   N => No`\
`#> `\
`#> $invalue`\
`#> KS Invalue: yesno_nc_inv (nocase)`\
`#> Target Type: character `\
`#> Mappings:`\
`#>   Yes => Y`\
`#>   No => N`\
\
`# Forward (mixed-case codes)`\
[`fputc`](https://crow16384.github.io/ksformat/reference/fputc.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"y"``, ``"N"``, ``"YES"``)``, ``"yesno_nc"``)`\
`#> [1] "Yes" "No"  "YES"`\
\
`# Reverse (mixed-case labels)`\
[`finputc`](https://crow16384.github.io/ksformat/reference/finputc.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"yes"``, ``"NO"``, ``"Yes"``)``, ``"yesno_nc_inv"``)`\
`#> [1] "Y" "N" "Y"`
