# Create a Format Definition (like 'SAS' PROC FORMAT)

Creates a format object that maps values to labels, similar to 'SAS'
PROC FORMAT. Supports discrete value mapping, ranges, and special
handling of missing values. The format is automatically stored in the
global format library if `name` is provided.

## Usage

``` r
fnew(
  ...,
  name = NULL,
  type = "auto",
  default = NULL,
  multilabel = FALSE,
  ignore_case = FALSE,
  date_format = NULL,
  range_subtype = c("numeric", "date", "datetime"),
  strata_sep = "|",
  verbose = FALSE
)
```

## Arguments

- ...:

  Named arguments defining value-label mappings, or one or more named
  vectors/lists using the R convention `c(Label = "Code")`. Can be:

  - Discrete values: `"M" = "Male", "F" = "Female"`

  - Named vector: `c(Male = "M", Female = "F")`

  - Named list: `list(Male = "M", Female = "F")`

  - Numeric pattern string: one unnamed `%f`-style pattern such as
    `"%,.2f"`, `"$%,.2f"`, or `"%.1f%%"` (for `type = "numeric"`)

  - [`fmap`](fmap.md) vector: `fmap(keys, values)` for data-driven
    formats (no reversal)

  - Special values: `.missing = "Missing"`, `.other = "Other"`

  Named vectors use the R idiom where names are labels and values are
  codes, which is the reverse of the `...` argument convention.

  **Named-vector reversal:** For character and numeric formats, named
  vectors are automatically reversed so that `c(Male = "M")` becomes the
  mapping `"M" -> "Male"` (following the
  [`factor()`](https://rdrr.io/r/base/factor.html) convention). For
  value types (`"Date"`, `"POSIXct"`, `"logical"`), no reversal occurs —
  the named vector is used as-is with names as input keys and values as
  output objects, because non-character objects cannot serve as vector
  names.

  **Data-driven formats:** For formats built programmatically from data,
  wrap your data in [`fmap`](fmap.md)`(keys, values)` to suppress
  automatic reversal for all types. See
  [`vignette("usage_examples")`](../articles/usage_examples.md) Example
  21 for a detailed walkthrough.

- name:

  Character. Optional name for the format. If provided, the format is
  automatically registered in the global format library.

- type:

  Character. Type of format: `"character"`, `"numeric"`, `"Date"`,
  `"POSIXct"`, `"logical"`, `"date_range"`, `"datetime_range"`, or
  `"auto"` (default) for auto-detection. Value types (`"Date"`,
  `"POSIXct"`, `"logical"`) store native R objects instead of character
  labels. For value types, `.missing` and `.other` are always typed NA.
  Range-bucketing types (`"date_range"`, `"datetime_range"`) bucket
  `Date`/`POSIXct` input into character labels using ISO date/datetime
  strings as range bounds. For `type = "numeric"`, a single unnamed
  `%f`-style pattern in `...` creates a numeric pattern format (no
  explicit value-label mappings required).

- default:

  Character. Default label for unmatched values (overrides .other)

- multilabel:

  Logical. If `TRUE`, the format supports overlapping ranges where a
  single value can match multiple labels. Used with
  [`fput_all`](fput_all.md) to retrieve all matching labels. Default
  `FALSE`.

- ignore_case:

  Logical. If `TRUE`, key matching for character formats is
  case-insensitive. Default `FALSE`.

- date_format:

  Character. Optional strptime-style format string used when parsing
  date/datetime range keys (e.g. `"%d/%m/%Y"`). When `NULL`, ISO 8601
  parsing is used. Applies to `"date_range"`, `"datetime_range"`, and
  the value types `"Date"` / `"POSIXct"` when keys are ranges.

- range_subtype:

  Character. For `type = "stratified_range"` only. One of `"numeric"`
  (default), `"date"`, or `"datetime"`. Determines how the range part of
  each stratified key is parsed and compared.

- strata_sep:

  Character. For `type = "stratified_range"` only. Single-character (or
  multi-character) separator between the stratum identifier and the
  range key in each mapping key. Default `"|"`.

- verbose:

  Logical. If `TRUE`, returns the format object visibly; otherwise
  returns it invisibly. Default `FALSE`.

## Value

An object of class `"ks_format"` containing the format definition. The
object is also stored in the format library if `name` is given.

## Details

Special directives:

- `.missing`: Label for NA, NULL, NaN values

- `.other`: Label for values not matching any rule

**Named-vector direction (reverse convention):**

When a named vector or list is passed as an unnamed argument (e.g.,
`fnew(c(Male = "M"))`), the direction of the name-to-value mapping
depends on the output `type`:

- For **character / numeric** types, names are *labels* and values are
  *codes*. The pairs are reversed internally so that the format maps
  `code -> label`. This follows the standard R idiom used by
  [`factor()`](https://rdrr.io/r/base/factor.html), where
  `c(Label = "Code")`.

- For **value types** (`Date`, `POSIXct`, `logical`), names are *input
  keys* and values are the native R objects returned by the format. No
  reversal is applied, because non-character objects cannot be used as
  vector names.

This means the *same data* may need to be arranged differently depending
on the target type. To avoid this inconsistency for data-driven formats,
use [`fmap`](fmap.md)`(keys, values)` which works identically for all
types:

    fnew(fmap(ids, dates), type = "Date")
    fnew(fmap(ids, date_strings), type = "character")

When in doubt, use explicit `key = "label"` arguments — these are never
reversed regardless of type.

**Expression labels:** If a label contains `.x1`, `.x2`, etc., it is
treated as an R expression that is evaluated at apply-time. Extra
arguments are passed positionally via `...` in [`fput`](fput.md):

    stat_fmt <- fnew("n" = "sprintf('%s', .x1)",
                     "pct" = "sprintf('%.1f%%', .x1 * 100)")
    fput(c("n", "pct"), stat_fmt, c(42, 0.15))
    # Returns: "42" "15.0%"

**Numeric pattern mode:** For `type = "numeric"`, you may pass one
unnamed `%f`-style pattern string in `...` instead of mapping keys. This
creates a continuous numeric display format:

    fnew("$%,.2f", name = "currency", type = "numeric")
    fnew("%.1f%%", name = "pct", type = "numeric")

The pattern may include literal prefix/suffix text and exactly one
numeric conversion token (`%f` / `%F` with flags/width/precision).

## Examples

``` r
# Discrete value format (auto-stored as "sex")
fnew(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  .other = "Other Gender",
  name = "sex"
)

# Apply immediately
fput(c("M", "F", NA, "X"), "sex")
#> [1] "Male"         "Female"       "Unknown"      "Other Gender"
# [1] "Male" "Female" "Unknown" "Other Gender"
fclear()
#> All formats cleared from library.

# Multilabel format: a value can match multiple labels
fnew(
  "0,5,TRUE,TRUE"   = "Infant",
  "6,11,TRUE,TRUE"  = "Child",
  "12,17,TRUE,TRUE" = "Adolescent",
  "0,17,TRUE,TRUE"  = "Pediatric",
  "18,64,TRUE,TRUE" = "Adult",
  "65,Inf,TRUE,TRUE" = "Elderly",
  "18,Inf,TRUE,TRUE" = "Non-Pediatric",
  name = "age_categories",
  type = "numeric",
  multilabel = TRUE
)

# fput returns first match; fput_all returns all matches
fput(c(3, 14, 25, 70), "age_categories")
#> [1] "Infant"        "Pediatric"     "Adult"         "Non-Pediatric"
fput_all(c(3, 14, 25, 70), "age_categories")
#> [[1]]
#> [1] "Infant"    "Pediatric"
#> 
#> [[2]]
#> [1] "Pediatric"  "Adolescent"
#> 
#> [[3]]
#> [1] "Adult"         "Non-Pediatric"
#> 
#> [[4]]
#> [1] "Non-Pediatric" "Elderly"      
#> 
fclear()
#> All formats cleared from library.

# From a named vector (Label = Code convention)
sex_vec <- fnew(c(Male = "M", Female = "F"), .missing = "Unknown",
               name = "sex_vec")
fput(c("M", "F", NA), sex_vec)
#> [1] "Male"    "Female"  "Unknown"
# [1] "Male" "Female" "Unknown"
fclear()
#> All formats cleared from library.

# Numeric pattern format
fnew("$%,.2f", name = "currency", type = "numeric")
fputn(c(1234.56, -7890.12, 0), "currency")
#> [1] "$1,234.56"  "-$7,890.12" "$0.00"     
# [1] "$1,234.56" "-$7,890.12" "$0.00"
fclear()
#> All formats cleared from library.
```
