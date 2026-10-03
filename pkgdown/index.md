<div class="kshero">

<img src="man/figures/ksformat-logo-hero.gif" alt="ksformat animated logo" class="kshero-logo" width="560" />

<p class="kshero-tag">SAS PROC FORMAT, brought to R —<br />codes in, labels out: one rule engine for values, ranges, windows and patterns.</p>

<div class="kshero-cta">
<a class="btn btn-ksformat" href="articles/usage_examples.html">Start here</a>
<a class="btn btn-ksformat-ghost" href="articles/nonstandard-applications.html">Clinical patterns</a>
</div>

</div>

## Why ksformat

**ksformat** re-implements the idea behind SAS `PROC FORMAT` for R: repeated conditional logic — code→label dictionaries, age and BMI buckets, protocol visit windows, date displays, report-ready numbers — lives in **named, registered formats** instead of scattered `ifelse()`/`case_when()` blocks. You define a format once near your analysis spec and apply it by name in every script of a study, so the mapping a reviewer approved is exactly the mapping the data were labelled with.

The package is a pure data layer: it produces plain character, factor and Date columns that any downstream tool can render — into a `ksTFL` table, a `ggplot2` scale, or a SHINY display. No plotting, no document formatting, no lock-in: one small deterministic engine with `Imports: cli` and nothing else.

### Key design principles

- **Rule engine, not a dictionary** — discrete values, numeric ranges, dates, composites and patterns are one concept: a format with a name
- **Both directions** — value→label (`fput`) and label→value reverse lookups (`finput`) for QC, with `fnew_bid()` creating the pair
- **Missing values are first-class** — `.missing` and `.other` rules beat silent `NA` fall-through
- **Text-diffable definitions** — `fexport()`/`fparse()` round-trip formats as reviewable text that belongs in Git
- **SAS compatibility** — import CNTLOUT catalogues, apply built-in `DATE9.`-style formats, and reuse `w.d` display patterns
- **Expression labels** — dynamic labels (`.x1`, `.x2`, …) evaluated at apply time for n(%) and p-value display strings

## Quick start

One call registers a named format; another applies it — including the missing
and unmatched branches that quietly corrupt most production pipelines:

```r
library(ksformat)

fnew("M" = "Male", "F" = "Female",
     .missing = "Unknown", .other = "Other",
     name = "sex")

fput(c("M", "F", NA, "U"), "sex")
#> [1] "Male"    "Female"  "Unknown" "Other"
```

Numeric ranges work the same way — and the same rules can be written as reviewable
text for your spec files:

```r
fparse(text = '
VALUE agegr (numeric)
  [0, 18)    = "Child"
  [18, 65)   = "Adult"
  [65, HIGH] = "Senior"
  .missing   = "Unknown"
;')

fputn(c(9, 17, 44, 81, NA), "agegr")
#> [1] "Child"   "Child"   "Adult"   "Senior"  "Unknown"
```

Report-ready numbers are formats too:

```r
fnew("$%,.2f", .missing = "-", type = "numeric", name = "cash")
fputn(c(1234.5, 9.875, NA), "cash")
#> [1] "$1,234.50" "$9.88"     "-"
```

That is the whole model: `fnew()`/`fparse()` **define** rules, `fput*()` **applies**
them, `finput*()` **reverses** them, `fexport()` **versions** them. Every study
table then shows the same label for the same code — because there is only one
definition of it.

## What else it can do

- **Protocol visit windows.** `stratified_range` formats map (arm, study day) to window labels — the derivation that usually hides in nested `ifelse()`, named and testable instead.
- **Composite keys, ADaM-style.** `fputk()` looks up a label from several columns at once (LBCAT | LBSPEC | LBTESTCD | LBSTRESU → PARAMCD), with the `na_as_string` discipline spelled out in the vignettes.
- **Dynamic labels.** A label containing `.x1` is evaluated at apply time: one format emits `n (%)`, p-value censoring and unit-suffixed strings straight from the statistics frame.
- **SAS date formats out of the box.** `fputn(x, "DATE9.")` gives `27SEP2026` for Date, POSIXct and epoch numerics — with a documented locale protocol so month abbreviations never bake in Cyrillic.
- **Reverse QC.** `fnew_bid()` registers the invalue next to the format (`name_inv`), so a mapping can be checked by converting the report labels back to codes.
- **Multilabel.** One value can match several rules (`fput_all`) — supertype/subtype groupings without duplicating data.
- **Auditable.** `flevels()`, `franges()`, `fprint()` dump any format's definition — the audit trail of a mapping is a function call.

## Installation

ksformat is on **CRAN**:

```r
install.packages("ksformat")
```

The development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("crow16384/ksformat")
```

## Documentation & resources

| I want to… | Go to |
|------------|-------|
| Walk through the most common uses | [Usage Examples](articles/usage_examples.html) |
| Solve clinical-trial problems: windows, PARAMCD, QC | [Non-standard Applications](articles/nonstandard-applications.html) |
| Look up any function | [Reference](reference/index.html) |
| Print a cheat sheet | [Cheatsheet (PDF)](ksformat_cheatsheet.pdf) |
| See what changed | [Changelog](news/index.html) |

--------------------------------------------------------------------------------

**License.** GPL-3. **Authors.** Vladimir Larchenko, Igor Aleschenkov — KeyStat
