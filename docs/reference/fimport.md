# Import Formats from 'SAS' PROC FORMAT CNTLOUT CSV

Reads a CSV file produced by 'SAS' `PROC FORMAT` with `CNTLOUT=` option
(typically exported via `PROC EXPORT`) and converts compatible format
definitions into `ks_format` and `ks_invalue` objects.

## Usage

``` r
fimport(file, register = TRUE, overwrite = TRUE)
```

## Arguments

- file:

  Path to the CSV file exported from a SAS format catalogue.

- register:

  Logical; if `TRUE` (default), each imported format is registered in
  the global format library.

- overwrite:

  Logical; if `TRUE` (default), existing library entries with the same
  name are overwritten.

## Value

A named list of `ks_format` and `ks_invalue` objects that were
successfully imported. Returned invisibly.

## Details

The 'SAS' format catalogue CSV is expected to contain the standard
CNTLOUT columns: `FMTNAME`, `START`, `END`, `LABEL`, `TYPE`, `HLO`,
`SEXCL`, `EEXCL`.

**Supported SAS format types:**

- `N`:

  Numeric VALUE format \\\to\\ `ks_format` with `type = "numeric"`

- `C`:

  Character VALUE format \\\to\\ `ks_format` with `type = "character"`

- `I`:

  Numeric INVALUE (informat) \\\to\\ `ks_invalue` with
  `target_type = "numeric"`

- `J`:

  Character INVALUE (informat) \\\to\\ `ks_invalue` with
  `target_type = "character"`

**Incompatible types (logged with a warning):**

- `P`:

  PICTURE formats \\-\\ no equivalent in ksformat

Rows with SAS special missing values (`.A`\\-\\`.Z`, `._`) in the HLO
field are logged as incompatible entries and skipped because R has no
equivalent concept.

## Examples

``` r
# In SAS:
# proc format library=work cntlout=fmts; run;
# proc export data=fmts outfile="formats.csv" dbms=csv replace; run;

csv_file <- system.file("extdata", "test_cntlout.csv", package = "ksformat")
imported <- fimport(csv_file)
#> Warning: Skipping PICTURE format: "PICFMT"
#> ℹ TYPE="P" is not supported by ksformat.
#> Warning: Skipped incompatible entry in format "SMISSING":
#> ✖ SAS special missing value '.A' (HLO='S') has no R equivalent.
#> Warning: Skipped incompatible entry in format "SMISSING":
#> ✖ SAS special missing value '.B' (HLO='S') has no R equivalent.
#> ✔ Imported 4 formats and 1 invalue from
#>   /private/var/folders/rn/3s0h46m118j426j_fmjr1z8m0000gn/T/RtmppXdY8X/temp_libpathe375453e87ab/ksformat/extdata/test_cntlout.csv.
flist()
#> [1] "AGEGRP"   "BMICAT"   "GENDER"   "RACEIN"   "SMISSING"
fprint()
#> Registered formats:
#>   AGEGRP - VALUE (numeric), 3 mapping(s)
#>   BMICAT - VALUE (numeric), 4 mapping(s)
#>   GENDER - VALUE (character), 2 mapping(s)
#>   RACEIN - INVALUE (numeric), 3 mapping(s)
#>   SMISSING - VALUE (numeric), 1 mapping(s)
fclear()
#> All formats cleared from library.
```
