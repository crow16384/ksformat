# Docs and Site Workflow (2026-05-15)

- Canonical vignette source: `vignettes/usage_examples.Rmd`.
- Current vignette icon tag uses:
  - `<img src="../man/figures/logo.svg" ... />`
- Rebuild sequence used:
  - `Rscript -e "devtools::document()"`
  - `Rscript -e "devtools::build_vignettes()"`
  - `Rscript -e "pkgdown::build_site(preview = FALSE)"` (if pkgdown installed)
- `devtools::build_vignettes()` writes to `doc/`; copy outputs to `inst/doc/` when installed docs must be refreshed.
- Cleanup after rebuild:
  - remove `doc/`, `Meta/`, and generated `vignettes/usage_examples.{R,html}`
- Expected pkgdown sitrep warnings currently present:
  - Bootstrap 3 deprecated
  - No `_pkgdown.yml` found
- These warnings are non-blocking; site still builds to `docs/`.

## Vignette examples added so far\n- Examples 1-27: existing (through stratified ranges)\n- Example 28: `fputk(na_as_string = TRUE)` — composite key with NA components\n- Example 29: `finputk()` — composite label invalue lookup\n- Example 30: `flevels()` — extract discrete levels\n- Example 31: numeric `%f` pattern formatting\n- Example 32 (0.8.4): bidirectional character lookup with `fnew_bid(..., ignore_case = TRUE)`\n- Companion scripts in `examples/`: `DateLookup.R`, `DateRanges.R`, `StratifiedRanges.R`, `CompositeKeyNA.R`\n\n## Practical fallback\n- If `devtools` / `roxygen2` / `pkgdown` are unavailable in the environment, update `man/`, `doc/`, and `docs/` artifacts manually to keep site/docs in sync with source changes.
