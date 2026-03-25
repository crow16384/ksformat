# Docs and Site Workflow (2026-03-26)

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
