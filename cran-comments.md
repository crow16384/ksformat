# CRAN submission comments

## Package: ksformat 0.4.6

### Test environments
- Local: R 4.5.3 on Debian GNU/Linux 13 (x86_64)

### Notes
- `R CMD check --as-cran` was run on the built tarball (`R CMD build` then `R CMD check ksformat_0.4.6.tar.gz --as-cran`).
- Warnings about missing `qpdf` or `tidy` are due to the local environment; CRAN check servers have these tools.

### Changes in 0.4.2 (CRAN pretest feedback)
- Excluded top-level `pkgdown/` from the source tarball via `.Rbuildignore` (favicon build output; not part of the package).
- Fixed `ksformat_cheatsheet` documentation: roxygen block is attached to the exported function; added `man/ksformat_cheatsheet.Rd` and `\link[ksformat]{ksformat_cheatsheet}` in `ksformat-package.Rd` so Rd cross-references pass `R CMD check`.

### Downstream dependencies
- None (first CRAN release / no reverse dependencies).
