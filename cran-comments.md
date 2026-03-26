# CRAN submission comments

## Package: ksformat 0.4.1

### Test environments
- Local: R 4.5.3 on Debian GNU/Linux 13 (x86_64)

### Notes
- `R CMD check --as-cran` was run on the built tarball (`R CMD build` then `R CMD check ksformat_0.4.1.tar.gz --as-cran`).
- Warnings about missing `qpdf` or `tidy` are due to the local environment; CRAN check servers have these tools.

### Downstream dependencies
- None (first CRAN release / no reverse dependencies).
