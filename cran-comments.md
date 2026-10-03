# CRAN submission comments

## Package: ksformat 0.8.5 (from 0.8.4)

### Test environments
- Local: R 4.5.3 on macOS (arm64)
- GitHub Actions: R 4.1, R 4.2, R 4.3, R 4.4, R 4.5

### R CMD check results
- `R CMD check --as-cran ksformat_0.8.5.tar.gz` passes with no errors, warnings, or notes.

### Summary of changes from 0.8.4 to 0.8.5

**Documentation only — no code changes:**
- Added an animated hero logo (GIF) shown at the top of the README and of each vignette; the GIF generator is included under `scripts/` (site/tooling only).
- Vignettes declare `resource_files` so the logo asset ships with the installed package.
- Pkgdown site configuration reorganized under `pkgdown/` (navbar article groups, landing-page hero, theme CSS, bundled cheatsheet link); these files are excluded from the source tarball.

### Summary of changes from 0.8.2 to 0.8.4

#### Version 0.8.4 — New features and documentation

**New functions:**
- `flevels()`: Extracts discrete value-label mappings from a `ks_format` object (or registered format name) as a tidy two-column `data.frame` with `value` and `label` columns. Simplifies introspection of format definitions.

**Enhanced functions:**
- `fnew()` now supports **numeric pattern mode** for `type = "numeric"`: users can pass a single unnamed `%f`-style format pattern (e.g., `"$%,.2f"` for currency or `"%.1f%%"` for percentages) to format continuous numeric values directly, complementing the existing discrete value-mapping modes.
- `fnew_bid()` gains an `ignore_case` argument. When `TRUE`, both the forward format and reverse invalue use case-insensitive matching. Default `FALSE` preserves backward compatibility.

**Documentation:**
- Added vignette **Example 31: Numeric Pattern Formatting** covering currency/grouping syntax, suffix text, and `.missing`/`.other` fallback handling with numeric patterns.
- Added runnable companion script: `examples/NumericPatterns.R`.

#### Version 0.8.3 / 0.8.2 maintenance
- Internal performance optimization and codebase refactoring (no breaking changes).
- All existing function behavior and API remain unchanged.

### Backward compatibility
- All changes are backward-compatible; no breaking changes to existing exported functions or arguments.
- Existing code using `fnew()`, `fput()`, `finput()`, etc. continues to work without modification.

### Downstream dependencies
- None identified (regular CRAN submission).
