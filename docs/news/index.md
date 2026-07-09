# Changelog

## ksformat 0.8.3

### New features

- Added [`flevels()`](../reference/flevels.md): extracts discrete
  value-label mappings from a `ks_format` object (or registered format
  name) as a tidy two-column `data.frame` (`value`, `label`).
- [`fnew()`](../reference/fnew.md) now supports numeric pattern mode for
  `type = "numeric"`: pass one unnamed `%f`-style pattern (for example
  `"$%,.2f"` or `"%.1f%%"`) to format continuous numeric values
  directly.

### Documentation

- Added vignette **Example 31: Numeric Pattern Formatting** covering
  currency/grouping, suffix text, and `.missing`/`.other` handling.
- Added runnable companion script: `examples/NumericPatterns.R`.

## ksformat 0.8.1

### Documentation refresh

- Added a vignette example showing how to use
  [`franges()`](../reference/franges.md) labels directly with
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  and how to extract unique labels from the `aesev` format mappings
  without converting to
  [`factor()`](https://rdrr.io/r/base/factor.html).

### Internal refactoring (no breaking changes)

- `R/format_parse.R` has been split in two: text-to-object parsing stays
  in `format_parse.R` (874 lines); object-to-text rendering and CSV
  CNTLOUT import/export ([`fexport()`](../reference/fexport.md),
  [`fimport()`](../reference/fimport.md),
  [`.format_to_text()`](../reference/dot-format_to_text.md), etc.) moves
  to the new `R/format_serialize.R` (755 lines).
- Three DRY internal helpers added to `utilities.R`: `.is_eval_label()`,
  `.parse_range_key_by_type()`, `.format_range_interval()`. Duplicate
  switch/if-chains across `format_apply.R`, `format_create.R`, and
  `format_library_app.R` now delegate to these helpers.
- Range-table now carries a `discrete_numeric_possible` flag computed at
  format creation time. [`fput()`](../reference/fput.md) /
  [`fput_all()`](../reference/fput_all.md) use it to skip the
  [`as.character()`](https://rdrr.io/r/base/character.html) +
  [`match()`](https://rdrr.io/r/base/match.html) discrete-key pass for
  numeric / Date / POSIXt inputs against formats whose discrete keys are
  all non-numeric strings, broadening the existing fast path.
- Friendly-interval regex in `.block_to_stratified_range_format()` is
  now built once per format parse instead of once per mapping entry.

## ksformat 0.7.2

### New features

- [`finput()`](../reference/finput.md) gains an `ignore_case` argument.
  When `TRUE`, label lookup is case-insensitive (equivalent to SAS
  `INVALUE name (nocase)`). Default `FALSE` preserves existing
  behaviour.
- [`fparse()`](../reference/fparse.md) now correctly propagates the
  `(nocase)` option on `INVALUE` blocks to the resulting `ks_invalue`
  object. Previously the flag was parsed but silently discarded.
- [`fexport()`](../reference/fexport.md) /
  [`fparse()`](../reference/fparse.md) round-trips now preserve
  `ignore_case` for invalue objects: exporting a `nocase` invalue emits
  `INVALUE name (nocase)` which re-imports with `ignore_case = TRUE`.
- [`print.ks_invalue()`](../reference/print.ks_invalue.md) now displays
  a `(nocase)` flag in the header when `ignore_case = TRUE`.

## ksformat 0.7.1

CRAN release: 2026-05-21

### New features

- [`fputk()`](../reference/fputk.md) gains a `na_as_string` argument.
  When `TRUE`, an `NA` in any key component is preserved as the literal
  string `"NA"` produced by
  [`paste()`](https://rdrr.io/r/base/paste.html) instead of being
  restored to `NA_character_`. This enables round-trip lookups against
  formats built via `fmap(paste(..., sep = "|"), values)`, where the
  stored keys themselves contain `"NA"`. Default `FALSE` preserves
  existing behavior (NA → `.missing` / `keep_na`).
- New [`finputk()`](../reference/finputk.md) — composite-label wrapper
  around INVALUE lookup, mirroring [`fputk()`](../reference/fputk.md) on
  the reverse direction. Supports the same `sep` and `na_as_string`
  arguments and dispatches on the stored invalue’s `target_type`.

### Documentation

- Vignette **Example 28: Composite Key Lookup with NA Components** —
  explains why [`paste()`](https://rdrr.io/r/base/paste.html)-built keys
  contain the literal string `"NA"` for `NA` inputs, shows the incorrect
  result with the default `na_as_string = FALSE`, and demonstrates the
  correct round-trip with `na_as_string = TRUE` using a clinical LB
  `PARAMCD` derivation scenario.
- Vignette **Example 29: Composite Label Invalue Lookup with
  [`finputk()`](../reference/finputk.md)** — basic two-column reverse
  lookup and `na_as_string = TRUE` usage with `NA` key components.
- `examples/CompositeKeyNA.R` — runnable companion script for both
  examples.

## ksformat 0.7.0

### New features

- New format type **`stratified_range`** — combines a discrete stratum
  (study arm, subject id, composite key, …) with per-stratum numeric /
  Date / POSIXct range buckets. Apply with
  [`fputk()`](../reference/fputk.md), passing the stratum column(s)
  first and the value column last.
- New builder
  `fmap_strata(stratum, low, high, label, inc_low, inc_high, sep)` —
  produces a `ks_fmap` ready for `fnew(..., type = "stratified_range")`.
  The chosen separator is carried as an attribute and picked up
  automatically by [`fnew()`](../reference/fnew.md).
- New builder `fmap_ranges(low, high, label, inc_low, inc_high)` —
  convenience helper that turns parallel vectors of numeric / Date /
  POSIXct bounds and labels into canonical range keys, removing the need
  to hand-craft `"low,high,inc_low,inc_high"` strings.
- [`fparse()`](../reference/fparse.md) /
  [`fexport()`](../reference/fexport.md) support the new type via the
  `VALUE name (stratified_range, range_subtype: numeric|date|datetime, strata_sep: |)`
  block syntax. Both canonical `"STRATUM|low,high,inc_low,inc_high"`
  keys and the friendly `"STRATUM"|[low, high)` interval form are
  accepted, including per-stratum `.missing|STRATUM` / `.other|STRATUM`
  directives.
- [`print.ks_format()`](../reference/print.ks_format.md) renders
  stratified formats grouped under `Stratum "X":` headers.

### Documentation

- Vignette **Example 26: Stratified Range Lookup with
  [`fputk()`](../reference/fputk.md)** — programmatic and text-based
  construction, date subtype with per-subject windows, per-stratum and
  global `.other` fallbacks, and
  [`fexport()`](../reference/fexport.md)/[`fparse()`](../reference/fparse.md)
  roundtrip.
- Vignette **Example 27: Plain Range Lookup with
  [`fmap_ranges()`](../reference/fmap_ranges.md)** — builds a numeric
  age-band format without hand-crafted canonical keys.
- `examples/StratifiedRanges.R` — runnable companion script.

## ksformat 0.6.7

### Documentation

- Added vignette **Example 25: Date Range Bucketing** covering
  `date_range` and `datetime_range` types: fiscal-year bucketing,
  [`fparse()`](../reference/fparse.md) with ISO bounds, `LOW`/`HIGH`
  open-ended arms, [`fput_all()`](../reference/fput_all.md) multilabel
  overlapping windows, auto-detection,
  [`fexport()`](../reference/fexport.md)/[`fparse()`](../reference/fparse.md)
  roundtrip, and `datetime_range` shift bucketing.
- Added `examples/DateRanges.R` — a self-contained runnable script with
  the same scenarios for quick interactive exploration.

## ksformat 0.6.6

### New features

- `date_range` and `datetime_range` format types: bucket Date or POSIXct
  values into character labels using interval bounds written as ISO
  date/datetime strings. Both types reuse the numeric range-table fast
  path (sorted-disjoint ranges hit
  [`findInterval()`](https://rdrr.io/r/base/findInterval.html) in C).
- [`fnew()`](../reference/fnew.md) accepts `type = "date_range"` /
  `"datetime_range"` and a new optional `date_format` argument for
  parsing custom bound strings.
- [`fparse()`](../reference/fparse.md) accepts interval notation with
  date/datetime bounds, e.g. `[2024-01-01, 2025-01-01) = "FY24"` and
  `[2024-01-01 08:00, 2024-01-01 16:00) = "Day"`. The type is
  auto-detected when no explicit subtype is given.
- [`fexport()`](../reference/fexport.md) renders date/datetime range
  bounds as ISO strings, enabling full roundtrip through
  [`fparse()`](../reference/fparse.md).
- Bounds support `LOW` / `HIGH` keywords (rendered as `-Inf` / `+Inf`
  and emitted back as `LOW` / `HIGH` on export) and exclusive `(` / `)`
  brackets.

## ksformat 0.6.5

### Performance

- Precomputed range table: `ks_format` objects now carry a pre-built
  `range_table` field. Range keys are parsed once at format-creation
  time (in [`fnew()`](../reference/fnew.md),
  [`fparse()`](../reference/fparse.md),
  [`fimport()`](../reference/fimport.md)) rather than on every
  [`fput()`](../reference/fput.md) call.
- [`findInterval()`](https://rdrr.io/r/base/findInterval.html) fast path
  in [`fput()`](../reference/fput.md): sorted, non-overlapping numeric
  ranges with standard `[low, high)` semantics now use R’s built-in
  [`findInterval()`](https://rdrr.io/r/base/findInterval.html) (O(n
  log k) in C), giving a ~10–14× speedup over the previous per-range R
  loop on large inputs (benchmarked at 1M rows).
- `skip_discrete` optimisation in [`fput()`](../reference/fput.md): pure
  numeric-range formats (no discrete keys) with numeric input now skip
  the [`as.character()`](https://rdrr.io/r/base/character.html) +
  [`match()`](https://rdrr.io/r/base/match.html) step entirely.
- [`is_missing()`](../reference/is_missing.md): removed a redundant
  [`is.nan()`](https://rdrr.io/r/base/is.finite.html) pass on numeric
  vectors — [`is.na()`](https://rdrr.io/r/base/NA.html) already returns
  `TRUE` for `NaN`.

### Bug Fixes

- `.build_range_table()`: ranges defined out-of-order are now sorted by
  `(low, high)` before storage, so the `findInterval` fast path triggers
  regardless of definition order.
- `.build_range_table()`: removed a dead `attr<-` loop that silently had
  no effect (subsetting a character vector creates a copy, stripping any
  attribute set on the subset).

## ksformat 0.6.4

### New Features

- Added [`franges()`](../reference/franges.md): extracts all range
  entries from a `ks_format` object as a tidy `data.frame` with columns
  `low`, `high`, `inc_low`, `inc_high`, and `label`. Accepts either a
  `ks_format` object or a registered format name.
- Added [`fmap_to_ranges()`](../reference/fmap_to_ranges.md):
  reverse-looks up range bounds by label — given a vector of coded
  values that match range labels in a format, returns the corresponding
  `low` / `high` bounds (and inclusivity flags) per element.

## ksformat 0.6.3

### Bug Fixes

- Fixed [`fput()`](../reference/fput.md) (and
  [`fputk()`](../reference/fputk.md)) losing the `tzone` attribute when
  returning `POSIXct` values from a value-type format. The result vector
  was seeded from a tzone-less NA, so values were silently displayed in
  the local timezone instead of the source timezone (e.g. UTC), causing
  an apparent time-shift. The `tzone` attribute of the mapped values is
  now propagated to the result.

## ksformat 0.6.2

### New Features

- Added [`format_library_app()`](../reference/format_library_app.md)
  Shiny browser for the global format library, including filters by
  name/object type, per-object details, and mapping table rendering for
  both `ks_format` and `ks_invalue` entries.
- Added an RStudio addin entry (**Format Library Browser**) to launch
  [`format_library_app()`](../reference/format_library_app.md) from the
  Addins menu.

### Enhancements

- Replaced plain-text print preview in the app with a formatted mapping
  table that shows mappings, range rules, and special directives
  (`.missing`, `.other`) in structured form.
- Added app management UX improvements: explicit quit action with
  confirmation modal, plus confirmations for destructive clear/remove
  operations.

### Documentation

- Added [`format_library_app()`](../reference/format_library_app.md)
  docs and package index references.
- Updated README with interactive browser usage and behavior summary.

## ksformat 0.6.1

### Documentation

- Cheat sheet updated: added [`fputk()`](../reference/fputk.md) examples
  (basic composite key, [`fmap()`](../reference/fmap.md) data-driven
  Date lookup, [`fparse()`](../reference/fparse.md) text-defined Date
  lookup), added [`fmap()`](../reference/fmap.md) to the Function
  Reference table, and rearranged layout to fit landscape page.
- Vignette Example 22 added: “Date Lookup via
  [`fparse()`](../reference/fparse.md) and
  [`fputk()`](../reference/fputk.md)” — character lookup, native Date
  lookup with [`fmap()`](../reference/fmap.md), and round-trip via
  [`fexport()`](../reference/fexport.md)/[`fparse()`](../reference/fparse.md).

## ksformat 0.6.0

### Breaking Changes

- [`fnew()`](../reference/fnew.md) no longer accepts the `reverse`
  parameter. Use `fmap(keys, values)` instead of
  `setNames(values, keys)` with `reverse = FALSE` to create data-driven
  formats consistently for all types.

### New Features

- New `fmap(keys, values)` helper creates a key-value mapping that tells
  [`fnew()`](../reference/fnew.md) to use the natural direction (keys →
  values) without reversal. Works identically for character, numeric,
  Date, POSIXct, and logical formats.

### Documentation

- Vignette Example 21 rewritten: “Consistent Data-Driven Formats with
  [`fmap()`](../reference/fmap.md)” — demonstrates how
  `fmap(keys, values)` replaces the old
  [`setNames()`](https://rdrr.io/r/stats/setNames.html) +
  `reverse = FALSE` pattern.
- Updated Example 20 (Composite Key Lookup) to use
  [`fmap()`](../reference/fmap.md).

## ksformat 0.5.1

### Bug Fixes

- Fixed Example 20 (Composite Key Lookup) in vignette: added missing
  `reverse = FALSE` to the character format created with
  [`setNames()`](https://rdrr.io/r/stats/setNames.html), which caused
  all lookups to return “NOT FOUND”.

## ksformat 0.5.0

### New Features

- [`fnew()`](../reference/fnew.md) gains a `reverse` parameter for
  explicit control over named-vector direction. Set `reverse = FALSE` to
  use the natural `setNames(values, keys)` convention consistently for
  all format types — character, numeric, Date, POSIXct, and logical. The
  default (`NULL`) preserves backward-compatible auto-detection:
  reversal for character/numeric, no reversal for value types.

### Documentation

- New vignette Example 21: “Consistent Data-Driven Formats with
  `reverse`” — demonstrates how to build lookup formats from data frames
  using the same [`setNames()`](https://rdrr.io/r/stats/setNames.html)
  pattern for all output types.
- Updated [`fnew()`](../reference/fnew.md) documentation with detailed
  explanation of the named-vector reversal convention and the new
  `reverse` parameter.
