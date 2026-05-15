# ksformat 0.7.1

## New features

* `fputk()` gains a `na_as_string` argument. When `TRUE`, an `NA` in any
  key component is preserved as the literal string `"NA"` produced by
  `paste()` instead of being restored to `NA_character_`. This enables
  round-trip lookups against formats built via
  `fmap(paste(..., sep = "|"), values)`, where the stored keys
  themselves contain `"NA"`. Default `FALSE` preserves existing behavior
  (NA → `.missing` / `keep_na`).
* New `finputk()` — composite-label wrapper around INVALUE lookup,
  mirroring `fputk()` on the reverse direction. Supports the same
  `sep` and `na_as_string` arguments and dispatches on the stored
  invalue's `target_type`.

## Documentation

* Vignette **Example 28: Composite Key Lookup with NA Components** —
  explains why `paste()`-built keys contain the literal string `"NA"` for
  `NA` inputs, shows the incorrect result with the default
  `na_as_string = FALSE`, and demonstrates the correct round-trip with
  `na_as_string = TRUE` using a clinical LB `PARAMCD` derivation scenario.
* Vignette **Example 29: Composite Label Invalue Lookup with `finputk()`** —
  basic two-column reverse lookup and `na_as_string = TRUE` usage with
  `NA` key components.
* `examples/CompositeKeyNA.R` — runnable companion script for both examples.

# ksformat 0.7.0

## New features

* New format type **`stratified_range`** — combines a discrete stratum
  (study arm, subject id, composite key, …) with per-stratum numeric /
  Date / POSIXct range buckets. Apply with `fputk()`, passing the
  stratum column(s) first and the value column last.
* New builder `fmap_strata(stratum, low, high, label, inc_low, inc_high,
  sep)` — produces a `ks_fmap` ready for
  `fnew(..., type = "stratified_range")`. The chosen separator is
  carried as an attribute and picked up automatically by `fnew()`.
* New builder `fmap_ranges(low, high, label, inc_low, inc_high)` —
  convenience helper that turns parallel vectors of numeric / Date /
  POSIXct bounds and labels into canonical range keys, removing the
  need to hand-craft `"low,high,inc_low,inc_high"` strings.
* `fparse()` / `fexport()` support the new type via the
  `VALUE name (stratified_range, range_subtype: numeric|date|datetime,
  strata_sep: |)` block syntax. Both canonical
  `"STRATUM|low,high,inc_low,inc_high"` keys and the friendly
  `"STRATUM"|[low, high)` interval form are accepted, including
  per-stratum `.missing|STRATUM` / `.other|STRATUM` directives.
* `print.ks_format()` renders stratified formats grouped under
  `Stratum "X":` headers.

## Documentation

* Vignette **Example 26: Stratified Range Lookup with `fputk()`** —
  programmatic and text-based construction, date subtype with
  per-subject windows, per-stratum and global `.other` fallbacks, and
  `fexport()`/`fparse()` roundtrip.
* Vignette **Example 27: Plain Range Lookup with `fmap_ranges()`** —
  builds a numeric age-band format without hand-crafted canonical keys.
* `examples/StratifiedRanges.R` — runnable companion script.

# ksformat 0.6.7

## Documentation

* Added vignette **Example 25: Date Range Bucketing** covering `date_range` and
  `datetime_range` types: fiscal-year bucketing, `fparse()` with ISO bounds,
  `LOW`/`HIGH` open-ended arms, `fput_all()` multilabel overlapping windows,
  auto-detection, `fexport()`/`fparse()` roundtrip, and `datetime_range`
  shift bucketing.
* Added `examples/DateRanges.R` — a self-contained runnable script with the
  same scenarios for quick interactive exploration.

# ksformat 0.6.6

## New features

* `date_range` and `datetime_range` format types: bucket Date or POSIXct
  values into character labels using interval bounds written as ISO
  date/datetime strings. Both types reuse the numeric range-table fast
  path (sorted-disjoint ranges hit `findInterval()` in C).
* `fnew()` accepts `type = "date_range"` / `"datetime_range"` and a new
  optional `date_format` argument for parsing custom bound strings.
* `fparse()` accepts interval notation with date/datetime bounds, e.g.
  `[2024-01-01, 2025-01-01) = "FY24"` and
  `[2024-01-01 08:00, 2024-01-01 16:00) = "Day"`. The type is auto-detected
  when no explicit subtype is given.
* `fexport()` renders date/datetime range bounds as ISO strings, enabling
  full roundtrip through `fparse()`.
* Bounds support `LOW` / `HIGH` keywords (rendered as `-Inf` / `+Inf` and
  emitted back as `LOW` / `HIGH` on export) and exclusive `(` / `)`
  brackets.

# ksformat 0.6.5

## Performance

* Precomputed range table: `ks_format` objects now carry a pre-built
  `range_table` field. Range keys are parsed once at format-creation time
  (in `fnew()`, `fparse()`, `fimport()`) rather than on every `fput()` call.
* `findInterval()` fast path in `fput()`: sorted, non-overlapping numeric
  ranges with standard `[low, high)` semantics now use R's built-in
  `findInterval()` (O(n log k) in C), giving a ~10–14× speedup over the
  previous per-range R loop on large inputs (benchmarked at 1M rows).
* `skip_discrete` optimisation in `fput()`: pure numeric-range formats
  (no discrete keys) with numeric input now skip the `as.character()` +
  `match()` step entirely.
* `is_missing()`: removed a redundant `is.nan()` pass on numeric vectors —
  `is.na()` already returns `TRUE` for `NaN`.

## Bug Fixes

* `.build_range_table()`: ranges defined out-of-order are now sorted by
  `(low, high)` before storage, so the `findInterval` fast path triggers
  regardless of definition order.
* `.build_range_table()`: removed a dead `attr<-` loop that silently had
  no effect (subsetting a character vector creates a copy, stripping any
  attribute set on the subset).

# ksformat 0.6.4

## New Features

* Added `franges()`: extracts all range entries from a `ks_format` object as a
  tidy `data.frame` with columns `low`, `high`, `inc_low`, `inc_high`, and
  `label`. Accepts either a `ks_format` object or a registered format name.
* Added `fmap_to_ranges()`: reverse-looks up range bounds by label — given a
  vector of coded values that match range labels in a format, returns the
  corresponding `low` / `high` bounds (and inclusivity flags) per element.

# ksformat 0.6.3

## Bug Fixes

* Fixed `fput()` (and `fputk()`) losing the `tzone` attribute when returning
  `POSIXct` values from a value-type format. The result vector was seeded from
  a tzone-less NA, so values were silently displayed in the local timezone
  instead of the source timezone (e.g. UTC), causing an apparent time-shift.
  The `tzone` attribute of the mapped values is now propagated to the result.

# ksformat 0.6.2

## New Features

* Added `format_library_app()` Shiny browser for the global format library,
  including filters by name/object type, per-object details, and mapping table
  rendering for both `ks_format` and `ks_invalue` entries.
* Added an RStudio addin entry (**Format Library Browser**) to launch
  `format_library_app()` from the Addins menu.

## Enhancements

* Replaced plain-text print preview in the app with a formatted mapping table
  that shows mappings, range rules, and special directives (`.missing`,
  `.other`) in structured form.
* Added app management UX improvements: explicit quit action with confirmation
  modal, plus confirmations for destructive clear/remove operations.

## Documentation

* Added `format_library_app()` docs and package index references.
* Updated README with interactive browser usage and behavior summary.

# ksformat 0.6.1

## Documentation

* Cheat sheet updated: added `fputk()` examples (basic composite key,
  `fmap()` data-driven Date lookup, `fparse()` text-defined Date lookup),
  added `fmap()` to the Function Reference table, and rearranged layout to
  fit landscape page.
* Vignette Example 22 added: "Date Lookup via `fparse()` and `fputk()`" —
  character lookup, native Date lookup with `fmap()`, and round-trip via
  `fexport()`/`fparse()`.

# ksformat 0.6.0

## Breaking Changes

* `fnew()` no longer accepts the `reverse` parameter. Use `fmap(keys, values)`
  instead of `setNames(values, keys)` with `reverse = FALSE` to create
  data-driven formats consistently for all types.

## New Features

* New `fmap(keys, values)` helper creates a key-value mapping that tells
  `fnew()` to use the natural direction (keys → values) without reversal.
  Works identically for character, numeric, Date, POSIXct, and logical formats.

## Documentation

* Vignette Example 21 rewritten: "Consistent Data-Driven Formats with
  `fmap()`" — demonstrates how `fmap(keys, values)` replaces the old
  `setNames()` + `reverse = FALSE` pattern.
* Updated Example 20 (Composite Key Lookup) to use `fmap()`.

# ksformat 0.5.1

## Bug Fixes

* Fixed Example 20 (Composite Key Lookup) in vignette: added missing
  `reverse = FALSE` to the character format created with `setNames()`, which
  caused all lookups to return "NOT FOUND".

# ksformat 0.5.0

## New Features

* `fnew()` gains a `reverse` parameter for explicit control over named-vector
  direction. Set `reverse = FALSE` to use the natural `setNames(values, keys)`
  convention consistently for all format types — character, numeric, Date,
  POSIXct, and logical. The default (`NULL`) preserves backward-compatible
  auto-detection: reversal for character/numeric, no reversal for value types.

## Documentation

* New vignette Example 21: "Consistent Data-Driven Formats with `reverse`" —
  demonstrates how to build lookup formats from data frames using the same
  `setNames()` pattern for all output types.
* Updated `fnew()` documentation with detailed explanation of the named-vector
  reversal convention and the new `reverse` parameter.
