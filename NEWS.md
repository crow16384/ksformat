# ksformat 0.8.7

Documentation-only release; no behavior changes.

* The article **"Non-standard Applications of `ksformat` for Clinical Trials
      and Other Tasks"** now executes all example chunks at build time and
      displays their results (it previously shipped with chunk execution
      disabled, so examples appeared as code-only listings). All examples run
      cleanly and deterministically against the package itself, which also
      turns every future build into a live validation of the vignette code.

# ksformat 0.8.6

## Bug Fixes

* Comma-containing discrete keys no longer break numeric range machinery.
  Two layers: (1) `detect_format_type()` now classifies a comma key as a
  range spec only if it actually parses with the canonical range grammar
  (`low,high[,inc_low[,inc_high]]`) — plain text keys like
  `"San Francisco, CA"` stay `character` under `type = "auto"` instead of
  being forced to `numeric`. (2) `.build_range_table()` validates every
  parsed range: keys like `"1,000"` (parses as `low = 1, high = 0`) are
  degraded to discrete keys with a visible warning. Previously such formats
  passed `sorted_disjoint = TRUE` (the adjacency test is vacuous for
  degenerate intervals) and `fput()` aborted at apply time with
  `'vec' must be sorted non-decreasingly` from `findInterval()`.
  Date-looking auto keys keep the legacy warning path nudging
  `type = "date_range"`.

* Overlapping ranges in non-multilabel formats now match in **declaration
  order** (first-hit-wins, top-down as written). The `(low, high)` sort
  introduced in 0.6.5 for the `findInterval()` fast path had also reordered
  the general fallback loop, so precedence silently became "smallest `low`
  first" — a wide catch-all declared last could steal matches from narrow
  ranges declared earlier. The range table keeps the sorted layout for the
  fast path (disjoint ranges are order-insensitive anyway); the general path
  in `fput()`, `fput_all()`, and stratified matching now iterate a stored
  declaration-order view (`decl_iter`). `fput_all()` label sequence within an
  element now follows declaration order as well. Precedence for overlapping
  ranges is documented in `?fput` and the nonstandard-applications vignette.
  Old format objects serialized without `decl_iter` (pre-fix `.RDS`) keep the
  corrected semantics via a `sort_perm` fallback.

* `POSIXct` value-type formats now support range keys. Bounds were previously
  parsed as `Date`, so comparisons against `POSIXct` input dispatched
  incompatible `Ops` methods and every range lookup returned `NA` with
  warnings (in any timezone). Datetime bounds are now parsed as `POSIXct`
  (UTC epoch representation), input keeps its native class instead of
  lossy string round-tripping, and reversed bounds (`low > high`) degrade to
  discrete keys with a warning like numeric range keys.

* Vectorized `fputn()`/`fputc()` now warn when `format_name` contains `NA`
  (those elements return `NA`); previously they were silently skipped.

* `%OS` (fractional seconds) is kept for `POSIXct` input in `TIME11.`/`TOD11`/
  `DATETIME22.` and other `%OS` patterns — previously the fraction was
  silently dropped (strftime has no `%OS`). Numeric-seconds output now pads
  the integer part (`12:30:00.50`, not `12:30:0.50`).

* Numeric display patterns reject out-of-range width/digits at creation
  (`"%10000000000.2f"` aborted cryptically at apply time before).

* `ignore_case = TRUE` formats/invalues warn about case-insensitive
  duplicate keys (`"Yes"` + `"yes"`); previously only exact duplicates
  warned while nocase-colliding pairs silently pinned the first key.

* `fputk()`/`finputk()` validate component lengths (all non-scalar
  components must share one length, scalars recycle) instead of silently
  partial-recycling mismatched vectors through `paste()`.

* Empty (`""`) mapping keys are rejected by every creator (`fnew()`,
  `finput()`, `fparse()` VALUE/INVALUE blocks) with guidance to use
  `.missing` — they were unreachable at apply time (`is_missing("")` is
  `TRUE`), crashed `fnew()` cryptically, and were silently accepted by
  `fparse()`.

# ksformat 0.8.5

## Documentation

* Added an animated hero logo (`man/figures/ksformat-logo-hero.gif`,
  `vignettes/figures/`) shown at the top of the README and of each
  vignette; the GIF plays a value→label "format machine" sequence once
  and settles on the package logo. The generator is preserved in
  `scripts/gen_hero_gif.py`.
* Vignettes now declare `resource_files` so the logo asset ships and
  renders correctly in the installed package and on the pkgdown site.
* The pkgdown site gained a navbar with Foundations/Advanced article
  groups, a Changelog entry, a bundled cheatsheet PDF link, a landing
  page hero, and a "document" theme with numbered report-style sections
  (site-only files under `pkgdown/`, excluded from the tarball).

# ksformat 0.8.4

## New features

* Added `flevels()`: extracts discrete value-label mappings from a
  `ks_format` object (or registered format name) as a tidy two-column
  `data.frame` (`value`, `label`).
* `fnew()` now supports numeric pattern mode for `type = "numeric"`:
  pass one unnamed `%f`-style pattern (for example `"$%,.2f"` or
  `"%.1f%%"`) to format continuous numeric values directly.
* `fnew_bid()` gains an `ignore_case` argument. When `TRUE`, both the
  forward format and reverse invalue use case-insensitive matching.
  Default `FALSE` preserves existing behaviour.

## Documentation

* Added vignette **Example 31: Numeric Pattern Formatting** covering
  currency/grouping, suffix text, and `.missing`/`.other` handling.
* Added runnable companion script: `examples/NumericPatterns.R`.

# ksformat 0.8.1

## Documentation refresh

* Added a vignette example showing how to use `franges()` labels directly with
  `tidyr::complete()` and how to extract unique labels from the `aesev` format
  mappings without converting to `factor()`.

## Internal refactoring (no breaking changes)

* `R/format_parse.R` has been split in two: text-to-object parsing stays in
  `format_parse.R` (874 lines); object-to-text rendering and CSV CNTLOUT
  import/export (`fexport()`, `fimport()`, `.format_to_text()`, etc.) moves to
  the new `R/format_serialize.R` (755 lines).
* Three DRY internal helpers added to `utilities.R`:
  `.is_eval_label()`, `.parse_range_key_by_type()`, `.format_range_interval()`.
  Duplicate switch/if-chains across `format_apply.R`, `format_create.R`,
  and `format_library_app.R` now delegate to these helpers.
* Range-table now carries a `discrete_numeric_possible` flag computed at
  format creation time. `fput()` / `fput_all()` use it to skip the
  `as.character()` + `match()` discrete-key pass for numeric / Date / POSIXt
  inputs against formats whose discrete keys are all non-numeric strings,
  broadening the existing fast path.
* Friendly-interval regex in `.block_to_stratified_range_format()` is now
  built once per format parse instead of once per mapping entry.

# ksformat 0.7.2

## New features

* `finput()` gains an `ignore_case` argument. When `TRUE`, label lookup is
  case-insensitive (equivalent to SAS `INVALUE name (nocase)`). Default
  `FALSE` preserves existing behaviour.
* `fparse()` now correctly propagates the `(nocase)` option on `INVALUE`
  blocks to the resulting `ks_invalue` object. Previously the flag was
  parsed but silently discarded.
* `fexport()` / `fparse()` round-trips now preserve `ignore_case` for
  invalue objects: exporting a `nocase` invalue emits `INVALUE name (nocase)`
  which re-imports with `ignore_case = TRUE`.
* `print.ks_invalue()` now displays a `(nocase)` flag in the header when
  `ignore_case = TRUE`.

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
