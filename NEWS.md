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
