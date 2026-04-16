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
