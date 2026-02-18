# Test Coverage & Known Issues (updated 2026-02-18)

## Test File: tests/testthat/test-formats.R
Single test file, ~130+ test cases, ~2162 lines.

### Test Contexts
1. **Format Creation (fnew)** — discrete values, auto-registration, unnamed formats
2. **Format Application (fput)** — missing values, keep_na, format name from library
3. **SAS-like Functions (fputn, fputc, finputn, finputc)** — numeric/char format by name, type warnings
4. **Invalue Creation and Application (finput)** — reverse formatting, name from library, default numeric type
5. **Utility Functions** — is_missing detection
6. **Format Library (fprint, fclear)** — register/retrieve/remove cycle, show format list
7. **Bidirectional** — fnew_bid creates both format and invalue
8. **Format Parsing (fparse)** — VALUE/INVALUE blocks, ranges, LOW keyword, comments, auto-detect type, file input, errors, unquoted
9. **Format Export** — SAS-like text for VALUE/INVALUE, roundtrips, file output, multiple formats
10. **Range Bounds and Decimal Support** — interval notation, exclusive/inclusive bounds, decimal ranges, range matching
11. **INVALUE Numeric Default** — INVALUE without type defaults numeric
12. **Data Frame Formatting** — fput_df
13. **INVALUE Export** — omit numeric type, include non-default type
14. **Datetime** — fnew_date (date/time/TOD/datetime), auto-register, default widths, R-epoch, fput with various SAS formats
15. **Multilabel** — fput_all with overlapping ranges, NA, keep_na, .other
16. **Date format parsing** — fparse date/time/datetime blocks
17. **ignore_case / nocase** — fnew, fput, fparse, fexport with case-insensitive matching
18. **Expression labels** — .is_expr_label, fput with .x1/.x2, mixed labels, ifelse, .other
19. **Edge Cases: Format Creation** — empty mappings, default param, detect_format_type edge cases
20. **Edge Cases: Format Application** — empty vector, all-NA, NaN, single element, invalid format, non-numeric in range
21. **Edge Cases: fput_df** — replace mode, missing column warning, custom suffix, non-data.frame error
22. **Edge Cases: Invalue** — na_if, unknown labels fallback, custom missing_value, NULL input, target_type int/logical, errors
23. **Edge Cases: Utilities and Validation** — is_missing edge cases, range_spec errors, .format_validate warnings, fclear/fprint for non-existent
24. **Edge Cases: Expression Labels** — eval error handling
25. **Edge Cases: Format Parsing** — text as character vector, unclosed block, inline comments, multilabel+nocase, .missing in INVALUE
26. **Edge Cases: Format Export** — formats parameter, invalid object warning, no objects error
27. **Edge Cases: Datetime Formats** — invalid type, .missing, character dates, time strings, SAS format resolution
28. **Edge Cases: fput_all** — empty vector, all-NA, NaN, unmatched, invalid format
29. **Edge Cases: Library Management** — empty library fclear, single format removal, fnew_bid registration, SAS datetime auto-resolve

### Functions Well Tested
- fnew, fput, fputn, fputc, fput_all, fput_df
- finput, .invalue_apply, finputn, finputc, fnew_bid
- fnew_date, datetime formatting
- fparse, fexport, fimport (via CNTLOUT CSV)
- fprint, fclear
- is_missing, range_spec, in_range
- Expression labels, ignore_case, multilabel
- print.ks_format, print.ks_invalue

### Known Issues
- None currently tracked. Previous issues (range matching not implemented, stub functions) have been resolved.
