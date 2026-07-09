# Resolve a SAS format definition from the built-in table

Resolve a SAS format definition from the built-in table

## Usage

``` r
.resolve_sas_format_def(name)
```

## Arguments

- name:

  Character. SAS format name (e.g., "DATE9.", "MMDDYY10", "TIME8.")

## Value

A list with `r_fmt`, `type`, `toupper`, or `NULL`.
