# Parse Cache for Expression Labels

Caches parsed R expressions (from
[`parse()`](https://rdrr.io/r/base/parse.html)) keyed by the expression
string to avoid repeated parsing on successive [`fput()`](fput.md)
calls.

## Usage

``` r
.expr_parse_cache
```
