# Mark a Label for Expression Evaluation

Marks a format label string so it will be evaluated as an R expression
at apply-time ([`fput`](fput.md)), even when it does not contain `.x1`,
`.x2`, etc. placeholders.

## Usage

``` r
e(expr)
```

## Arguments

- expr:

  Character string. The R expression to evaluate.

## Value

The same character string with an `"eval"` attribute set to `TRUE`.

## Details

This is useful when a label should call a function that does not need
positional `.xN` arguments. The expression is evaluated in the caller's
environment of [`fput`](fput.md), so user-defined functions are
accessible.

Labels containing `.x1`, `.x2`, etc. are still evaluated automatically
without needing `e()`.

## Examples

``` r
# Mark an expression for evaluation at apply-time
fmt <- fnew(
  "timestamp" = e("format(Sys.time(), '%Y-%m-%d')"),
  "static"    = "Hello",
  name = "demo_eval"
)
fput(c("timestamp", "static"), fmt)
#> [1] "2026-07-09" "Hello"     
fclear()
#> All formats cleared from library.
```
