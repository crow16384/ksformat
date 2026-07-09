# Apply Format Using a Composite Key

Convenience wrapper around \[fput()\] that pastes multiple vectors
together into a composite key before lookup. Useful when a format is
keyed on the combination of several columns (e.g.,
\`USUBJID\|VISITNUM\`).

## Usage

``` r
fputk(..., format, sep = "|", keep_na = FALSE, na_as_string = FALSE)
```

## Arguments

- ...:

  Vectors to paste together into a composite key. All vectors are
  recycled to a common length by \[paste()\].

- format:

  A \[ks_format\] object or a registered format name (character string).

- sep:

  Separator inserted between the pasted components (default \`"\|"\`).

- keep_na:

  If \`TRUE\`, \`NA\` inputs remain \`NA\` in the output instead of
  being mapped via \`.missing\`. Passed through to \[fput()\].

- na_as_string:

  If \`FALSE\` (default), an \`NA\` in any component propagates to the
  composite key (restored to \`NA_character\_\` after the \[paste()\]
  step) so that \[fput()\] can apply \`.missing\` handling. If \`TRUE\`,
  the literal string \`"NA"\` produced by \[paste()\] is kept, which is
  useful when the format was built with composite keys via
  \`fmap(paste(..., sep = "\|"), values)\` — because \[paste()\]
  converts \`NA\` to \`"NA"\` on both sides, the round-trip lookup then
  matches.

## Value

A character vector of formatted labels, the same length as the
(recycled) input vectors.

## See also

\[fput()\], \[fputn()\], \[fputc()\], \[finputk()\]

## Examples

``` r
# Build a lookup keyed on two columns
fnew(
  "A|1" = "2025-01-15",
  "A|2" = "2025-02-20",
  "B|1" = "2025-03-10",
  .other = "NOT FOUND",
  name = "visit_date",
  type = "character"
)

subj  <- c("A", "A", "B", "B")
visit <- c(1, 2, 1, 3)

fputk(subj, visit, format = "visit_date")
#> [1] "2025-01-15" "2025-02-20" "2025-03-10" "NOT FOUND" 
# -> "2025-01-15" "2025-02-20" "2025-03-10" "NOT FOUND"

fclear()
#> All formats cleared from library.

# Composite key with NA components matching a paste()-built format
fnew(
  fmap(
    paste(c("CHEM", "COAG"), c("ALB", "INR"), c("g/L", NA), sep = "|"),
    c("ALB", "INR")
  ),
  name = "lb_param", type = "character"
)

fputk(c("CHEM", "COAG"), c("ALB", "INR"), c("g/L", NA),
      format = "lb_param", na_as_string = TRUE)
#> [1] "ALB" "INR"
# -> "ALB" "INR"

fclear()
#> All formats cleared from library.
```
