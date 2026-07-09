## NumericPatterns.R
## Demonstrates numeric pattern formats in ksformat.
##
## Pattern mode uses one unnamed %f-style token with optional literal
## prefix/suffix text, for example "$%,.2f" or "%.1f%%".

library(ksformat)

# ---------------------------------------------------------------------------
# 1) Currency format with grouping and custom missing/other handling
# ---------------------------------------------------------------------------

fnew(
  "$%,.2f",
  .missing = "NO DATA",
  .other = "INVALID",
  type = "numeric",
  name = "currency"
)

amounts <- c(1234.56, -7890.12, 0, NA, "oops")
cat("\n--- Currency pattern ---\n")
print(data.frame(raw = amounts, formatted = fput(amounts, "currency")))

# ---------------------------------------------------------------------------
# 2) Percent-like suffix via literal text
# ---------------------------------------------------------------------------

fnew("%.1f%%", type = "numeric", name = "pct")
vals <- c(0, 12.345, -7.8)

cat("\n--- Percent pattern ---\n")
print(data.frame(raw = vals, pct = fputn(vals, "pct")))

# ---------------------------------------------------------------------------
# 3) Keep NA unchanged when needed
# ---------------------------------------------------------------------------

cat("\n--- keep_na = TRUE ---\n")
print(fput(c(NA_real_, 10), "currency", keep_na = TRUE))

fclear()
