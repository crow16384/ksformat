# Stratified Range Lookup — companion script for Example 26
#
# Demonstrates `stratified_range` formats: per-stratum numeric and date
# bucket boundaries dispatched via `fputk()`.

library(ksformat)

# --- 1. Programmatic numeric stratified format ----------------------------

visits <- fmap_strata(
  stratum = c("ARM_A", "ARM_A", "ARM_A", "ARM_B", "ARM_B"),
  low     = c(0,        7,       28,      0,       14),
  high    = c(7,        28,      Inf,     14,      Inf),
  label   = c("Baseline", "Wk1-3", "Wk4+", "Baseline", "Wk2+"),
  inc_high = c(FALSE, FALSE, TRUE, FALSE, TRUE)
)
fnew(visits, type = "stratified_range",
     ".other|ARM_A" = "A_outside",
     .other = "outside_window",
     name = "vw")

print(format_get("vw"))

df <- data.frame(
  arm = c("ARM_A", "ARM_A", "ARM_B", "ARM_B", "ARM_C"),
  day = c(3,        35,      5,       40,      10)
)
df$visit <- fputk(df$arm, df$day, format = "vw")
print(df)


# --- 2. Same format from text via fparse() --------------------------------

fclear()
fparse(text = '
VALUE vw_text (stratified_range, range_subtype: numeric)
  "ARM_A"|[0, 7)     = "Baseline"
  "ARM_A"|[7, 28)    = "Wk1-3"
  "ARM_A"|[28, HIGH] = "Wk4+"
  "ARM_B"|[0, 14)    = "Baseline"
  "ARM_B"|[14, HIGH] = "Wk2+"
  ".other|ARM_A"     = "A_outside"
  .other             = "outside_window"
  ;
')

print(fputk(df$arm, df$day, format = "vw_text"))


# --- 3. Per-subject date windows ------------------------------------------

fclear()
windows <- fmap_strata(
  stratum = c("S001", "S001", "S002", "S002"),
  low     = as.Date(c("2024-01-01", "2024-01-15",
                       "2024-02-01", "2024-02-20")),
  high    = as.Date(c("2024-01-15", "2024-02-01",
                       "2024-02-20", "2024-03-10")),
  label   = c("Screen", "Treat", "Screen", "Treat")
)
fnew(windows, type = "stratified_range", range_subtype = "date",
     .other = "off-window", name = "win")

subj   <- c("S001", "S001", "S002", "S002", "S003")
dates  <- as.Date(c("2024-01-05", "2024-01-20",
                     "2024-02-10", "2024-03-01", "2024-01-01"))
print(data.frame(
  subj  = subj,
  date  = dates,
  phase = fputk(subj, dates, format = "win")
))


# --- 4. Export / parse roundtrip ------------------------------------------

txt <- fexport(format_get("win"))
cat(txt, "\n")
fclear()
fparse(text = txt)
print(fputk(subj, dates, format = "win"))

fclear()
