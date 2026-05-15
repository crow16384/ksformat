# Composite Key Lookup with NA Components — companion script for Examples 28-29
#
# Demonstrates `na_as_string = TRUE` in `fputk()` and the new `finputk()`
# function for cases where format keys were built via `fmap(paste(...), ...)`.

library(ksformat)

# ---------------------------------------------------------------------------
# Example 28: fputk() with na_as_string = TRUE
# ---------------------------------------------------------------------------

# Source lab mapping (some tests have no unit → LBSTRESU = NA)
lb_map <- data.frame(
  LBCAT    = c("BLOOD CHEMISTRY", "COAGULOGRAM", "COAGULATION PANEL", "COAGULOGRAM"),
  LBSPEC   = c("BLOOD",           "BLOOD",        "BLOOD",             "BLOOD"),
  LBTESTCD = c("ALB",             "FIBRINO",      "INR",               "INR"),
  LBSTRESU = c("g/L",             "g/L",           NA,                  NA),
  PARAMCD  = c("ALB",             "FIBRINO",       "INR",               "INR"),
  stringsAsFactors = FALSE
)

# Build format: paste() stores "NA" literally for missing LBSTRESU
with(lb_map,
  fmap(paste(LBCAT, LBSPEC, LBTESTCD, LBSTRESU, sep = "|"), PARAMCD)
) |>
  fnew(ignore_case = TRUE, .other = NA,
       type = "character", name = "lb_param")

fprint("lb_param")

# Default (na_as_string = FALSE): NA components → real NA → no match
lb_map$PARAMCD_default <- with(lb_map,
  fputk(LBCAT, LBSPEC, LBTESTCD, LBSTRESU, format = "lb_param")
)
cat("\n--- Default (na_as_string = FALSE) ---\n")
print(lb_map[, c("LBTESTCD", "LBSTRESU", "PARAMCD", "PARAMCD_default")])

# na_as_string = TRUE: NA → "NA" string → matches stored key → correct result
lb_map$PARAMCD_back <- with(lb_map,
  fputk(LBCAT, LBSPEC, LBTESTCD, LBSTRESU,
        format = "lb_param", na_as_string = TRUE)
)
cat("\n--- na_as_string = TRUE ---\n")
print(lb_map[, c("LBTESTCD", "LBSTRESU", "PARAMCD", "PARAMCD_back")])

fclear()

# ---------------------------------------------------------------------------
# Example 29: finputk() — reverse lookup with composite labels
# ---------------------------------------------------------------------------

# --- 29a. Basic two-column reverse lookup ----------------------------------

finput(
  fmap(paste(c("BLOOD CHEMISTRY", "COAGULOGRAM", "COAGULATION PANEL"),
             c("ALB",             "FIBRINO",      "INR"),
             sep = "|"),
       c(1L, 2L, 3L)),
  target_type = "integer",
  name = "lb_code_inv"
)

cat_vec  <- c("BLOOD CHEMISTRY", "COAGULOGRAM", "COAGULATION PANEL", "OTHER")
test_vec <- c("ALB",              "FIBRINO",     "INR",               "X")

result <- finputk(cat_vec, test_vec, invalue_name = "lb_code_inv")
cat("\n--- finputk basic ---\n")
print(data.frame(LBCAT = cat_vec, LBTESTCD = test_vec, code = result))

fclear()

# --- 29b. With NA components (na_as_string = TRUE) -------------------------

finput(
  fmap(
    paste(lb_map$LBCAT, lb_map$LBTESTCD, lb_map$LBSTRESU, sep = "|"),
    seq_len(nrow(lb_map))
  ),
  target_type = "integer",
  name = "lb_row_inv"
)

row_idx <- finputk(lb_map$LBCAT, lb_map$LBTESTCD, lb_map$LBSTRESU,
                   invalue_name = "lb_row_inv", na_as_string = TRUE)
cat("\n--- finputk with na_as_string = TRUE ---\n")
print(data.frame(LBTESTCD = lb_map$LBTESTCD, LBSTRESU = lb_map$LBSTRESU,
                 row_idx = row_idx))

fclear()
