# ============================================================================
# Performance Benchmarks for ksformat
# ============================================================================
#
# Measures execution time of fput, fputn, fputc, fput_df, fput_all
# across various vector sizes and format configurations.
#
# Usage:
#   Rscript examples/benchmark_performance.R
#   # or in R:
#   source("examples/benchmark_performance.R")
#
# ============================================================================

library(ksformat)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' Run a single benchmark: evaluate expr `reps` times (default 5),
#' return median time in seconds and ops/sec.
bench <- function(label, expr, reps = 5L, n = NULL) {
  expr <- substitute(expr)
  env <- parent.frame()

  times <- numeric(reps)
  for (i in seq_len(reps)) {
    gc(verbose = FALSE, full = FALSE)
    t0 <- as.numeric(Sys.time())
    eval(expr, envir = env)
    t1 <- as.numeric(Sys.time())
    times[i] <- t1 - t0
  }

  med <- median(times)
  ops <- if (!is.null(n) && med > 0) round(n / med) else NA_real_

  list(label = label, n = n, median_sec = med, ops_per_sec = ops,
       times = times)
}

#' Pretty print a bench result
print_bench <- function(b) {
  n_str <- if (!is.null(b$n)) formatC(b$n, format = "d", big.mark = ",") else "?"
  time_str <- if (b$median_sec < 0.001) {
    sprintf("%.1f us", b$median_sec * 1e6)
  } else if (b$median_sec < 1) {
    sprintf("%.1f ms", b$median_sec * 1e3)
  } else {
    sprintf("%.2f s", b$median_sec)
  }
  ops_str <- if (!is.na(b$ops_per_sec)) {
    formatC(b$ops_per_sec, format = "d", big.mark = ",")
  } else {
    "-"
  }
  cat(sprintf("  %-50s  n=%12s  %10s  (%s ops/s)\n",
              b$label, n_str, time_str, ops_str))
}

# Collect all results
results <- list()
add <- function(b) {
  results[[length(results) + 1L]] <<- b
  print_bench(b)
}

# ---------------------------------------------------------------------------
# Setup: create formats used in benchmarks
# ---------------------------------------------------------------------------
cat("============================================================\n")
cat("ksformat Performance Benchmarks\n")
cat("============================================================\n\n")

cat("R version:", R.version.string, "\n")
cat("ksformat version:", as.character(packageVersion("ksformat")), "\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Character discrete format (small: 2 keys)
sex_fmt <- fnew("M" = "Male", "F" = "Female",
                .missing = "Unknown", name = "sex")

# Character discrete format (medium: 10 keys)
race_keys <- paste0("R", 1:10)
race_vals <- paste("Race", 1:10)
race_args <- setNames(as.list(race_vals), race_keys)
race_fmt <- do.call(fnew, c(race_args, list(.missing = "Unknown",
                                             .other = "Other",
                                             name = "race")))

# Character discrete format (large: 100 keys)
big_keys <- sprintf("K%03d", 1:100)
big_vals <- paste("Label", 1:100)
big_args <- setNames(as.list(big_vals), big_keys)
big_fmt <- do.call(fnew, c(big_args, list(name = "big100")))

# Numeric range format (5 ranges)
invisible(fparse(text = '
VALUE agecat (numeric)
  [0, 18)    = "Child"
  [18, 30)   = "Young Adult"
  [30, 50)   = "Middle-aged"
  [50, 65)   = "Pre-Senior"
  [65, HIGH] = "Senior"
  .missing   = "Age Unknown"
;
'))

# Numeric range format (20 ranges)
range_lines <- vapply(1:20, function(i) {
  lo <- (i - 1) * 5
  hi <- i * 5
  bracket <- if (i == 20) "]" else ")"
  sprintf("  [%d, %d%s = \"Group %d\"", lo, hi, bracket, i)
}, character(1))
invisible(fparse(text = paste(c("VALUE range20 (numeric)",
                               range_lines, ";"), collapse = "\n")))

# Multilabel format (5 overlapping ranges)
invisible(fparse(text = '
VALUE risk_ml (numeric, multilabel)
  [0, 3]  = "Low Risk"
  [0, 7]  = "Monitored"
  (3, 7]  = "Medium Risk"
  (7, 10] = "High Risk"
  [0, 10] = "Any Risk"
;
'))

# Date format
date_fmt <- fnew_date("DATE9.", name = "date9")

# Case-insensitive format
ci_fmt <- fnew("yes" = "Affirmative", "no" = "Negative",
               name = "yesno", ignore_case = TRUE)

# Expression label format
expr_fmt <- fnew("n" = "sprintf('%d', .x1)",
                 "pct" = "sprintf('%.1f%%', .x1 * 100)",
                 name = "stat_expr")

# ---------------------------------------------------------------------------
# Generate test data at various sizes
# ---------------------------------------------------------------------------
sizes <- c(100L, 1000L, 10000L, 100000L, 1000000L)
size_labels <- c("100", "1K", "10K", "100K", "1M")

# ============================================================================
cat("\n--- 1. fput: character discrete (2 keys) ---\n")
# ============================================================================
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- sample(c("M", "F", NA_character_), n, replace = TRUE)
  add(bench(
    paste0("char discrete 2-key [", size_labels[si], "]"),
    fput(x, sex_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 2. fput: character discrete (10 keys) ---\n")
# ============================================================================
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- sample(c(race_keys, NA_character_), n, replace = TRUE)
  add(bench(
    paste0("char discrete 10-key [", size_labels[si], "]"),
    fput(x, race_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 3. fput: character discrete (100 keys) ---\n")
# ============================================================================
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- sample(c(big_keys, NA_character_), n, replace = TRUE)
  add(bench(
    paste0("char discrete 100-key [", size_labels[si], "]"),
    fput(x, big_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 4. fput: numeric range (5 ranges) ---\n")
# ============================================================================
agecat_fmt <- ksformat:::.format_get("agecat")
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- c(runif(as.integer(n * 0.9), 0, 100), rep(NA_real_, as.integer(n * 0.1)))
  x <- sample(x)
  add(bench(
    paste0("num range 5-range [", size_labels[si], "]"),
    fput(x, agecat_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 5. fput: numeric range (20 ranges) ---\n")
# ============================================================================
range20_fmt <- ksformat:::.format_get("range20")
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- runif(n, 0, 100)
  add(bench(
    paste0("num range 20-range [", size_labels[si], "]"),
    fput(x, range20_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 6. fput: case-insensitive matching ---\n")
# ============================================================================
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- sample(c("YES", "Yes", "yes", "NO", "No", "no", NA_character_), n, replace = TRUE)
  add(bench(
    paste0("nocase [", size_labels[si], "]"),
    fput(x, ci_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 7. fput: expression labels ---\n")
# ============================================================================
for (si in seq_along(sizes[1:4])) {
  n <- sizes[si]
  x <- sample(c("n", "pct"), n, replace = TRUE)
  extra <- runif(n)
  add(bench(
    paste0("expr labels [", size_labels[si], "]"),
    fput(x, expr_fmt, extra),
    n = n
  ))
}

# ============================================================================
cat("\n--- 8. fputn / fputc by name ---\n")
# ============================================================================
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x_num <- runif(n, 0, 100)
  x_chr <- sample(c("M", "F"), n, replace = TRUE)
  add(bench(
    paste0("fputn by name [", size_labels[si], "]"),
    fputn(x_num, "agecat"),
    n = n
  ))
  add(bench(
    paste0("fputc by name [", size_labels[si], "]"),
    fputc(x_chr, "sex"),
    n = n
  ))
}

# ============================================================================
cat("\n--- 9. fput: date format (DATE9.) ---\n")
# ============================================================================
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- as.Date("2020-01-01") + sample(0:3650, n, replace = TRUE)
  add(bench(
    paste0("date DATE9. [", size_labels[si], "]"),
    fput(x, date_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 10. fput_all: multilabel (5 overlapping ranges) ---\n")
# ============================================================================
risk_ml <- ksformat:::.format_get("risk_ml")
for (si in seq_along(sizes[1:4])) {
  n <- sizes[si]
  x <- runif(n, 0, 10)
  add(bench(
    paste0("multilabel 5-range [", size_labels[si], "]"),
    fput_all(x, risk_ml),
    n = n
  ))
}

# ============================================================================
cat("\n--- 11. fput_df: data frame formatting ---\n")
# ============================================================================
for (si in seq_along(sizes[1:4])) {
  n <- sizes[si]
  df <- data.frame(
    sex = sample(c("M", "F", NA_character_), n, replace = TRUE),
    age = c(runif(as.integer(n * 0.9), 0, 100), rep(NA_real_, as.integer(n * 0.1))),
    stringsAsFactors = FALSE
  )
  add(bench(
    paste0("fput_df 2 cols [", size_labels[si], "]"),
    fput_df(df, sex = sex_fmt, age = agecat_fmt),
    n = n
  ))
}

# ============================================================================
cat("\n--- 12. finputn: invalue apply ---\n")
# ============================================================================
invisible(finput("Male" = 1, "Female" = 2, name = "sex_inv"))
sex_inv <- ksformat:::.format_get("sex_inv")
for (si in seq_along(sizes)) {
  n <- sizes[si]
  x <- sample(c("Male", "Female", "Other"), n, replace = TRUE)
  add(bench(
    paste0("finputn [", size_labels[si], "]"),
    finputn(x, "sex_inv"),
    n = n
  ))
}

# ============================================================================
cat("\n--- 13. fparse: parsing speed ---\n")
# ============================================================================
parse_text_small <- '
VALUE test_fmt (character)
  "A" = "Alpha"
  "B" = "Beta"
  "C" = "Gamma"
;
'
parse_text_large <- paste(c(
  "VALUE test_big (character)",
  vapply(1:200, function(i) sprintf('  "K%03d" = "Label %03d"', i, i), character(1)),
  "  .missing = \"Unknown\"",
  "  .other = \"Other\"",
  ";"
), collapse = "\n")

add(bench("fparse small (3 mappings)", invisible(fparse(text = parse_text_small)), reps = 20L, n = 3L))
add(bench("fparse large (200 mappings)", invisible(fparse(text = parse_text_large)), reps = 20L, n = 200L))

# ============================================================================
# Summary
# ============================================================================
cat("\n============================================================\n")
cat("Summary\n")
cat("============================================================\n\n")

cat(sprintf("%-50s  %12s  %10s  %14s\n",
            "Benchmark", "N", "Median", "Ops/sec"))
cat(paste(rep("-", 92), collapse = ""), "\n")

for (b in results) {
  n_str <- if (!is.null(b$n)) formatC(b$n, format = "d", big.mark = ",") else "?"
  time_str <- if (b$median_sec < 0.001) {
    sprintf("%.1f us", b$median_sec * 1e6)
  } else if (b$median_sec < 1) {
    sprintf("%.1f ms", b$median_sec * 1e3)
  } else {
    sprintf("%.2f s", b$median_sec)
  }
  ops_str <- if (!is.na(b$ops_per_sec)) {
    formatC(b$ops_per_sec, format = "d", big.mark = ",")
  } else {
    "-"
  }
  cat(sprintf("  %-50s  %12s  %10s  %14s\n",
              b$label, n_str, time_str, ops_str))
}

cat("\n")

# Cleanup
fclear()
