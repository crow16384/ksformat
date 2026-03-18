# Example usage of ksformat package

# This script demonstrates how to use the ksformat package
# Run this after installing the package with: devtools::install()

library(ksformat)

# ============================================================================
# Example 1: Basic discrete value formatting
# ============================================================================

cat("\n=== Example 1: Basic Discrete Formatting ===\n")

# Create a format for gender codes (auto-stored in library as "sex")
fnew(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  .other = "Other Gender",
  name = "sex"
)

# Apply format using name from library
gender_codes <- c("M", "F", "M", NA, "X", "F")
formatted_genders <- fput(gender_codes, "sex")

print(data.frame(
  code = gender_codes,
  label = formatted_genders
))

# Show registered format
fprint("sex")

# ============================================================================
# Example 2: Numeric range formatting with interval notation
# ============================================================================

cat("\n=== Example 2: Numeric Range Formatting ===\n")

# Define formats in SAS-like text (auto-registered)
fparse(text = '
VALUE age (numeric)
  [0, 18)     = "Child"
  [18, 65)    = "Adult"
  [65, HIGH]  = "Senior"
  .missing    = "Age Unknown"
;
')

# Apply to age data using fputn (numeric format by name)
ages <- c(5, 15.3, 17.9, 18, 45, 64.99, 65, 85, NA)
age_groups <- fputn(ages, "age")

print(data.frame(
  age = ages,
  group = age_groups
))

# ============================================================================
# Example 3: Decimal ranges (e.g. BMI categories)
# ============================================================================

cat("\n=== Example 3: Decimal Ranges ===\n")

fparse(text = '
VALUE bmi (numeric)
  [0, 18.5)    = "Underweight"
  [18.5, 25)   = "Normal"
  [25, 30)     = "Overweight"
  [30, HIGH]   = "Obese"
  .missing     = "No data"
;
')

bmi_values <- c(16.2, 18.5, 22.7, 25, 29.9, 35.1, NA)
bmi_labels <- fputn(bmi_values, "bmi")

print(data.frame(
  bmi = bmi_values,
  category = bmi_labels
))

# ============================================================================
# Example 4: Exclusive/inclusive bounds
# ============================================================================

cat("\n=== Example 4: Bound Inclusivity ===\n")

fparse(text = '
VALUE score (numeric)
  (0, 50]    = "Low"
  (50, 100]  = "High"
  .other     = "Out of range"
;
')

scores <- c(0, 1, 50, 51, 100, 101)
score_labels <- fputn(scores, "score")

print(data.frame(
  score = scores,
  label = score_labels
))

# ============================================================================
# Example 5: Reverse formatting with invalue (numeric by default)
# ============================================================================

cat("\n=== Example 5: Reverse Formatting (Invalue) ===\n")

# Create numeric invalue (default target_type is "numeric")
finput(
  "Male" = 1,
  "Female" = 2,
  name = "sex_inv"
)

# Apply invalue using name
labels <- c("Male", "Female", "Male", "Unknown", "Female")
codes <- finputn(labels, "sex_inv")

print(data.frame(
  label = labels,
  code = codes
))

# ============================================================================
# Example 6: Bidirectional formatting
# ============================================================================

cat("\n=== Example 6: Bidirectional Formatting ===\n")

status_bi <- fnew_bid(
  "A" = "Active",
  "I" = "Inactive",
  "P" = "Pending",
  name = "status"
)

# Test forward formatting using fputc (character format by name)
status_codes <- c("A", "I", "P", "A")
status_labels <- fputc(status_codes, "status")

cat("\nForward (code -> label):\n")
print(data.frame(code = status_codes, label = status_labels))

# Test reverse formatting using finputc
test_labels <- c("Active", "Pending", "Inactive")
test_codes <- finputc(test_labels, "status_inv")

cat("\nReverse (label -> code):\n")
print(data.frame(label = test_labels, code = test_codes))

# ============================================================================
# Example 7: Parse multiple formats from text
# ============================================================================

cat("\n=== Example 7: Multi-format Parsing ===\n")

fparse(text = '
// Study format definitions

VALUE race (character)
  "W" = "White"
  "B" = "Black"
  "A" = "Asian"
  .missing = "Unknown"
;

INVALUE race_inv
  "White" = 1
  "Black" = 2
  "Asian" = 3
;
')

# Show all registered formats
fprint()

# ============================================================================
# Example 8: Export formats back to text
# ============================================================================

cat("\n=== Example 8: Export Formats to Text ===\n")

# Retrieve format from library and export
bmi_fmt <- format_get("bmi")
cat(fexport(bmi = bmi_fmt))
cat("\n")

# ============================================================================
# Example 9: SAS-like PUT/INPUT functions
# ============================================================================

cat("\n=== Example 9: SAS-like PUT/INPUT ===\n")

# fputn - apply numeric format by name
cat("fputn:", fputn(c(5, 30, 70), "age"), "\n")

# fputc - apply character format by name
cat("fputc:", fputc(c("M", "F"), "sex"), "\n")

# finputn - apply numeric invalue by name
cat("finputn:", finputn(c("White", "Black"), "race_inv"), "\n")

# ============================================================================
# Example 10: Data frame formatting
# ============================================================================

cat("\n=== Example 10: Data Frame Formatting ===\n")

df <- data.frame(
  id = 1:6,
  sex = c("M", "F", "M", "F", NA, "X"),
  age = c(15, 25, 45, 70, 35, NA),
  stringsAsFactors = FALSE
)

cat("Original data:\n")
print(df)

sex_f <- format_get("sex")
age_f <- format_get("age")

df_formatted <- fput_df(
  df,
  sex = sex_f,
  age = age_f,
  suffix = "_label"
)

cat("\nFormatted data:\n")
print(df_formatted)

# ============================================================================
# Example 11: Missing value handling
# ============================================================================

cat("\n=== Example 11: Missing Value Handling ===\n")

cat("With .missing label:\n")
print(fput(c("M", "F", NA), "sex"))

cat("\nWith keep_na = TRUE:\n")
print(fput(c("M", "F", NA), sex_f, keep_na = TRUE))

cat("\nis_missing() (NA, NaN, and empty string are missing):\n")
cat("  NA:   ", is_missing(NA), "\n")
cat("  NaN:  ", is_missing(NaN), "\n")
cat("  '':   ", is_missing(""), "\n")
cat("  'x':  ", is_missing("x"), "\n")

# ============================================================================
# Example 12: Date/Time Formats (SAS-style)
# ============================================================================

cat("\n=== Example 12: Date/Time Formats ===\n")

# --- 12a: SAS date formats ---
cat("\n--- SAS Date Formats ---\n")

# Apply SAS date format directly by name (auto-resolved, no pre-creation needed)
today <- Sys.Date()
cat("DATE9.  :", fputn(today, "DATE9."), "\n")
cat("MMDDYY10.:", fputn(today, "MMDDYY10."), "\n")
cat("DDMMYY10.:", fputn(today, "DDMMYY10."), "\n")
cat("YYMMDD10.:", fputn(today, "YYMMDD10."), "\n")
cat("MONYY7. :", fputn(today, "MONYY7."), "\n")
cat("WORDDATE.:", fputn(today, "WORDDATE."), "\n")
cat("YEAR4.  :", fputn(today, "YEAR4."), "\n")
cat("QTR.    :", fputn(today, "QTR."), "\n")

# Multiple dates at once
dates <- as.Date(c("2020-01-15", "2020-06-30", "2020-12-25"))
cat("\nDATE9 vector:", fputn(dates, "DATE9."), "\n")

# --- 12b: R numeric dates (days since 1970-01-01) ---
cat("\n--- R Numeric Dates (epoch 1970-01-01) ---\n")

r_days <- as.numeric(as.Date("2025-01-01"))
cat("R days for 2025-01-01:", r_days, "\n")
cat("DATE9 from R days:    ", fputn(r_days, "DATE9."), "\n")
cat("MMDDYY10 from R days: ", fputn(r_days, "MMDDYY10."), "\n")

# --- 12c: Time formats ---
cat("\n--- Time Formats ---\n")

# Time as seconds since midnight
seconds <- c(0, 3600, 45000, 86399)
cat("TIME8.  :", fputn(seconds, "TIME8."), "\n")
cat("TIME5.  :", fputn(seconds, "TIME5."), "\n")
cat("HHMM.   :", fputn(seconds, "HHMM."), "\n")

cat("\nMeaning:\n")
print(data.frame(
  seconds = seconds,
  time8 = fputn(seconds, "TIME8."),
  time5 = fputn(seconds, "TIME5.")
))

# --- 12d: Datetime formats ---
cat("\n--- Datetime Formats ---\n")

now <- Sys.time()
cat("DATETIME20.:", fputn(now, "DATETIME20."), "\n")
cat("DATETIME13.:", fputn(now, "DATETIME13."), "\n")
cat("DTDATE.    :", fputn(now, "DTDATE."), "\n")
cat("DTYYMMDD.  :", fputn(now, "DTYYMMDD."), "\n")

# R numeric datetime (seconds since 1970-01-01 00:00:00)
r_secs <- as.numeric(as.POSIXct("2025-06-15 14:30:00", tz = "UTC"))
cat("\nR seconds for 2025-06-15 14:30:00:", r_secs, "\n")
cat("DATETIME20 from R secs:", fputn(r_secs, "DATETIME20."), "\n")

# --- 12e: Create named date format with fnew_date ---
cat("\n--- Custom Date Formats ---\n")

# SAS-named format
fnew_date("DATE9.", name = "bday_fmt")
birthdays <- as.Date(c("1990-03-25", "1985-11-03", "2000-07-14"))
cat("Birthdays (DATE9):", fput(birthdays, "bday_fmt"), "\n")

# Custom strftime pattern (e.g., Russian style: DD.MM.YYYY)
fnew_date("%d.%m.%Y", name = "ru_date", type = "date")
cat("Birthdays (DD.MM.YYYY):", fput(birthdays, "ru_date"), "\n")

# Custom pattern with missing label
fnew_date("MMDDYY10.", name = "us_date", .missing = "NO DATE")
mixed <- c(as.Date("2025-01-01"), NA, as.Date("2025-12-31"))
cat("Mixed (with NA):", fput(mixed, "us_date"), "\n")

# Show the datetime format object
fprint("bday_fmt")

# --- 12f: Date formats in data frames ---
cat("\n--- Date Formats in Data Frame ---\n")

patients <- data.frame(
  id = 1:4,
  visit_date = as.Date(c("2025-01-10", "2025-02-15", "2025-03-20", NA)),
  stringsAsFactors = FALSE
)

visit_fmt <- fnew_date("DATE9.", name = "visit_fmt", .missing = "NOT RECORDED")
patients_fmt <- fput_df(patients, visit_date = visit_fmt)
print(patients_fmt)

# --- 12g: Parse date format from text ---
cat("\n--- Parse Date Format from Text ---\n")

fparse(text = '
VALUE enrldt (date)
  pattern = "DATE9."
  .missing = "Not Enrolled"
;

VALUE visit_time (time)
  pattern = "TIME8."
;

VALUE stamp (datetime)
  pattern = "DATETIME20."
;
')

fprint()

# Apply the parsed formats
cat("enrldt:", fput(as.Date("2025-03-01"), "enrldt"), "\n")
cat("visit_time:", fput(36000, "visit_time"), "\n")
cat("stamp:", fput(as.POSIXct("2025-03-01 10:00:00", tz = "UTC"), "stamp"), "\n")

# Export back to text
enrl_obj <- format_get("enrldt")
cat("\nExported text:\n")
cat(fexport(enrldt = enrl_obj))
cat("\n")

fclear()

# ============================================================================
# Example 13: Multilabel Formats
# ============================================================================

cat("\n=== Example 13: Multilabel Formats ===\n")

# --- 13a: Basic multilabel with overlapping ranges ---
cat("\n--- Overlapping Age Categories ---\n")

# In clinical data, a patient can belong to multiple categories
fnew(
  "0,5,TRUE,TRUE"   = "Infant",
  "6,11,TRUE,TRUE"  = "Child",
  "12,17,TRUE,TRUE" = "Adolescent",
  "0,17,TRUE,TRUE"  = "Pediatric",
  "18,64,TRUE,TRUE" = "Adult",
  "65,Inf,TRUE,TRUE" = "Elderly",
  "18,Inf,TRUE,TRUE" = "Non-Pediatric",
  name = "age_categories",
  type = "numeric",
  multilabel = TRUE
)

ages <- c(3, 14, 25, 70)

# fput returns first match only
cat("fput (first match):\n")
cat("  ", fput(ages, "age_categories"), "\n")

# fput_all returns ALL matching labels
cat("\nfput_all (all matches):\n")
all_labels <- fput_all(ages, "age_categories")
for (i in seq_along(ages)) {
  cat("  Age", ages[i], "->", paste(all_labels[[i]], collapse = ", "), "\n")
}

# --- 13b: Multilabel with missing values ---
cat("\n--- Multilabel with Missing Values ---\n")

fnew(
  "0,100,TRUE,TRUE"    = "Valid Score",
  "0,49,TRUE,TRUE"     = "Below Average",
  "50,100,TRUE,TRUE"   = "Above Average",
  "90,100,TRUE,TRUE"   = "Excellent",
  .missing = "No Score",
  .other = "Out of Range",
  name = "score_ml",
  type = "numeric",
  multilabel = TRUE
)

scores <- c(95, 45, NA, 150)
ml_result <- fput_all(scores, "score_ml")

for (i in seq_along(scores)) {
  cat("  Score", ifelse(is.na(scores[i]), "NA", scores[i]),
      "->", paste(ml_result[[i]], collapse = ", "), "\n")
}

# --- 13c: Parse multilabel from text ---
cat("\n--- Parse Multilabel from Text ---\n")

fparse(text = '
VALUE risk (numeric, multilabel)
  [0, 3]   = "Low Risk"
  [0, 7]   = "Monitored"
  (3, 7]   = "Medium Risk"
  (7, 10]  = "High Risk"
;
')

risk_scores <- c(2, 5, 9)
cat("Risk scores:\n")
risk_labels <- fput_all(risk_scores, "risk")
for (i in seq_along(risk_scores)) {
  cat("  Score", risk_scores[i], "->",
      paste(risk_labels[[i]], collapse = " | "), "\n")
}

# --- 13d: Multilabel export ---
cat("\n--- Multilabel Export ---\n")

risk_obj <- format_get("risk")
cat(fexport(risk = risk_obj))
cat("\n")

# Show format object
fprint("risk")

# --- 13e: Practical example: adverse event severity grading ---
cat("\n--- AE Severity Grading (Multilabel) ---\n")

fnew(
  "1,1,TRUE,TRUE" = "Mild",
  "2,2,TRUE,TRUE" = "Moderate",
  "3,3,TRUE,TRUE" = "Severe",
  "4,4,TRUE,TRUE" = "Life-threatening",
  "5,5,TRUE,TRUE" = "Fatal",
  "3,5,TRUE,TRUE" = "Serious",
  "1,2,TRUE,TRUE" = "Non-serious",
  name = "ae_grade",
  type = "numeric",
  multilabel = TRUE
)

grades <- c(1, 2, 3, 4, 5)
cat("Grade classifications:\n")
ae_labels <- fput_all(grades, "ae_grade")
for (i in seq_along(grades)) {
  cat("  Grade", grades[i], ":",
      paste(ae_labels[[i]], collapse = " + "), "\n")
}

# Clean up
fclear()

# ===========================================================================
# Example 14: Case-Insensitive Matching
# ===========================================================================
cat("\n=== Example 14: Case-Insensitive Matching ===\n\n")

# Create a character format with ignore_case = TRUE
sex_nc <- fnew(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  name = "sex_nc",
  type = "character",
  ignore_case = TRUE
)


# Mixed-case input is matched regardless of case
input <- c("m", "F", "M", "f", NA)
cat("Input:  ", paste(input, collapse = ", "), "\n")
cat("Result: ", paste(fput(input, sex_nc), collapse = ", "), "\n\n")

# Show the format - note the [nocase] flag
fprint("sex_nc")

# Also works with fputc
cat("\nfputc('m', 'sex_nc'):", fputc("m", "sex_nc"), "\n")

fclear()

# ===========================================================================
# Example 15: Expression Labels in Formats
# ===========================================================================
cat("\n=== Example 15: Expression Labels in Formats ===\n\n")

# Expression labels contain .x1, .x2, ... which reference extra arguments
# passed to fput(). This lets you compute labels dynamically.

# --- Example 15a: Simple sprintf expression ---
cat("--- 15a: sprintf expression ---\n")
stat_fmt <- fnew(
  "n"   = "sprintf('%s', .x1)",
  "pct" = "sprintf('%.1f%%', .x1 * 100)",
  name = "stat",
  type = "character"
)

types  <- c("n",  "pct",  "n",   "pct")
values <- c(42,   0.053,  100,   0.255)

cat("Types:  ", paste(types, collapse = ", "), "\n")
cat("Values: ", paste(values, collapse = ", "), "\n")
# Pass values as extra positional arg -> .x1
cat("Result: ", paste(fput(types, stat_fmt, values), collapse = ", "), "\n\n")

# --- Example 15b: Expressions with two extra args ---
cat("--- 15b: Two extra arguments (.x1, .x2) ---\n")
ratio_fmt <- fnew(
  "ratio" = "sprintf('%s/%s', .x1, .x2)",
  name = "ratio",
  type = "character"
)

cat("fput('ratio', ratio_fmt, 3, 10):", fput("ratio", ratio_fmt, 3, 10), "\n")
cat("fput(c('ratio','ratio'), ratio_fmt, c(3,7), c(10,20)):",
    paste(fput(c("ratio", "ratio"), ratio_fmt, c(3, 7), c(10, 20)),
          collapse = ", "), "\n\n")

# --- Example 15c: ifelse expression ---
cat("--- 15c: ifelse expression ---\n")
sign_fmt <- fnew(
  "val" = "ifelse(.x1 > 0, paste0('+', .x1), as.character(.x1))",
  name = "sign",
  type = "character"
)

nums <- c(5, 0, -3)
cat("Values: ", paste(nums, collapse = ", "), "\n")
cat("Result: ", paste(fput(rep("val", 3), sign_fmt, nums), collapse = ", "), "\n\n")

# --- Example 15d: Mixed static and expression labels ---
cat("--- 15d: Mixed static and expression labels ---\n")
mixed_fmt <- fnew(
  "header" = "HEADER",
  "n"      = "sprintf('N=%s', .x1)",
  "pct"    = "sprintf('%.1f%%', .x1 * 100)",
  name = "mixed",
  type = "character"
)

keys <- c("header", "n", "pct", "header", "n")
vals <- c(0,        42,  0.15,  0,        100)
cat("Keys:   ", paste(keys, collapse = ", "), "\n")
cat("Values: ", paste(vals, collapse = ", "), "\n")
cat("Result: ", paste(fput(keys, mixed_fmt, vals), collapse = ", "), "\n\n")

# --- Example 15e: Expression in .other ---
cat("--- 15e: Expression in .other fallback ---\n")
known_fmt <- fnew(
  "ok" = "OK",
  .other = "sprintf('Error(%s)', .x1)",
  name = "err_fmt",
  type = "character"
)

codes   <- c("ok", "E01", "ok", "E99")
details <- c("",   "timeout", "", "overflow")
cat("Codes:  ", paste(codes, collapse = ", "), "\n")
cat("Result: ", paste(fput(codes, known_fmt, details), collapse = ", "), "\n\n")

# --- Example 15f: Scalar recycling ---
cat("--- 15f: Scalar recycling ---\n")
label_fmt <- fnew(
  "val" = "sprintf('%s (N=%s)', .x1, .x2)",
  name = "recycle",
  type = "character"
)

cat("fput(c('val','val'), label_fmt, c(42, 55), 100):\n")
cat("  ", paste(fput(c("val", "val"), label_fmt, c(42, 55), 100),
               collapse = ", "), "\n")

fclear()

# ============================================================================
# Example 16: Vectorized format names (SAS PUTC/PUTN with variable format)
# ============================================================================

cat("\n=== Example 16: Vectorized Format Names (SAS PUTC-style) ===\n")

# Define a "dispatch" format that maps type codes to format names
fnew("1" = "groupx", "2" = "groupy", "3" = "groupz",
     name = "typefmt", type = "numeric")

# Define per-group character formats
fnew("positive" = "agree",  "negative" = "disagree", "neutral" = "notsure",
     name = "groupx", type = "character")
fnew("positive" = "accept", "negative" = "reject",   "neutral" = "possible",
     name = "groupy", type = "character")
fnew("positive" = "pass",   "negative" = "fail",     "neutral" = "retest",
     name = "groupz", type = "character")

type     <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
response <- c("positive", "negative", "neutral",
              "positive", "negative", "neutral",
              "positive", "negative", "neutral")

# Step 1: map type -> format name
respfmt <- fput(type, "typefmt")

# Step 2: apply per-element format (vector of format names)
word <- fputc(response, respfmt)

print(data.frame(type = type, response = response, respfmt = respfmt, word = word))

fclear()

# ============================================================================
# Example 17: Working with Dates and Formats — PUTN with date formats
# ============================================================================
#
# SAS equivalent:
#   proc format; value writfmt 1='date9.' 2='mmddyy10.'; run;
#   data dates;
#     input number key;
#     datefmt = put(key, writfmt.);
#     date    = putn(number, datefmt);
#     datalines;
#     15756 1
#     14552 2
#     ;
#   proc print data=dates; title 'Working with Dates and Formats'; run;
#
# The PROC FORMAT step creates a format, WRITFMT., that formats the variable
# values 1 and 2 with the name of a SAS date format. The DATA step creates a
# SAS data set from raw data that consists of a number and a key. After reading
# a record, the DATA step uses the value of KEY to create a variable, DATEFMT,
# that contains the value of the appropriate date format. The DATA step also
# creates a new variable, DATE, whose value is the formatted value of the date.
# PUTN assigns the value of DATE based on the value of NUMBER and the
# appropriate format.

cat("\n=== Example 17: Working with Dates and Formats (SAS PUTN) ===\n\n")

# Step 1: Create a numeric format that maps key codes to date format names
fnew("1" = "date9.", "2" = "mmddyy10.",
     name = "writfmt", type = "numeric")

# Step 2: Register the SAS date formats (R epoch, days since 1970-01-01)
fnew_date("date9.")
fnew_date("mmddyy10.")

# Input data (R date numbers = days since 1970-01-01)
number <- c(12103, 10899)
key    <- c(1, 2)

# Step 3: Look up format name per observation (like SAS PUT(key, writfmt.))
datefmt <- fputn(key, "writfmt")

# Step 4: Apply per-element date format (like SAS PUTN(number, datefmt))
date <- fputn(number, datefmt)

cat("Working with Dates and Formats\n")
result <- data.frame(number = number, key = key, datefmt = datefmt, date = date)
print(result)

fclear()

# ============================================================================
# Example 18: Import SAS Formats from CNTLOUT CSV (fimport)
# ============================================================================
#
# SAS equivalent:
#   /* Export format catalogue to CSV */
#   proc format library=work cntlout=fmtlib; run;
#   proc export data=fmtlib outfile="formats.csv" dbms=csv replace; run;
#
# The CNTLOUT dataset contains all format definitions from a SAS format
# catalogue. Each row describes one entry (discrete value or range bound)
# of a format. Column TYPE indicates the format kind:
#   N = numeric VALUE format  -> ks_format  (type = "numeric")
#   C = character VALUE format -> ks_format  (type = "character")
#   I = numeric INVALUE       -> ks_invalue (target_type = "numeric")
#   J = character INVALUE     -> ks_invalue (target_type = "character")
#   P = PICTURE format        -> skipped (no R equivalent)
#
# Rows with SAS special missing values (.A-.Z, ._) are skipped with a
# warning because R has no equivalent concept.

cat("\n=== Example 18: Import SAS Formats from CNTLOUT CSV ===\n\n")

# The package ships with a sample CNTLOUT CSV for testing.
# In real use, point to the CSV exported from your SAS environment.
csv_path <- system.file("tests/testthat/test_cntlout.csv",
                        package = "ksformat")
# Fallback for running from source tree:
if (csv_path == "") {
  csv_path <- file.path(
    dirname(dirname(normalizePath(".", mustWork = FALSE))),
    "tests", "testthat", "test_cntlout.csv"
  )
}
if (!file.exists(csv_path)) {
  csv_path <- "../tests/testthat/test_cntlout.csv"
}

cat("Importing from:", csv_path, "\n\n")

# --- 18a: Import all compatible formats ---
cat("--- 18a: Import all compatible formats ---\n")

# fimport reads the CSV, creates ks_format / ks_invalue objects,
# and registers them in the global format library (by default).
imported <- fimport(csv_path)

cat("Imported formats:\n")
print(names(imported))

# Show what's in the library now
fprint()

# --- 18b: Use the imported character format (GENDER) ---
cat("\n--- 18b: Use imported GENDER format ---\n")

gender_codes <- c("M", "F", NA, "X")
gender_labels <- fputc(gender_codes, "GENDER")

print(data.frame(
  code = gender_codes,
  label = gender_labels
))

# --- 18c: Use the imported numeric format (AGEGRP) ---
cat("\n--- 18c: Use imported AGEGRP format ---\n")

ages <- c(5, 17, 18, 45, 65, 100, NA, -1)
age_labels <- fputn(ages, "AGEGRP")

print(data.frame(
  age = ages,
  group = age_labels
))

# --- 18d: Use the imported numeric format (BMICAT) ---
cat("\n--- 18d: Use imported BMICAT format ---\n")

bmi_values <- c(15.0, 18.5, 22.3, 25.0, 28.7, 30.0, 35.5)
bmi_labels <- fputn(bmi_values, "BMICAT")

print(data.frame(
  bmi = bmi_values,
  category = bmi_labels
))

# --- 18e: Use the imported invalue (RACEIN) ---
cat("\n--- 18e: Use imported RACEIN invalue ---\n")

race_labels <- c("White", "Black", "Asian", "Other")
race_codes <- finputn(race_labels, "RACEIN")

print(data.frame(
  label = race_labels,
  code = race_codes
))

# --- 18f: Apply imported formats to a data frame ---
cat("\n--- 18f: Apply imported formats to data frame ---\n")

df <- data.frame(
  id = 1:5,
  sex = c("M", "F", "M", NA, "F"),
  age = c(10, 30, 70, NA, 50),
  stringsAsFactors = FALSE
)

cat("Original data:\n")
print(df)

gender_fmt <- imported[["GENDER"]]
age_fmt    <- imported[["AGEGRP"]]

df_fmt <- fput_df(df, sex = gender_fmt, age = age_fmt, suffix = "_label")

cat("\nFormatted data:\n")
print(df_fmt)

# --- 18g: Export an imported format back to text ---
cat("\n--- 18g: Export imported format back to text ---\n")

cat(fexport(AGEGRP = age_fmt))
cat("\n")
cat(fexport(GENDER = gender_fmt))
cat("\n")

# --- 18h: Selective import (register = FALSE) ---
cat("\n--- 18h: Selective import (no auto-register) ---\n")

fclear()

# Import without registering — returns list of objects to use manually
manual <- fimport(csv_path, register = FALSE)

cat("Library after import with register=FALSE:\n")
fprint()  # should be empty

# Use objects directly from the returned list
cat("\nUsing GENDER format from returned list:\n")
cat(fput(c("M", "F"), manual[["GENDER"]]), "\n")

# --- 19: Bilingual format ---
cat("\n--- 19: Create and apply bilingual format ---\n")
sex_bi <- fnew(
  "M" = "ifelse(.x1 == 'en', 'Male', 'Homme')",
  "F" = "ifelse(.x1 == 'en', 'Female', 'Femme')",
  .missing = "Unknown",   # single language only
  name = "sex_bi"
)
fput(c("M", "F", "M"), sex_bi, c("en", "fr", "en"))
# .x1 = language
# → "Male" "Femme" "Male"

# Create one format per language
fnew("M" = "Male",  "F" = "Female",  .missing = "Unknown", name = "sex_en")
fnew("M" = "Homme", "F" = "Femme",   .missing = "Inconnu", name = "sex_fr")
# Select language at apply-time
lang <- "fr"
fput(c("M", "F", NA), paste0("sex_", lang))
# -> "Homme" "Femme" "Inconnu"

fclear()
cat("\n=== Examples completed ===\n")

