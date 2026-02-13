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

status_bi <- format_bidirectional(
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
bmi_fmt <- ksformat:::.format_get("bmi")
cat(format_export(bmi = bmi_fmt))
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

sex_f <- ksformat:::.format_get("sex")
age_f <- ksformat:::.format_get("age")

df_formatted <- format_apply_df(
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

cat("\nis_missing():\n")
cat("  NA:            ", is_missing(NA), "\n")
cat("  NaN:           ", is_missing(NaN), "\n")
cat("  '':            ", is_missing(""), "\n")
cat("  '' (w/ empty): ", is_missing("", include_empty = TRUE), "\n")

# Clean up
fclear()
cat("\n=== Examples completed ===\n")
