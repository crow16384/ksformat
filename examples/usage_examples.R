# Example usage of ksformat package

# This script demonstrates how to use the ksformat package
# Run this after installing the package with: devtools::install()

library(ksformat)

# ============================================================================
# Example 1: Basic discrete value formatting
# ============================================================================

cat("\n=== Example 1: Basic Discrete Formatting ===\n")

# Create a format for gender codes
sex_fmt <- format_create(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  .other = "Other Gender",
  name = "sex"
)

# Apply format to data
gender_codes <- c("M", "F", "M", NA, "X", "F")
formatted_genders <- format_apply(gender_codes, sex_fmt)

print(data.frame(
  code = gender_codes,
  label = formatted_genders
))

# ============================================================================
# Example 2: Numeric range formatting with interval notation
# ============================================================================

cat("\n=== Example 2: Numeric Range Formatting ===\n")

# Define formats in SAS-like text with interval notation
fmt_text <- '
VALUE age (numeric)
  [0, 18)     = "Child"
  [18, 65)    = "Adult"
  [65, HIGH]  = "Senior"
  .missing    = "Age Unknown"
;
'

formats <- format_parse(text = fmt_text)
age_fmt <- formats$age

# Apply to age data (decimals work too)
ages <- c(5, 15.3, 17.9, 18, 45, 64.99, 65, 85, NA)
age_groups <- format_apply(ages, age_fmt)

print(data.frame(
  age = ages,
  group = age_groups
))

# ============================================================================
# Example 3: Decimal ranges (e.g. BMI categories)
# ============================================================================

cat("\n=== Example 3: Decimal Ranges ===\n")

bmi_text <- '
VALUE bmi (numeric)
  [0, 18.5)    = "Underweight"
  [18.5, 25)   = "Normal"
  [25, 30)     = "Overweight"
  [30, HIGH]   = "Obese"
  .missing     = "No data"
;
'

bmi_fmt <- format_parse(text = bmi_text)$bmi
bmi_values <- c(16.2, 18.5, 22.7, 25, 29.9, 35.1, NA)
bmi_labels <- format_apply(bmi_values, bmi_fmt)

print(data.frame(
  bmi = bmi_values,
  category = bmi_labels
))

# ============================================================================
# Example 4: Exclusive/inclusive bounds
# ============================================================================

cat("\n=== Example 4: Bound Inclusivity ===\n")

score_text <- '
VALUE score (numeric)
  (0, 50]    = "Low"
  (50, 100]  = "High"
  .other     = "Out of range"
;
'

score_fmt <- format_parse(text = score_text)$score
scores <- c(0, 1, 50, 51, 100, 101)
score_labels <- format_apply(scores, score_fmt)

print(data.frame(
  score = scores,
  label = score_labels
))

# ============================================================================
# Example 5: Reverse formatting with invalue
# ============================================================================

cat("\n=== Example 5: Reverse Formatting (Invalue) ===\n")

# Create invalue to convert labels back to codes
sex_inv <- format_invalue(
  "Male" = "M",
  "Female" = "F",
  "Unknown" = NA
)

# Apply invalue
labels <- c("Male", "Female", "Male", "Unknown", "Female")
codes <- invalue_apply(labels, sex_inv)

print(data.frame(
  label = labels,
  code = codes
))

# ============================================================================
# Example 6: Bidirectional formatting
# ============================================================================

cat("\n=== Example 6: Bidirectional Formatting ===\n")

# Create both format and invalue at once
status_bi <- format_bidirectional(
  "A" = "Active",
  "I" = "Inactive",
  "P" = "Pending",
  name = "status"
)

# Test forward formatting
status_codes <- c("A", "I", "P", "A")
status_labels <- format_apply(status_codes, status_bi$format)

cat("\nForward (code -> label):\n")
print(data.frame(code = status_codes, label = status_labels))

# Test reverse formatting
test_labels <- c("Active", "Pending", "Inactive")
test_codes <- invalue_apply(test_labels, status_bi$invalue)

cat("\nReverse (label -> code):\n")
print(data.frame(label = test_labels, code = test_codes))

# ============================================================================
# Example 7: Parse multiple formats from a file-like text
# ============================================================================

cat("\n=== Example 7: Multi-format Parsing ===\n")

catalog <- '
// Study format definitions

VALUE sex (character)
  "M" = "Male"
  "F" = "Female"
  .missing = "Unknown"
;

VALUE age (numeric)
  [0, 18)    = "Child"
  [18, 65)   = "Adult"
  [65, HIGH] = "Senior"
;

INVALUE sex_inv (character)
  "Male"   = "M"
  "Female" = "F"
;
'

all_fmts <- format_parse(text = catalog, register = TRUE)

cat("Parsed formats:", paste(names(all_fmts), collapse = ", "), "\n")
cat("Registered in library:", paste(format_list(), collapse = ", "), "\n")

# ============================================================================
# Example 8: Export formats back to text
# ============================================================================

cat("\n=== Example 8: Export Formats to Text ===\n")

# Export a format created programmatically
cat(format_export(bmi = bmi_fmt))
cat("\n")

# ============================================================================
# Example 9: Format library usage
# ============================================================================

cat("\n=== Example 9: Format Library ===\n")

cat("Registered formats:", paste(format_list(), collapse = ", "), "\n")

# Retrieve and use a format from library
retrieved_fmt <- format_get("sex")
cat("Using retrieved format:\n")
print(format_apply(c("M", "F", NA), retrieved_fmt))

# ============================================================================
# Example 10: Data frame formatting
# ============================================================================

cat("\n=== Example 10: Data Frame Formatting ===\n")

# Create sample data
df <- data.frame(
  id = 1:6,
  sex = c("M", "F", "M", "F", NA, "X"),
  age = c(15, 25, 45, 70, 35, NA),
  status = c("A", "I", "A", "P", "A", "I")
)

cat("Original data:\n")
print(df)

# Format multiple columns at once
df_formatted <- format_apply_df(
  df,
  sex = all_fmts$sex,
  age = all_fmts$age,
  status = status_bi$format,
  suffix = "_label"
)

cat("\nFormatted data:\n")
print(df_formatted)

# ============================================================================
# Example 11: Missing value handling
# ============================================================================

cat("\n=== Example 11: Missing Value Handling ===\n")

cat("With .missing label:\n")
print(format_apply(c("M", "F", NA), all_fmts$sex))

cat("\nWith keep_na = TRUE:\n")
print(format_apply(c("M", "F", NA), all_fmts$sex, keep_na = TRUE))

cat("\nis_missing_value():\n")
cat("  NA:            ", is_missing_value(NA), "\n")
cat("  NaN:           ", is_missing_value(NaN), "\n")
cat("  '':            ", is_missing_value(""), "\n")
cat("  '' (w/ empty): ", is_missing_value("", include_empty = TRUE), "\n")

# ============================================================================
# Example 12: Format validation
# ============================================================================

cat("\n=== Example 12: Format Validation ===\n")

validation_result <- format_validate(c("M", "F", "X", NA, "M"), all_fmts$sex)
cat("Validation results:\n")
print(validation_result)

# Range validation
range_validation <- format_validate(c(5, 18, 50, 100, NA), all_fmts$age)
cat("\nRange validation:\n")
print(range_validation)

# Clean up
format_clear()
cat("\n=== Examples completed ===\n")
