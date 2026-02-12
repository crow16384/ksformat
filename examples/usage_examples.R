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
# Example 2: Numeric range formatting
# ============================================================================

cat("\n=== Example 2: Numeric Range Formatting ===\n")

# Create age group format
age_fmt <- format_create(
  c(0, 18) = "Child",
  c(18, 65) = "Adult",
  c(65, Inf) = "Senior",
  .missing = "Age Unknown",
  name = "age"
)

# Apply to age data
ages <- c(5, 15, 25, 45, 70, 85, NA)
age_groups <- format_apply(ages, age_fmt)

print(data.frame(
  age = ages,
  group = age_groups
))

# ============================================================================
# Example 3: Reverse formatting with invalue
# ============================================================================

cat("\n=== Example 3: Reverse Formatting (Invalue) ===\n")

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
# Example 4: Bidirectional formatting
# ============================================================================

cat("\n=== Example 4: Bidirectional Formatting ===\n")

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
# Example 5: Format library usage
# ============================================================================

cat("\n=== Example 5: Format Library ===\n")

# Register formats for reuse
format_register(sex_fmt)
format_register(age_fmt)

cat("\nRegistered formats:\n")
print(format_list())

# Retrieve and use a format
retrieved_fmt <- format_get("sex")
cat("\nUsing retrieved format:\n")
print(format_apply(c("M", "F"), retrieved_fmt))

# ============================================================================
# Example 6: Data frame formatting
# ============================================================================

cat("\n=== Example 6: Data Frame Formatting ===\n")

# Create sample data
df <- data.frame(
  id = 1:6,
  sex = c("M", "F", "M", "F", NA, "X"),
  age = c(15, 25, 45, 70, 35, NA),
  status = c("A", "I", "A", "P", "A", "I")
)

cat("\nOriginal data:\n")
print(df)

# Format multiple columns at once
df_formatted <- format_apply_df(
  df,
  sex = sex_fmt,
  age = age_fmt,
  status = status_bi$format,
  suffix = "_label"
)

cat("\nFormatted data:\n")
print(df_formatted)

# ============================================================================
# Example 7: Missing value handling
# ============================================================================

cat("\n=== Example 7: Missing Value Handling ===\n")

# Test various missing value types
test_values <- c("M", "F", NA, NULL)
cat("\nWith .missing label:\n")
print(format_apply(test_values, sex_fmt))

cat("\nWith keep_na = TRUE:\n")
print(format_apply(c("M", "F", NA), sex_fmt, keep_na = TRUE))

# Test is_missing_value utility
cat("\nTesting is_missing_value():\n")
cat("is_missing_value(NA):", is_missing_value(NA), "\n")
cat("is_missing_value(''):", is_missing_value(""), "\n")
cat("is_missing_value('', include_empty = TRUE):", 
    is_missing_value("", include_empty = TRUE), "\n")

# ============================================================================
# Example 8: Format validation
# ============================================================================

cat("\n=== Example 8: Format Validation ===\n")

# Validate data against format
validation_data <- c("M", "F", "X", NA, "M")
validation_result <- format_validate(validation_data, sex_fmt)

cat("\nValidation results:\n")
print(validation_result)

# Clean up
format_clear()
cat("\n=== Examples completed ===\n")
