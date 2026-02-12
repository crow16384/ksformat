# ksformat: SAS-Style PROC FORMAT for R

Provides SAS PROC FORMAT-like functionality for creating and applying value formats in R. The package supports mapping values to labels, range-based formatting, reverse formatting (invalue), and proper handling of missing values (NA, NULL, NaN).

## Installation

```r
# From local directory
install.packages(".", repos = NULL, type = "source")

# Or using devtools
devtools::install()
```

## Features

- **Format Creation**: Create value-to-label mappings like SAS PROC FORMAT
- **Format Application**: Apply formats to vectors and data frames
- **Reverse Formatting**: Convert labels back to original values (INVALUE)
- **Missing Value Handling**: Comprehensive support for NA, NULL, NaN, and empty values
- **Range Support**: Define ranges for numeric values
- **Format Library**: Store and retrieve formats globally

## Quick Start

### Basic Discrete Formatting

```r
library(ksformat)

# Create a format for gender codes
sex_fmt <- format_create(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown",
  name = "sex"
)

# Apply the format
format_apply(c("M", "F", NA, "X"), sex_fmt)
# [1] "Male"    "Female"  "Unknown" "X"
```

### Numeric Range Formatting

```r
# Create age group format
age_fmt <- format_create(
  c(0, 18) = "Child",
  c(18, 65) = "Adult",
  c(65, Inf) = "Senior",
  .missing = "Age Unknown",
  name = "age"
)

format_apply(c(5, 25, 70, NA), age_fmt)
# [1] "Child"       "Adult"       "Senior"      "Age Unknown"
```

### Reverse Formatting (Invalue)

```r
# Create invalue for converting labels back to codes
sex_inv <- format_invalue(
  "Male" = "M",
  "Female" = "F",
  "Unknown" = NA
)

invalue_apply(c("Male", "Female", "Unknown"), sex_inv)
# [1] "M" "F" NA
```

### Format Library

```r
# Register formats for later use
format_register(sex_fmt)
format_register(age_fmt)

# List available formats
format_list()
# [1] "sex" "age"

# Retrieve a format
fmt <- format_get("sex")

# Remove a format
format_remove("sex")

# Clear all formats
format_clear()
```

### Working with Data Frames

```r
df <- data.frame(
  sex = c("M", "F", "M", NA),
  age = c(15, 25, 70, 35)
)

# Apply formats to multiple columns
format_apply_df(df, sex = sex_fmt, age = age_fmt)
#   sex age sex_fmt    age_fmt
# 1   M  15    Male      Child
# 2   F  25  Female      Adult
# 3   M  70    Male     Senior
# 4  NA  35 Unknown      Adult
```

## Missing Value Handling

The package handles missing values in priority order:

1. **NA, NULL, NaN** → Uses `.missing` label if defined, otherwise NA
2. **Exact matches** → Uses defined value-label mapping
3. **Range matches** → Uses range label (for numeric formats)
4. **No match** → Uses `.other` label or returns original value

Special options:
- `keep_na = TRUE` preserves NA instead of labeling
- `na_if` parameter treats additional values as missing
- `include_empty = TRUE` treats empty strings as missing

## Functions

### Format Creation
- `format_create()` - Create a format definition
- `format_invalue()` - Create a reverse format (invalue)
- `format_bidirectional()` - Create both format and invalue

### Format Application
- `format_apply()` - Apply format to a vector
- `format_apply_df()` - Apply formats to data frame columns
- `invalue_apply()` - Apply invalue (reverse formatting)

### Format Library
- `format_register()` - Store format in library
- `format_get()` - Retrieve format from library
- `format_list()` - List all stored formats
- `format_remove()` - Remove format from library
- `format_clear()` - Clear all formats

### Utilities
- `is_missing_value()` - Check for missing values
- `range_spec()` - Create range specification
- `format_validate()` - Validate values against format

## Development

```r
# Install dependencies
install.packages(c("roxygen2", "testthat", "devtools"))

# Generate documentation
devtools::document()

# Run tests
devtools::test()

# Check package
devtools::check()
```

## License

MIT License - see [LICENSE](LICENSE) file for details.
