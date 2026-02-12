pkgname <- "ksformat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ksformat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("format_apply")
### * format_apply

flush(stderr()); flush(stdout())

### Name: format_apply
### Title: Apply Format to Data (like SAS FORMAT statement)
### Aliases: format_apply

### ** Examples

sex_fmt <- format_create("M" = "Male", "F" = "Female", .missing = "Unknown")
format_apply(c("M", "F", NA, "X"), sex_fmt)
# Returns: "Male" "Female" "Unknown" "X"



cleanEx()
nameEx("format_apply_df")
### * format_apply_df

flush(stderr()); flush(stdout())

### Name: format_apply_df
### Title: Apply Format to Data Frame Columns
### Aliases: format_apply_df

### ** Examples

df <- data.frame(
  sex = c("M", "F", "M", NA),
  status = c("A", "I", "A", "P")
)

sex_fmt <- format_create("M" = "Male", "F" = "Female", .missing = "Unknown")
status_fmt <- format_create("A" = "Active", "I" = "Inactive", "P" = "Pending")

format_apply_df(df, sex = sex_fmt, status = status_fmt)



cleanEx()
nameEx("format_bidirectional")
### * format_bidirectional

flush(stderr()); flush(stdout())

### Name: format_bidirectional
### Title: Create Bidirectional Format
### Aliases: format_bidirectional

### ** Examples

sex_bi <- format_bidirectional(
  "M" = "Male",
  "F" = "Female",
  name = "sex"
)

# Format: M -> Male
format_apply("M", sex_bi$format)

# Invalue: Male -> M
invalue_apply("Male", sex_bi$invalue)



cleanEx()
nameEx("format_create")
### * format_create

flush(stderr()); flush(stdout())

### Name: format_create
### Title: Create a Format Definition (like SAS PROC FORMAT)
### Aliases: format_create

### ** Examples

# Discrete value format
sex_fmt <- format_create(
  "M" = "Male",
  "F" = "Female",
  .missing = "Unknown"
)

# For range-based formatting, use string keys:
## Not run: 
##D age_fmt <- format_create(
##D   "0-18" = "Child",
##D   "18-65" = "Adult",
##D   "65+" = "Senior",
##D   .missing = "Age Unknown"
##D )
## End(Not run)



cleanEx()
nameEx("format_get")
### * format_get

flush(stderr()); flush(stdout())

### Name: format_get
### Title: Retrieve Format from Library
### Aliases: format_get

### ** Examples

## Not run: 
##D sex_fmt <- format_get("sex")
## End(Not run)



cleanEx()
nameEx("format_invalue")
### * format_invalue

flush(stderr()); flush(stdout())

### Name: format_invalue
### Title: Create Invalue Format (Reverse Formatting like SAS INVALUE)
### Aliases: format_invalue

### ** Examples

# Convert text labels back to codes
sex_inv <- format_invalue(
  "Male" = "M",
  "Female" = "F",
  "Unknown" = NA
)

# Convert age groups to numeric midpoints
age_inv <- format_invalue(
  "Child" = 10,
  "Adult" = 40,
  "Senior" = 75,
  target_type = "numeric"
)



cleanEx()
nameEx("format_list")
### * format_list

flush(stderr()); flush(stdout())

### Name: format_list
### Title: List Available Formats in Library
### Aliases: format_list

### ** Examples

format_list()



cleanEx()
nameEx("format_register")
### * format_register

flush(stderr()); flush(stdout())

### Name: format_register
### Title: Register Format in Library
### Aliases: format_register

### ** Examples

sex_fmt <- format_create("M" = "Male", "F" = "Female", name = "sex")
format_register(sex_fmt)

# Later retrieve it
fmt <- format_get("sex")



cleanEx()
nameEx("format_remove")
### * format_remove

flush(stderr()); flush(stdout())

### Name: format_remove
### Title: Remove Format from Library
### Aliases: format_remove

### ** Examples

## Not run: 
##D format_remove("sex")
## End(Not run)



cleanEx()
nameEx("format_validate")
### * format_validate

flush(stderr()); flush(stdout())

### Name: format_validate
### Title: Validate Vector Against Format
### Aliases: format_validate

### ** Examples

sex_fmt <- format_create("M" = "Male", "F" = "Female")
format_validate(c("M", "F", "X", NA), sex_fmt)



cleanEx()
nameEx("invalue_apply")
### * invalue_apply

flush(stderr()); flush(stdout())

### Name: invalue_apply
### Title: Apply Invalue Format (Reverse Formatting)
### Aliases: invalue_apply

### ** Examples

sex_inv <- format_invalue("Male" = "M", "Female" = "F", "Unknown" = NA)
invalue_apply(c("Male", "Female", "Unknown", NA), sex_inv)
# Returns: "M" "F" NA NA



cleanEx()
nameEx("is_missing_value")
### * is_missing_value

flush(stderr()); flush(stdout())

### Name: is_missing_value
### Title: Check if Value is Missing
### Aliases: is_missing_value

### ** Examples

is_missing_value(NA)          # TRUE
is_missing_value(NULL)        # TRUE (length 0)
is_missing_value(NaN)         # TRUE
is_missing_value("")          # FALSE
is_missing_value("", TRUE)    # TRUE



cleanEx()
nameEx("ksformat-package")
### * ksformat-package

flush(stderr()); flush(stdout())

### Name: ksformat-package
### Title: ksformat: SAS-Style PROC FORMAT for R
### Aliases: ksformat-package ksformat
### Keywords: package

### ** Examples

## Not run: 
##D # Create and apply a format
##D sex_fmt <- format_create(
##D   "M" = "Male",
##D   "F" = "Female",
##D   .missing = "Unknown"
##D )
##D format_apply(c("M", "F", NA), sex_fmt)
## End(Not run)




cleanEx()
nameEx("range_spec")
### * range_spec

flush(stderr()); flush(stdout())

### Name: range_spec
### Title: Create Range Specification
### Aliases: range_spec

### ** Examples

range_spec(0, 18, "Child")
range_spec(18, 65, "Adult")
range_spec(65, Inf, "Senior")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
