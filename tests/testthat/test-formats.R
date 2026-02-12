# Test suite for ksformat package

context("Format Creation and Application")

test_that("format_create works with discrete values", {
  sex_fmt <- format_create(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown",
    name = "sex"
  )

  expect_s3_class(sex_fmt, "ks_format")
  expect_equal(sex_fmt$name, "sex")
  expect_equal(sex_fmt$type, "character")
  expect_equal(length(sex_fmt$mappings), 2)
})

test_that("format_apply handles missing values correctly", {
  sex_fmt <- format_create(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown"
  )

  result <- format_apply(c("M", "F", NA, "X"), sex_fmt)

  expect_equal(result[1], "Male")
  expect_equal(result[2], "Female")
  expect_equal(result[3], "Unknown")
  expect_equal(result[4], "X")
})

test_that("format_apply preserves NA when keep_na = TRUE", {
  sex_fmt <- format_create(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown"
  )

  result <- format_apply(c("M", NA), sex_fmt, keep_na = TRUE)

  expect_equal(result[1], "Male")
  expect_true(is.na(result[2]))
})

test_that("invalue_apply reverses formatting", {
  sex_inv <- format_invalue(
    "Male" = "M",
    "Female" = "F",
    "Unknown" = NA
  )

  result <- invalue_apply(c("Male", "Female", "Unknown"), sex_inv)

  expect_equal(result[1], "M")
  expect_equal(result[2], "F")
  expect_true(is.na(result[3]))
})

test_that("is_missing_value detects various missing types", {
  expect_true(is_missing_value(NA))
  expect_true(is_missing_value(NaN))
  expect_false(is_missing_value(""))
  expect_true(is_missing_value("", include_empty = TRUE))
  expect_false(is_missing_value("text"))
})

test_that("format_register and format_get work", {
  sex_fmt <- format_create(
    "M" = "Male",
    "F" = "Female",
    name = "test_sex"
  )

  format_register(sex_fmt)

  expect_true("test_sex" %in% format_list())

  retrieved <- format_get("test_sex")
  expect_equal(retrieved$name, "test_sex")

  format_remove("test_sex")
  expect_false("test_sex" %in% format_list())
})

test_that("format_bidirectional creates both format and invalue", {
  bi <- format_bidirectional(
    "M" = "Male",
    "F" = "Female",
    name = "sex_bi"
  )

  expect_s3_class(bi$format, "ks_format")
  expect_s3_class(bi$invalue, "ks_invalue")

  # Test forward
  formatted <- format_apply("M", bi$format)
  expect_equal(formatted, "Male")

  # Test reverse
  original <- invalue_apply("Male", bi$invalue)
  expect_equal(original, "M")
})

# ===================================================================
# SAS-like text parsing and export
# ===================================================================

context("Format Parsing (SAS-like syntax)")

test_that("format_parse parses a basic VALUE block", {
  txt <- '
VALUE sex (character)
  "M" = "Male"
  "F" = "Female"
  .missing = "Unknown"
  .other = "N/A"
;
'
  result <- format_parse(text = txt)

  expect_length(result, 1)
  expect_true("sex" %in% names(result))

  fmt <- result$sex
  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$name, "sex")
  expect_equal(fmt$type, "character")
  expect_equal(fmt$mappings[["M"]], "Male")
  expect_equal(fmt$mappings[["F"]], "Female")
  expect_equal(fmt$missing_label, "Unknown")
  expect_equal(fmt$other_label, "N/A")
})

test_that("format_parse handles numeric ranges", {
  txt <- '
VALUE age (numeric)
  0 - 18 = "Child"
  18 - 65 = "Adult"
  65 - HIGH = "Senior"
  .missing = "Age Unknown"
;
'
  result <- format_parse(text = txt)
  fmt <- result$age

  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "numeric")
  expect_equal(fmt$missing_label, "Age Unknown")
  expect_equal(length(fmt$mappings), 3)
  # Range keys are stored as "low,high,inc_low,inc_high"
  # (legacy syntax defaults to [low, high))
  expect_true("0,18,TRUE,FALSE" %in% names(fmt$mappings))
  expect_true("65,Inf,TRUE,FALSE" %in% names(fmt$mappings))
})

test_that("format_parse handles LOW keyword", {
  txt <- '
VALUE score (numeric)
  LOW - 0 = "Negative"
  0 - 100 = "Normal"
;
'
  result <- format_parse(text = txt)
  fmt <- result$score

  expect_true("-Inf,0,TRUE,FALSE" %in% names(fmt$mappings))
  expect_equal(fmt$mappings[["-Inf,0,TRUE,FALSE"]], "Negative")
})

test_that("format_parse parses INVALUE block", {
  txt <- '
INVALUE sex_inv (character)
  "Male" = "M"
  "Female" = "F"
;
'
  result <- format_parse(text = txt)
  inv <- result$sex_inv

  expect_s3_class(inv, "ks_invalue")
  expect_equal(inv$name, "sex_inv")
  expect_equal(inv$mappings[["Male"]], "M")
  expect_equal(inv$mappings[["Female"]], "F")
})

test_that("format_parse handles multiple blocks", {
  txt <- '
VALUE sex (character)
  "M" = "Male"
  "F" = "Female"
;

INVALUE sex_inv (character)
  "Male" = "M"
  "Female" = "F"
;
'
  result <- format_parse(text = txt)

  expect_length(result, 2)
  expect_s3_class(result$sex, "ks_format")
  expect_s3_class(result$sex_inv, "ks_invalue")
})

test_that("format_parse skips comments", {
  txt <- '
// This is a comment
/* Block comment */
VALUE status (character)
  # Another comment
  "A" = "Active"
  * SAS-style comment
  "I" = "Inactive"
;
'
  result <- format_parse(text = txt)
  fmt <- result$status

  expect_equal(length(fmt$mappings), 2)
  expect_equal(fmt$mappings[["A"]], "Active")
})

test_that("format_parse auto-detects type from ranges", {
  txt <- '
VALUE age
  0 - 18 = "Child"
  18 - 65 = "Adult"
;
'
  result <- format_parse(text = txt)
  expect_equal(result$age$type, "numeric")
})

test_that("format_parse with register = TRUE stores formats", {
  txt <- '
VALUE test_parse_reg (character)
  "Y" = "Yes"
  "N" = "No"
;
'
  format_parse(text = txt, register = TRUE)

  expect_true("test_parse_reg" %in% format_list())

  retrieved <- format_get("test_parse_reg")
  expect_equal(retrieved$mappings[["Y"]], "Yes")

  format_remove("test_parse_reg")
})

test_that("format_parse reads from file", {
  tmp <- tempfile(fileext = ".fmt")
  writeLines(c(
    'VALUE color (character)',
    '  "R" = "Red"',
    '  "G" = "Green"',
    ';'
  ), tmp)

  result <- format_parse(file = tmp)
  expect_s3_class(result$color, "ks_format")
  expect_equal(result$color$mappings[["R"]], "Red")

  unlink(tmp)
})

test_that("format_parse errors with no input", {
  expect_error(format_parse(), "Either 'text' or 'file' must be provided")
})

test_that("format_parse errors with both inputs", {
  expect_error(
    format_parse(text = "x", file = "y"),
    "Only one of 'text' or 'file'"
  )
})

test_that("format_parse handles unquoted values", {
  txt <- '
VALUE yn (character)
  Y = Yes
  N = No
;
'
  result <- format_parse(text = txt)
  fmt <- result$yn

  expect_equal(fmt$mappings[["Y"]], "Yes")
  expect_equal(fmt$mappings[["N"]], "No")
})

context("Format Export (SAS-like syntax)")

test_that("format_export produces valid SAS-like text for VALUE", {
  sex_fmt <- format_create(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown",
    .other = "N/A",
    name = "sex"
  )

  txt <- format_export(sex = sex_fmt)

  expect_true(grepl("VALUE sex", txt))
  expect_true(grepl('"M" = "Male"', txt))
  expect_true(grepl('"F" = "Female"', txt))
  expect_true(grepl('.missing = "Unknown"', txt))
  expect_true(grepl('.other = "N/A"', txt))
  expect_true(grepl(";", txt))
})

test_that("format_export produces valid SAS-like text for INVALUE", {
  sex_inv <- format_invalue(
    "Male" = "M",
    "Female" = "F",
    name = "sex_inv"
  )

  txt <- format_export(sex_inv = sex_inv)

  expect_true(grepl("INVALUE sex_inv", txt))
  expect_true(grepl('"Male" = "M"', txt))
})

test_that("format_export roundtrips with format_parse", {
  original_fmt <- format_create(
    "A" = "Active",
    "I" = "Inactive",
    .missing = "Unknown",
    name = "status"
  )

  txt <- format_export(status = original_fmt)
  parsed <- format_parse(text = txt)

  expect_equal(parsed$status$mappings[["A"]], "Active")
  expect_equal(parsed$status$mappings[["I"]], "Inactive")
  expect_equal(parsed$status$missing_label, "Unknown")
})

test_that("format_export writes to file", {
  sex_fmt <- format_create("M" = "Male", "F" = "Female", name = "sex")
  tmp <- tempfile(fileext = ".fmt")

  result <- format_export(sex = sex_fmt, file = tmp)

  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("VALUE sex", content)))

  unlink(tmp)
})

test_that("format_export handles multiple formats", {
  fmt1 <- format_create("M" = "Male", "F" = "Female", name = "sex")
  fmt2 <- format_create("Y" = "Yes", "N" = "No", name = "yn")

  txt <- format_export(sex = fmt1, yn = fmt2)

  expect_true(grepl("VALUE sex", txt))
  expect_true(grepl("VALUE yn", txt))
})

# ===================================================================
# Range bounds and decimal support
# ===================================================================

context("Range Bounds and Decimal Support")

test_that("format_parse handles interval notation [low, high)", {
  txt <- '
VALUE age (numeric)
  [0, 18) = "Child"
  [18, 65) = "Adult"
  [65, HIGH] = "Senior"
;
'
  result <- format_parse(text = txt)
  fmt <- result$age

  expect_equal(fmt$type, "numeric")
  expect_equal(length(fmt$mappings), 3)
  # [0, 18) -> inc_low=TRUE, inc_high=FALSE
  expect_true("0,18,TRUE,FALSE" %in% names(fmt$mappings))
  # [65, Inf] -> inc_low=TRUE, inc_high=TRUE
  expect_true("65,Inf,TRUE,TRUE" %in% names(fmt$mappings))
})

test_that("format_parse handles exclusive lower bound (low, high]", {
  txt <- '
VALUE score (numeric)
  (0, 50] = "Low"
  (50, 100] = "High"
;
'
  result <- format_parse(text = txt)
  fmt <- result$score

  expect_true("0,50,FALSE,TRUE" %in% names(fmt$mappings))
  expect_true("50,100,FALSE,TRUE" %in% names(fmt$mappings))
})

test_that("format_parse handles fully open and fully closed intervals", {
  txt <- '
VALUE test (numeric)
  (0, 10) = "Open"
  [10, 20] = "Closed"
;
'
  result <- format_parse(text = txt)
  fmt <- result$test

  expect_true("0,10,FALSE,FALSE" %in% names(fmt$mappings))
  expect_true("10,20,TRUE,TRUE" %in% names(fmt$mappings))
})

test_that("format_parse handles decimal numbers in ranges", {
  txt <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, 30) = "Overweight"
  [30, HIGH] = "Obese"
;
'
  result <- format_parse(text = txt)
  fmt <- result$bmi

  expect_equal(length(fmt$mappings), 4)
  expect_true("0,18.5,TRUE,FALSE" %in% names(fmt$mappings))
  expect_true("18.5,25,TRUE,FALSE" %in% names(fmt$mappings))
  expect_equal(fmt$mappings[["18.5,25,TRUE,FALSE"]], "Normal")
})

test_that("format_apply matches ranges correctly with [low, high)", {
  txt <- '
VALUE age (numeric)
  [0, 18) = "Child"
  [18, 65) = "Adult"
  [65, HIGH] = "Senior"
;
'
  result <- format_parse(text = txt)
  fmt <- result$age

  applied <- format_apply(c(0, 5, 17.9, 18, 64.99, 65, 100), fmt)

  expect_equal(applied[1], "Child")    # 0 is in [0, 18)
  expect_equal(applied[2], "Child")    # 5 is in [0, 18)
  expect_equal(applied[3], "Child")    # 17.9 is in [0, 18)
  expect_equal(applied[4], "Adult")    # 18 is in [18, 65)
  expect_equal(applied[5], "Adult")    # 64.99 is in [18, 65)
  expect_equal(applied[6], "Senior")   # 65 is in [65, HIGH]
  expect_equal(applied[7], "Senior")   # 100 is in [65, HIGH]
})

test_that("format_apply handles exclusive lower bound correctly", {
  txt <- '
VALUE score (numeric)
  (0, 50] = "Low"
  (50, 100] = "High"
;
'
  result <- format_parse(text = txt)
  fmt <- result$score

  applied <- format_apply(c(0, 1, 50, 51, 100), fmt)

  expect_equal(applied[1], "0")       # 0 is NOT in (0, 50] — returned as-is
  expect_equal(applied[2], "Low")     # 1 is in (0, 50]
  expect_equal(applied[3], "Low")     # 50 is in (0, 50]
  expect_equal(applied[4], "High")    # 51 is in (50, 100]
  expect_equal(applied[5], "High")    # 100 is in (50, 100]
})

test_that("format_apply handles decimal ranges", {
  txt <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, 30) = "Overweight"
;
'
  result <- format_parse(text = txt)
  fmt <- result$bmi

  applied <- format_apply(c(15.2, 18.5, 24.9, 25, 29.99), fmt)

  expect_equal(applied[1], "Underweight")
  expect_equal(applied[2], "Normal")
  expect_equal(applied[3], "Normal")
  expect_equal(applied[4], "Overweight")
  expect_equal(applied[5], "Overweight")
})

test_that("format_apply handles missing + ranges together", {
  txt <- '
VALUE temp (numeric)
  [LOW, 0) = "Freezing"
  [0, 20) = "Cold"
  [20, HIGH] = "Warm"
  .missing = "No data"
;
'
  result <- format_parse(text = txt)
  fmt <- result$temp

  applied <- format_apply(c(-10, 0, 15, 25, NA), fmt)

  expect_equal(applied[1], "Freezing")
  expect_equal(applied[2], "Cold")
  expect_equal(applied[3], "Cold")
  expect_equal(applied[4], "Warm")
  expect_equal(applied[5], "No data")
})

test_that("format_export roundtrips interval notation", {
  txt_in <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, HIGH] = "Overweight"
;
'
  parsed <- format_parse(text = txt_in)
  exported <- format_export(bmi = parsed$bmi)

  # Should contain interval notation
  expect_true(grepl("\\[0, 18\\.5\\)", exported))
  expect_true(grepl("\\[18\\.5, 25\\)", exported))
  expect_true(grepl("\\[25, HIGH\\]", exported))

  # Re-parse and verify roundtrip
  reparsed <- format_parse(text = exported)
  expect_equal(reparsed$bmi$mappings[["18.5,25,TRUE,FALSE"]], "Normal")
})

test_that("range_spec supports inc_low and inc_high parameters", {
  rs1 <- range_spec(0, 10, "test1")
  expect_true(rs1$inc_low)
  expect_false(rs1$inc_high)

  rs2 <- range_spec(0, 10, "test2", inc_low = FALSE, inc_high = TRUE)
  expect_false(rs2$inc_low)
  expect_true(rs2$inc_high)
})

test_that("in_range respects bound inclusivity", {
  rs_half_open <- range_spec(0, 10, "test")                          # [0, 10)
  rs_closed <- range_spec(0, 10, "test", inc_high = TRUE)            # [0, 10]
  rs_open <- range_spec(0, 10, "test", inc_low = FALSE)              # (0, 10)

  # [0, 10): includes 0, excludes 10
  expect_true(in_range(0, rs_half_open))
  expect_true(in_range(5, rs_half_open))
  expect_false(in_range(10, rs_half_open))

  # [0, 10]: includes both
  expect_true(in_range(0, rs_closed))
  expect_true(in_range(10, rs_closed))

  # (0, 10): excludes both bounds
  expect_false(in_range(0, rs_open))
  expect_true(in_range(5, rs_open))
  expect_false(in_range(10, rs_open))
})

test_that("format_validate works with range formats", {
  txt <- '
VALUE temp (numeric)
  [0, 100) = "Normal"
  .other = "Abnormal"
;
'
  result <- format_parse(text = txt)
  fmt <- result$temp

  validation <- format_validate(c(50, 150, NA), fmt)

  expect_true(validation$matched[1])    # 50 in [0, 100)
  expect_false(validation$matched[2])   # 150 not in range
  expect_false(validation$matched[3])   # NA
})
