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
  # Range keys are stored as "low,high"
  expect_true("0,18" %in% names(fmt$mappings))
  expect_true("65,Inf" %in% names(fmt$mappings))
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

  expect_true("-Inf,0" %in% names(fmt$mappings))
  expect_equal(fmt$mappings[["-Inf,0"]], "Negative")
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
