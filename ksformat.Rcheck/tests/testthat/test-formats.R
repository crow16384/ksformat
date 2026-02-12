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
