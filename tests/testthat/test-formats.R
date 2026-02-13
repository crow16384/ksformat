# Test suite for ksformat package

context("Format Creation (fnew)")

test_that("fnew works with discrete values", {
  sex_fmt <- fnew(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown",
    name = "sex"
  )

  expect_s3_class(sex_fmt, "ks_format")
  expect_equal(sex_fmt$name, "sex")
  expect_equal(sex_fmt$type, "character")
  expect_equal(length(sex_fmt$mappings), 2)

  fclear()
})

test_that("fnew auto-registers named formats", {
  fnew("M" = "Male", "F" = "Female", name = "test_auto")

  expect_true("test_auto" %in% ls(envir = ksformat:::.format_library))

  fclear()
})

test_that("fnew without name does not register", {
  fmt <- fnew("Y" = "Yes", "N" = "No")
  expect_null(fmt$name)
  # Should not be in library (no name to store under)
  fclear()
})

context("Format Application (fput)")

test_that("fput handles missing values correctly", {
  sex_fmt <- fnew(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown"
  )

  result <- fput(c("M", "F", NA, "X"), sex_fmt)

  expect_equal(result[1], "Male")
  expect_equal(result[2], "Female")
  expect_equal(result[3], "Unknown")
  expect_equal(result[4], "X")
})

test_that("fput preserves NA when keep_na = TRUE", {
  sex_fmt <- fnew(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown"
  )

  result <- fput(c("M", NA), sex_fmt, keep_na = TRUE)

  expect_equal(result[1], "Male")
  expect_true(is.na(result[2]))
})

test_that("fput accepts format name from library", {
  fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")

  result <- fput(c("M", "F", NA), "sex")

  expect_equal(result[1], "Male")
  expect_equal(result[2], "Female")
  expect_equal(result[3], "Unknown")

  fclear()
})

context("SAS-like Functions (fputn, fputc, finputn, finputc)")

test_that("fputn applies numeric format by name", {
  fparse(text = '
VALUE age (numeric)
  [0, 18) = "Child"
  [18, 65) = "Adult"
  [65, HIGH] = "Senior"
;
')

  result <- fputn(c(5, 25, 70), "age")
  expect_equal(result, c("Child", "Adult", "Senior"))

  fclear()
})

test_that("fputc applies character format by name", {
  fnew("M" = "Male", "F" = "Female", name = "sex")

  result <- fputc(c("M", "F"), "sex")
  expect_equal(result, c("Male", "Female"))

  fclear()
})

test_that("fputn warns for non-numeric format", {
  fnew("M" = "Male", name = "char_fmt")

  expect_warning(fputn("M", "char_fmt"), "not 'numeric'")

  fclear()
})

test_that("fputc warns for non-character format", {
  fparse(text = '
VALUE nums (numeric)
  [0, 10) = "Low"
;
')

  expect_warning(fputc("5", "nums"), "not 'character'")

  fclear()
})

test_that("finputn applies numeric invalue by name", {
  finput("Male" = 1, "Female" = 2, name = "sex_inv")

  result <- finputn(c("Male", "Female"), "sex_inv")
  expect_equal(result, c(1, 2))

  fclear()
})

test_that("finputc applies character invalue by name", {
  finput("Male" = "M", "Female" = "F",
         name = "sex_inv", target_type = "character")

  result <- finputc(c("Male", "Female"), "sex_inv")
  expect_equal(result, c("M", "F"))

  fclear()
})

context("Invalue Creation and Application (finput)")

test_that("invalue_apply reverses formatting", {
  sex_inv <- finput(
    "Male" = 1,
    "Female" = 2,
    name = "sex_inv"
  )

  result <- invalue_apply(c("Male", "Female", "Unknown"), sex_inv)

  expect_equal(result[1], 1)
  expect_equal(result[2], 2)
  expect_true(is.na(result[3]))

  fclear()
})

test_that("invalue_apply accepts name from library", {
  finput("Male" = 1, "Female" = 2, name = "sex_inv")

  result <- invalue_apply(c("Male", "Female"), "sex_inv")
  expect_equal(result, c(1, 2))

  fclear()
})

test_that("finput defaults to numeric target_type", {
  inv <- finput("X" = 10, "Y" = 20)
  expect_equal(inv$target_type, "numeric")
})

test_that("INVALUE is numeric by default (no type specified)", {
  txt <- '
INVALUE score_inv
  "Low" = 1
  "Medium" = 5
  "High" = 10
;
'
  result <- fparse(text = txt)
  inv <- result$score_inv

  expect_s3_class(inv, "ks_invalue")
  expect_equal(inv$target_type, "numeric")

  # Apply it
  applied <- invalue_apply(c("Low", "High"), inv)
  expect_equal(applied, c(1, 10))

  fclear()
})

context("Utility Functions")

test_that("is_missing detects various missing types", {
  expect_true(is_missing(NA))
  expect_true(is_missing(NaN))
  expect_false(is_missing(""))
  expect_true(is_missing("", include_empty = TRUE))
  expect_false(is_missing("text"))
})

test_that("is_missing handles NULL", {
  expect_equal(is_missing(NULL), logical(0))
})

context("Format Library (fprint, fclear)")

test_that("fprint and fclear work", {
  fnew("M" = "Male", "F" = "Female", name = "test_sex")

  expect_true("test_sex" %in% ls(envir = ksformat:::.format_library))

  # fprint should produce output without errors
  expect_output(fprint("test_sex"), "KS Format")

  # fclear specific format
  fclear("test_sex")
  expect_false("test_sex" %in% ls(envir = ksformat:::.format_library))
})

test_that("fclear removes all formats", {
  fnew("A" = "Alpha", name = "fmt1")
  fnew("B" = "Beta", name = "fmt2")

  fclear()
  expect_equal(length(ls(envir = ksformat:::.format_library)), 0)
})

test_that("fprint shows format list", {
  fnew("X" = "Ex", name = "test_print")

  expect_output(fprint(), "Registered formats:")
  expect_output(fprint(), "test_print")

  fclear()
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
  formatted <- fput("M", bi$format)
  expect_equal(formatted, "Male")

  # Test reverse
  original <- invalue_apply("Male", bi$invalue)
  expect_equal(original, "M")

  fclear()
})

# ===================================================================
# SAS-like text parsing and export
# ===================================================================

context("Format Parsing (fparse)")

test_that("fparse parses a basic VALUE block", {
  txt <- '
VALUE sex (character)
  "M" = "Male"
  "F" = "Female"
  .missing = "Unknown"
  .other = "N/A"
;
'
  result <- fparse(text = txt)

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

  fclear()
})

test_that("fparse handles numeric ranges", {
  txt <- '
VALUE age (numeric)
  0 - 18 = "Child"
  18 - 65 = "Adult"
  65 - HIGH = "Senior"
  .missing = "Age Unknown"
;
'
  result <- fparse(text = txt)
  fmt <- result$age

  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "numeric")
  expect_equal(fmt$missing_label, "Age Unknown")
  expect_equal(length(fmt$mappings), 3)
  expect_true("0,18,TRUE,FALSE" %in% names(fmt$mappings))
  expect_true("65,Inf,TRUE,FALSE" %in% names(fmt$mappings))

  fclear()
})

test_that("fparse handles LOW keyword", {
  txt <- '
VALUE score (numeric)
  LOW - 0 = "Negative"
  0 - 100 = "Normal"
;
'
  result <- fparse(text = txt)
  fmt <- result$score

  expect_true("-Inf,0,TRUE,FALSE" %in% names(fmt$mappings))
  expect_equal(fmt$mappings[["-Inf,0,TRUE,FALSE"]], "Negative")

  fclear()
})

test_that("fparse parses INVALUE block (numeric by default)", {
  txt <- '
INVALUE score_inv
  "Low" = 1
  "Medium" = 5
  "High" = 10
;
'
  result <- fparse(text = txt)
  inv <- result$score_inv

  expect_s3_class(inv, "ks_invalue")
  expect_equal(inv$target_type, "numeric")

  fclear()
})

test_that("fparse handles INVALUE with explicit character type", {
  txt <- '
INVALUE sex_inv (character)
  "Male" = "M"
  "Female" = "F"
;
'
  result <- fparse(text = txt)
  inv <- result$sex_inv

  expect_s3_class(inv, "ks_invalue")
  expect_equal(inv$target_type, "character")
  expect_equal(inv$mappings[["Male"]], "M")
  expect_equal(inv$mappings[["Female"]], "F")

  fclear()
})

test_that("fparse auto-registers all formats", {
  txt <- '
VALUE sex (character)
  "M" = "Male"
  "F" = "Female"
;

INVALUE score_inv
  "Low" = 1
  "High" = 10
;
'
  fparse(text = txt)

  expect_true("sex" %in% ls(envir = ksformat:::.format_library))
  expect_true("score_inv" %in% ls(envir = ksformat:::.format_library))

  fclear()
})

test_that("fparse handles multiple blocks", {
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
  result <- fparse(text = txt)

  expect_length(result, 2)
  expect_s3_class(result$sex, "ks_format")
  expect_s3_class(result$sex_inv, "ks_invalue")

  fclear()
})

test_that("fparse skips comments", {
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
  result <- fparse(text = txt)
  fmt <- result$status

  expect_equal(length(fmt$mappings), 2)
  expect_equal(fmt$mappings[["A"]], "Active")

  fclear()
})

test_that("fparse auto-detects type from ranges", {
  txt <- '
VALUE age
  0 - 18 = "Child"
  18 - 65 = "Adult"
;
'
  result <- fparse(text = txt)
  expect_equal(result$age$type, "numeric")

  fclear()
})

test_that("fparse reads from file", {
  tmp <- tempfile(fileext = ".fmt")
  writeLines(c(
    'VALUE color (character)',
    '  "R" = "Red"',
    '  "G" = "Green"',
    ';'
  ), tmp)

  result <- fparse(file = tmp)
  expect_s3_class(result$color, "ks_format")
  expect_equal(result$color$mappings[["R"]], "Red")

  unlink(tmp)
  fclear()
})

test_that("fparse errors with no input", {
  expect_error(fparse(), "Either 'text' or 'file' must be provided")
})

test_that("fparse errors with both inputs", {
  expect_error(
    fparse(text = "x", file = "y"),
    "Only one of 'text' or 'file'"
  )
})

test_that("fparse handles unquoted values", {
  txt <- '
VALUE yn (character)
  Y = Yes
  N = No
;
'
  result <- fparse(text = txt)
  fmt <- result$yn

  expect_equal(fmt$mappings[["Y"]], "Yes")
  expect_equal(fmt$mappings[["N"]], "No")

  fclear()
})

context("Format Export (SAS-like syntax)")

test_that("format_export produces valid SAS-like text for VALUE", {
  sex_fmt <- fnew(
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

  fclear()
})

test_that("format_export produces valid SAS-like text for INVALUE", {
  sex_inv <- finput(
    "Male" = 1,
    "Female" = 2,
    name = "sex_inv"
  )

  txt <- format_export(sex_inv = sex_inv)

  expect_true(grepl("INVALUE sex_inv", txt))
  expect_true(grepl('"Male" = "1"', txt))

  fclear()
})

test_that("format_export roundtrips with fparse", {
  original_fmt <- fnew(
    "A" = "Active",
    "I" = "Inactive",
    .missing = "Unknown",
    name = "status"
  )

  txt <- format_export(status = original_fmt)
  fclear()
  parsed <- fparse(text = txt)

  expect_equal(parsed$status$mappings[["A"]], "Active")
  expect_equal(parsed$status$mappings[["I"]], "Inactive")
  expect_equal(parsed$status$missing_label, "Unknown")

  fclear()
})

test_that("format_export writes to file", {
  sex_fmt <- fnew("M" = "Male", "F" = "Female", name = "sex")
  tmp <- tempfile(fileext = ".fmt")

  result <- format_export(sex = sex_fmt, file = tmp)

  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("VALUE sex", content)))

  unlink(tmp)
  fclear()
})

test_that("format_export handles multiple formats", {
  fmt1 <- fnew("M" = "Male", "F" = "Female", name = "sex")
  fmt2 <- fnew("Y" = "Yes", "N" = "No", name = "yn")

  txt <- format_export(sex = fmt1, yn = fmt2)

  expect_true(grepl("VALUE sex", txt))
  expect_true(grepl("VALUE yn", txt))

  fclear()
})

# ===================================================================
# Range bounds and decimal support
# ===================================================================

context("Range Bounds and Decimal Support")

test_that("fparse handles interval notation [low, high)", {
  txt <- '
VALUE age (numeric)
  [0, 18) = "Child"
  [18, 65) = "Adult"
  [65, HIGH] = "Senior"
;
'
  result <- fparse(text = txt)
  fmt <- result$age

  expect_equal(fmt$type, "numeric")
  expect_equal(length(fmt$mappings), 3)
  expect_true("0,18,TRUE,FALSE" %in% names(fmt$mappings))
  expect_true("65,Inf,TRUE,TRUE" %in% names(fmt$mappings))

  fclear()
})

test_that("fparse handles exclusive lower bound (low, high]", {
  txt <- '
VALUE score (numeric)
  (0, 50] = "Low"
  (50, 100] = "High"
;
'
  result <- fparse(text = txt)
  fmt <- result$score

  expect_true("0,50,FALSE,TRUE" %in% names(fmt$mappings))
  expect_true("50,100,FALSE,TRUE" %in% names(fmt$mappings))

  fclear()
})

test_that("fparse handles fully open and fully closed intervals", {
  txt <- '
VALUE test (numeric)
  (0, 10) = "Open"
  [10, 20] = "Closed"
;
'
  result <- fparse(text = txt)
  fmt <- result$test

  expect_true("0,10,FALSE,FALSE" %in% names(fmt$mappings))
  expect_true("10,20,TRUE,TRUE" %in% names(fmt$mappings))

  fclear()
})

test_that("fparse handles decimal numbers in ranges", {
  txt <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, 30) = "Overweight"
  [30, HIGH] = "Obese"
;
'
  result <- fparse(text = txt)
  fmt <- result$bmi

  expect_equal(length(fmt$mappings), 4)
  expect_true("0,18.5,TRUE,FALSE" %in% names(fmt$mappings))
  expect_true("18.5,25,TRUE,FALSE" %in% names(fmt$mappings))
  expect_equal(fmt$mappings[["18.5,25,TRUE,FALSE"]], "Normal")

  fclear()
})

test_that("fput matches ranges correctly with [low, high)", {
  txt <- '
VALUE age (numeric)
  [0, 18) = "Child"
  [18, 65) = "Adult"
  [65, HIGH] = "Senior"
;
'
  result <- fparse(text = txt)
  fmt <- result$age

  applied <- fput(c(0, 5, 17.9, 18, 64.99, 65, 100), fmt)

  expect_equal(applied[1], "Child")
  expect_equal(applied[2], "Child")
  expect_equal(applied[3], "Child")
  expect_equal(applied[4], "Adult")
  expect_equal(applied[5], "Adult")
  expect_equal(applied[6], "Senior")
  expect_equal(applied[7], "Senior")

  fclear()
})

test_that("fput handles exclusive lower bound correctly", {
  txt <- '
VALUE score (numeric)
  (0, 50] = "Low"
  (50, 100] = "High"
;
'
  result <- fparse(text = txt)
  fmt <- result$score

  applied <- fput(c(0, 1, 50, 51, 100), fmt)

  expect_equal(applied[1], "0")
  expect_equal(applied[2], "Low")
  expect_equal(applied[3], "Low")
  expect_equal(applied[4], "High")
  expect_equal(applied[5], "High")

  fclear()
})

test_that("fput handles decimal ranges", {
  txt <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, 30) = "Overweight"
;
'
  result <- fparse(text = txt)
  fmt <- result$bmi

  applied <- fput(c(15.2, 18.5, 24.9, 25, 29.99), fmt)

  expect_equal(applied[1], "Underweight")
  expect_equal(applied[2], "Normal")
  expect_equal(applied[3], "Normal")
  expect_equal(applied[4], "Overweight")
  expect_equal(applied[5], "Overweight")

  fclear()
})

test_that("fput handles missing + ranges together", {
  txt <- '
VALUE temp (numeric)
  [LOW, 0) = "Freezing"
  [0, 20) = "Cold"
  [20, HIGH] = "Warm"
  .missing = "No data"
;
'
  result <- fparse(text = txt)
  fmt <- result$temp

  applied <- fput(c(-10, 0, 15, 25, NA), fmt)

  expect_equal(applied[1], "Freezing")
  expect_equal(applied[2], "Cold")
  expect_equal(applied[3], "Cold")
  expect_equal(applied[4], "Warm")
  expect_equal(applied[5], "No data")

  fclear()
})

test_that("format_export roundtrips interval notation", {
  txt_in <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, HIGH] = "Overweight"
;
'
  parsed <- fparse(text = txt_in)
  exported <- format_export(bmi = parsed$bmi)

  expect_true(grepl("\\[0, 18\\.5\\)", exported))
  expect_true(grepl("\\[18\\.5, 25\\)", exported))
  expect_true(grepl("\\[25, HIGH\\]", exported))

  # Re-parse and verify roundtrip
  fclear()
  reparsed <- fparse(text = exported)
  expect_equal(reparsed$bmi$mappings[["18.5,25,TRUE,FALSE"]], "Normal")

  fclear()
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
  rs_half_open <- range_spec(0, 10, "test")
  rs_closed <- range_spec(0, 10, "test", inc_high = TRUE)
  rs_open <- range_spec(0, 10, "test", inc_low = FALSE)

  expect_true(in_range(0, rs_half_open))
  expect_true(in_range(5, rs_half_open))
  expect_false(in_range(10, rs_half_open))

  expect_true(in_range(0, rs_closed))
  expect_true(in_range(10, rs_closed))

  expect_false(in_range(0, rs_open))
  expect_true(in_range(5, rs_open))
  expect_false(in_range(10, rs_open))
})

# ===================================================================
# INVALUE numeric default
# ===================================================================

context("INVALUE Numeric Default")

test_that("INVALUE without type spec defaults to numeric", {
  txt <- '
INVALUE age_inv
  "Child" = 10
  "Adult" = 40
  "Senior" = 75
;
'
  result <- fparse(text = txt)
  inv <- result$age_inv

  expect_equal(inv$target_type, "numeric")

  applied <- invalue_apply(c("Child", "Adult", "Senior"), inv)
  expect_equal(applied, c(10, 40, 75))
  expect_true(is.numeric(applied))

  fclear()
})

test_that("finput creates numeric invalue by default", {
  inv <- finput("Low" = 1, "Mid" = 5, "High" = 10)

  expect_equal(inv$target_type, "numeric")

  result <- invalue_apply(c("Low", "High"), inv)
  expect_equal(result, c(1, 10))
  expect_true(is.numeric(result))
})

# ===================================================================
# format_apply_df
# ===================================================================

context("Data Frame Formatting")

test_that("format_apply_df works with fput", {
  sex_fmt <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
  status_fmt <- fnew("A" = "Active", "I" = "Inactive", "P" = "Pending")

  df <- data.frame(
    sex = c("M", "F", "M", NA),
    status = c("A", "I", "A", "P"),
    stringsAsFactors = FALSE
  )

  result <- format_apply_df(df, sex = sex_fmt, status = status_fmt)

  expect_true("sex_fmt" %in% names(result))
  expect_true("status_fmt" %in% names(result))
  expect_equal(result$sex_fmt, c("Male", "Female", "Male", "Unknown"))
})

# ===================================================================
# INVALUE export without (numeric) type
# ===================================================================

context("INVALUE Export")

test_that("INVALUE export omits numeric type (default)", {
  inv <- finput("X" = 1, "Y" = 2, name = "test_inv")
  txt <- format_export(test_inv = inv)

  # Should say "INVALUE test_inv" without "(numeric)"
  expect_true(grepl("INVALUE test_inv$", txt, perl = TRUE) ||
              grepl("INVALUE test_inv\n", txt, fixed = TRUE))
  expect_false(grepl("(numeric)", txt, fixed = TRUE))

  fclear()
})

test_that("INVALUE export includes non-default type", {
  inv <- finput("X" = "a", "Y" = "b",
                name = "test_inv", target_type = "character")
  txt <- format_export(test_inv = inv)

  expect_true(grepl("(character)", txt, fixed = TRUE))

  fclear()
})
