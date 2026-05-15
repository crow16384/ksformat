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

  expect_warning(fputn("M", "char_fmt"), "not.*numeric")

  fclear()
})

test_that("fputc warns for non-character format", {
  fparse(text = '
VALUE nums (numeric)
  [0, 10) = "Low"
;
')

  expect_warning(fputc("5", "nums"), "not.*character")

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

test_that(".find_cheatsheet_path returns a path for known formats", {
  html_result <- ksformat:::.find_cheatsheet_path("html")
  expect_type(html_result, "character")

  pdf_result <- ksformat:::.find_cheatsheet_path("pdf")
  expect_type(pdf_result, "character")
})


# ===========================================================================
# Named vector support
# ===========================================================================

context("Named Vector Support")

test_that("fnew works with named character vector", {
  fmt <- fnew(c(Male = "M", Female = "F"), name = "sex_vec")

  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "character")
  expect_equal(length(fmt$mappings), 2)

  result <- fput(c("M", "F"), fmt)
  expect_equal(result, c("Male", "Female"))

  fclear()
})

test_that("fnew named vector with .missing and .other directives", {
  fmt <- fnew(c(Male = "M", Female = "F", .missing = "Unknown", .other = "Other"))

  result <- fput(c("M", "F", NA, "X"), fmt)
  expect_equal(result, c("Male", "Female", "Unknown", "Other"))
})

test_that("fnew mixed: named vector + scalar ... args", {
  fmt <- fnew(c(Male = "M", Female = "F"), .missing = "Unknown")

  result <- fput(c("M", NA), fmt)
  expect_equal(result, c("Male", "Unknown"))
})

test_that("fnew with named list", {
  fmt <- fnew(list(Male = "M", Female = "F"))

  result <- fput(c("M", "F"), fmt)
  expect_equal(result, c("Male", "Female"))
})

test_that("fnew with multiple named vectors", {
  fmt <- fnew(c(Male = "M", Female = "F"), c(Child = "C", Adult = "A"))

  result <- fput(c("M", "F", "C", "A"), fmt)
  expect_equal(result, c("Male", "Female", "Child", "Adult"))
})

test_that("fnew errors on unnamed vector without names", {
  expect_error(fnew(c("M", "F")), "fully named")
})

test_that("fmap creates ks_fmap class with correct names/values", {
  result <- fmap(c("A", "B"), c("Label-A", "Label-B"))
  expect_s3_class(result, "ks_fmap")
  expect_equal(names(result), c("A", "B"))
  expect_equal(unclass(result), c(A = "Label-A", B = "Label-B"))
})

test_that("fmap errors on length mismatch", {
  expect_error(fmap(c("A", "B"), c("X")), "same length")
})

test_that("fmap suppresses reversal for character type in fnew", {
  # Without fmap: c(Male = "M") means "M" -> "Male" (auto-reversed)
  fmt_default <- fnew(c(Male = "M", Female = "F"))
  expect_equal(fput("M", fmt_default), "Male")

  # With fmap: keys are input keys, values are output labels (no reversal)
  fmt_fmap <- fnew(fmap(c("M", "F"), c("Male", "Female")))
  expect_equal(fput("M", fmt_fmap), "Male")
  expect_equal(fput("F", fmt_fmap), "Female")
})

test_that("fmap enables consistent pattern for all types in fnew", {
  keys <- c("A", "B")
  vals_chr <- c("Label-A", "Label-B")
  vals_date <- as.Date(c("2021-01-01", "2021-06-15"))

  # Same fmap(keys, values) pattern for both types
  fmt_chr <- fnew(fmap(keys, vals_chr), type = "character")
  fmt_date <- fnew(fmap(keys, vals_date), type = "Date")

  expect_equal(fput("A", fmt_chr), "Label-A")
  expect_equal(fput("B", fmt_chr), "Label-B")
  expect_equal(fput("A", fmt_date), vals_date[1])
  expect_equal(fput("B", fmt_date), vals_date[2])
})

test_that("fnew default auto-reversal still works without fmap", {
  # character reverses
  fmt_chr <- fnew(c(Male = "M"))
  expect_equal(fput("M", fmt_chr), "Male")

  # Date does not reverse
  fmt_date <- fnew(c("M" = as.Date("2021-01-01")))
  expect_equal(fput("M", fmt_date), as.Date("2021-01-01"))
})

test_that("finput works with named numeric vector", {
  inv <- finput(c(Male = 1, Female = 2), name = "sex_inv_vec")

  result <- finputn(c("Male", "Female"), "sex_inv_vec")
  expect_equal(result, c(1, 2))

  fclear()
})

test_that("finput works with named character vector", {
  inv <- finput(c(Male = "M", Female = "F"),
                name = "sex_inv_chr", target_type = "character")

  result <- finputc(c("Male", "Female"), "sex_inv_chr")
  expect_equal(result, c("M", "F"))

  fclear()
})

test_that("fnew_bid works with named vector", {
  bi <- fnew_bid(c(Male = "M", Female = "F"), name = "sex_bid_vec")

  expect_s3_class(bi$format, "ks_format")
  expect_s3_class(bi$invalue, "ks_invalue")

  fwd <- fputc(c("M", "F"), "sex_bid_vec")
  expect_equal(fwd, c("Male", "Female"))

  rev <- finputc(c("Male", "Female"), "sex_bid_vec_inv")
  expect_equal(rev, c("M", "F"))

  fclear()
})

context("Invalue Creation and Application (finput)")

test_that(".invalue_apply reverses formatting", {
  sex_inv <- finput(
    "Male" = 1,
    "Female" = 2,
    name = "sex_inv"
  )

  result <- ksformat:::.invalue_apply(c("Male", "Female", "Unknown"), sex_inv)

  expect_equal(result[1], 1)
  expect_equal(result[2], 2)
  expect_true(is.na(result[3]))

  fclear()
})

test_that(".invalue_apply accepts name from library", {
  finput("Male" = 1, "Female" = 2, name = "sex_inv")

  result <- ksformat:::.invalue_apply(c("Male", "Female"), "sex_inv")
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
  applied <- ksformat:::.invalue_apply(c("Low", "High"), inv)
  expect_equal(applied, c(1, 10))

  fclear()
})

context("Utility Functions")

test_that("is_missing detects various missing types", {
  expect_true(is_missing(NA))
  expect_true(is_missing(NaN))
  expect_true(is_missing(""))
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

test_that("fnew_bid creates both format and invalue", {
  bi <- fnew_bid(
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
  original <- ksformat:::.invalue_apply("Male", bi$invalue)
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
  expect_error(fparse(), "Either .text. or .file. must be provided")
})

test_that("fparse handles hyphenated format names", {
  fclear()
  txt <- '
VALUE 014-001
  "M" = "Male",
  "F" = "Female",
  .missing = "No sex";
'
  result <- fparse(text = txt)
  expect_length(result, 1)
  expect_s3_class(result[["014-001"]], "ks_format")
  expect_equal(result[["014-001"]]$mappings[["M"]], "Male")

  # Verify auto-registration
  out <- fput(c("M", "F", ""), "014-001")
  expect_equal(out, c("Male", "Female", "No sex"))

  fclear()
})

test_that("fparse errors with both inputs", {
  expect_error(
    fparse(text = "x", file = "y"),
    "Only one of .text. or .file."
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

test_that("fexport produces valid SAS-like text for VALUE", {
  sex_fmt <- fnew(
    "M" = "Male",
    "F" = "Female",
    .missing = "Unknown",
    .other = "N/A",
    name = "sex"
  )

  txt <- fexport(sex = sex_fmt)

  expect_true(grepl("VALUE sex", txt))
  expect_true(grepl('"M" = "Male"', txt))
  expect_true(grepl('"F" = "Female"', txt))
  expect_true(grepl('.missing = "Unknown"', txt))
  expect_true(grepl('.other = "N/A"', txt))
  expect_true(grepl(";", txt))

  fclear()
})

test_that("fexport produces valid SAS-like text for INVALUE", {
  sex_inv <- finput(
    "Male" = 1,
    "Female" = 2,
    name = "sex_inv"
  )

  txt <- fexport(sex_inv = sex_inv)

  expect_true(grepl("INVALUE sex_inv", txt))
  expect_true(grepl('"Male" = "1"', txt))

  fclear()
})

test_that("fexport roundtrips with fparse", {
  original_fmt <- fnew(
    "A" = "Active",
    "I" = "Inactive",
    .missing = "Unknown",
    name = "status"
  )

  txt <- fexport(status = original_fmt)
  fclear()
  parsed <- fparse(text = txt)

  expect_equal(parsed$status$mappings[["A"]], "Active")
  expect_equal(parsed$status$mappings[["I"]], "Inactive")
  expect_equal(parsed$status$missing_label, "Unknown")

  fclear()
})

test_that("fexport writes to file", {
  sex_fmt <- fnew("M" = "Male", "F" = "Female", name = "sex")
  tmp <- tempfile(fileext = ".fmt")

  result <- fexport(sex = sex_fmt, file = tmp)

  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("VALUE sex", content)))

  unlink(tmp)
  fclear()
})

test_that("fexport handles multiple formats", {
  fmt1 <- fnew("M" = "Male", "F" = "Female", name = "sex")
  fmt2 <- fnew("Y" = "Yes", "N" = "No", name = "yn")

  txt <- fexport(sex = fmt1, yn = fmt2)

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

test_that("fexport roundtrips interval notation", {
  txt_in <- '
VALUE bmi (numeric)
  [0, 18.5) = "Underweight"
  [18.5, 25) = "Normal"
  [25, HIGH] = "Overweight"
;
'
  parsed <- fparse(text = txt_in)
  exported <- fexport(bmi = parsed$bmi)

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
# franges()
# ===================================================================

context("franges")

test_that("franges extracts range entries from a numeric format", {
  fmt <- fparse(text = '
VALUE age (numeric)
  [0, 18)    = "Child"
  [18, 65)   = "Adult"
  [65, HIGH] = "Senior"
  .missing   = "Unknown"
;
')$age

  df <- franges(fmt)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
  expect_equal(names(df), c("low", "high", "inc_low", "inc_high", "label"))
  expect_equal(df$low, c(0, 18, 65))
  expect_equal(df$high, c(18, 65, Inf))
  expect_equal(df$inc_low, c(TRUE, TRUE, TRUE))
  expect_equal(df$inc_high, c(FALSE, FALSE, TRUE))
  expect_equal(df$label, c("Child", "Adult", "Senior"))

  fclear()
})

test_that("franges accepts a registered format name", {
  fparse(text = '
VALUE bmi (numeric)
  [0, 25)    = "Normal"
  [25, HIGH] = "Overweight"
;
')

  df <- franges("bmi")
  expect_equal(nrow(df), 2L)
  expect_equal(df$label, c("Normal", "Overweight"))

  fclear()
})

test_that("franges returns empty data frame for discrete-only format", {
  fmt <- fnew("M" = "Male", "F" = "Female", name = "sex")
  df <- franges(fmt)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_equal(names(df), c("low", "high", "inc_low", "inc_high", "label"))

  fclear()
})

test_that("franges errors on ks_invalue object", {
  inv <- fparse(text = '
INVALUE yn
  "Yes" = 1
  "No"  = 0
;
')$yn

  expect_error(franges(inv), "ks_format")

  fclear()
})

test_that("fmap_to_ranges returns bounds for matching labels", {
  fparse(text = '
VALUE visit_ther (numeric)
  [LOW,  1] =  0
  [ 8, 22] =  2
  [22, 36] =  4
  [37, 50] =  6
;
')

  res <- fmap_to_ranges(c(0, 2, 4, 6), "visit_ther")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4L)
  expect_equal(res$low, c(-Inf, 8, 22, 37))
  expect_equal(res$high, c(1, 22, 36, 50))
  expect_true(all(res$inc_low) && all(res$inc_high))

  fclear()
})

test_that("fmap_to_ranges returns NA for unmatched labels", {
  fparse(text = '
VALUE v (numeric)
  [0, 10) = 1
  [10, 20) = 2
;
')

  res <- fmap_to_ranges(c(1, 99), "v")
  expect_equal(res$low, c(0, NA))
  expect_equal(res$high, c(10, NA))

  fclear()
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

  applied <- ksformat:::.invalue_apply(c("Child", "Adult", "Senior"), inv)
  expect_equal(applied, c(10, 40, 75))
  expect_true(is.numeric(applied))

  fclear()
})

test_that("finput creates numeric invalue by default", {
  inv <- finput("Low" = 1, "Mid" = 5, "High" = 10)

  expect_equal(inv$target_type, "numeric")

  result <- ksformat:::.invalue_apply(c("Low", "High"), inv)
  expect_equal(result, c(1, 10))
  expect_true(is.numeric(result))
})

# ===================================================================
# fput_df
# ===================================================================

context("Data Frame Formatting")

test_that("fput_df works with fput", {
  sex_fmt <- fnew("M" = "Male", "F" = "Female", .missing = "Unknown")
  status_fmt <- fnew("A" = "Active", "I" = "Inactive", "P" = "Pending")

  df <- data.frame(
    sex = c("M", "F", "M", NA),
    status = c("A", "I", "A", "P"),
    stringsAsFactors = FALSE
  )

  result <- fput_df(df, sex = sex_fmt, status = status_fmt)

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
  txt <- fexport(test_inv = inv)

  # Should say "INVALUE test_inv" without "(numeric)"
  expect_true(grepl("INVALUE test_inv$", txt, perl = TRUE) ||
              grepl("INVALUE test_inv\n", txt, fixed = TRUE))
  expect_false(grepl("(numeric)", txt, fixed = TRUE))

  fclear()
})

test_that("INVALUE export includes non-default type", {
  inv <- finput("X" = "a", "Y" = "b",
                name = "test_inv", target_type = "character")
  txt <- fexport(test_inv = inv)

  expect_true(grepl("(character)", txt, fixed = TRUE))

  fclear()
})


# ===========================================================================
# Date/Time Format Tests
# ===========================================================================

test_that("fnew_date creates date format from SAS name", {
  fmt <- fnew_date("DATE9.", name = "mydate")
  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "date")
  expect_equal(fmt$dt_pattern, "%d%b%Y")
  expect_true(fmt$dt_toupper)
  expect_equal(fmt$sas_name, "DATE9")
  fclear()
})

test_that("fnew_date creates time format", {
  fmt <- fnew_date("TIME8.", name = "mytime")
  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "time")
  expect_equal(fmt$dt_pattern, "%_H:%M:%S")  # TIME has no leading zero
  fclear()
})

test_that("fnew_date creates TOD format with leading zero", {
  fmt <- fnew_date("TOD8.", name = "mytod")
  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "time")
  expect_equal(fmt$dt_pattern, "%H:%M:%S")  # TOD has leading zero
  fclear()
})

test_that("fnew_date creates datetime format", {
  fmt <- fnew_date("DATETIME20.", name = "mydt")
  expect_s3_class(fmt, "ks_format")
  expect_equal(fmt$type, "datetime")
  expect_equal(fmt$dt_pattern, "%d%b%Y:%H:%M:%S")
  expect_true(fmt$dt_toupper)
  fclear()
})

test_that("fnew_date with custom strftime pattern", {
  fmt <- fnew_date("%Y/%m/%d", name = "custom", type = "date")
  expect_equal(fmt$type, "date")
  expect_equal(fmt$dt_pattern, "%Y/%m/%d")
  expect_false(fmt$dt_toupper)
  expect_null(fmt$sas_name)
  fclear()
})

test_that("fnew_date auto-registers in library", {
  fnew_date("DATE9.", name = "testdate")
  expect_true("testdate" %in% ls(envir = ksformat:::.format_library))
  fclear()
})

test_that("fnew_date requires type for custom patterns", {
  expect_error(fnew_date("%Y-%m-%d"), "type")
  fclear()
})

test_that("fnew_date handles default widths", {
  fmt <- fnew_date("DATE.", name = "d1")
  expect_equal(fmt$dt_pattern, "%d%b%Y")  # DATE. defaults to DATE9.
  fmt2 <- fnew_date("TIME.", name = "t1")
  expect_equal(fmt2$dt_pattern, "%_H:%M:%S")  # TIME. defaults to TIME8. (no leading zero)
  fclear()
})

test_that("fnew_date uses R epoch (1970-01-01)", {
  fmt <- fnew_date("DATE9.", name = "dorigin")
  # dt_origin is no longer stored; epoch is always R's 1970-01-01
  expect_null(fmt$dt_origin)
  fclear()
})

test_that("fnew_date does not accept origin parameter", {
  expect_error(fnew_date("DATE9.", name = "dsas", origin = "1960-01-01"))
  fclear()
})

test_that("fput with Date objects and date format", {
  fmt <- fnew_date("DATE9.", name = "datefmt")
  result <- fput(as.Date("2020-01-15"), fmt)
  expect_equal(result, "15JAN2020")
  fclear()
})

test_that("fput with numeric R-epoch dates", {
  fmt <- fnew_date("DATE9.", name = "datefmt")
  # R epoch: days since 1970-01-01
  r_date <- as.numeric(as.Date("2020-01-01"))
  result <- fput(r_date, fmt)
  expect_equal(result, "01JAN2020")
  fclear()
})

test_that("fput with numeric R-epoch dates (default)", {
  fmt <- fnew_date("DATE9.", name = "datefmt")
  # R epoch: days since 1970-01-01
  r_date <- as.numeric(as.Date("2020-01-01"))
  result <- fput(r_date, fmt)
  expect_equal(result, "01JAN2020")
  fclear()
})

test_that("fput with MMDDYY10 format", {
  fmt <- fnew_date("MMDDYY10.", name = "usfmt")
  result <- fput(as.Date("2020-06-15"), fmt)
  expect_equal(result, "06/15/2020")
  fclear()
})

test_that("fput with YYMMDD10 format", {
  fmt <- fnew_date("YYMMDD10.", name = "isofmt")
  result <- fput(as.Date("2020-06-15"), fmt)
  expect_equal(result, "2020-06-15")
  fclear()
})

test_that("fput with DDMMYY10 format", {
  fmt <- fnew_date("DDMMYY10.", name = "eufmt")
  result <- fput(as.Date("2020-06-15"), fmt)
  expect_equal(result, "15/06/2020")
  fclear()
})

test_that("fput with MONYY7 format", {
  fmt <- fnew_date("MONYY7.", name = "myfmt")
  result <- fput(as.Date("2020-06-15"), fmt)
  expect_equal(result, "JUN2020")
  fclear()
})

test_that("fput with YEAR4 format", {
  fmt <- fnew_date("YEAR4.", name = "yrfmt")
  result <- fput(as.Date("2020-06-15"), fmt)
  expect_equal(result, "2020")
  fclear()
})

test_that("fput date format handles NA", {
  fmt <- fnew_date("DATE9.", name = "dfmt", .missing = "No Date")
  result <- fput(c(as.Date("2020-01-01"), NA), fmt)
  expect_equal(result[2], "No Date")
  fclear()
})

test_that("fput date format NA with keep_na", {
  fmt <- fnew_date("DATE9.", name = "dfmt", .missing = "No Date")
  result <- fput(c(as.Date("2020-01-01"), NA), fmt, keep_na = TRUE)
  expect_true(is.na(result[2]))
  fclear()
})

test_that("fput date with vector of dates", {
  fmt <- fnew_date("DATE9.", name = "dfmt")
  dates <- as.Date(c("2020-01-01", "2020-06-15", "2020-12-31"))
  result <- fput(dates, fmt)
  expect_equal(result, c("01JAN2020", "15JUN2020", "31DEC2020"))
  fclear()
})

test_that("fput time format with numeric seconds", {
  fmt <- fnew_date("TIME8.", name = "tfmt")
  result <- fput(c(0, 3600, 45000), fmt)
  expect_equal(result, c("0:00:00", "1:00:00", "12:30:00"))  # TIME: no leading zero
  fclear()
})

test_that("fput time format TIME5 (H:MM)", {
  fmt <- fnew_date("TIME5.", name = "t5fmt")
  result <- fput(c(0, 45000), fmt)
  expect_equal(result, c("0:00", "12:30"))  # TIME: no leading zero
  fclear()
})

test_that("fput TOD format with leading zero", {
  fmt <- fnew_date("TOD8.", name = "todfmt")
  result <- fput(c(0, 3600, 45000), fmt)
  expect_equal(result, c("00:00:00", "01:00:00", "12:30:00"))  # TOD: leading zero
  fclear()
})

test_that("fput dynamic TIME widths work", {
  result4 <- fputn(c(0, 3600, 45000), "time4.")
  expect_equal(result4, c("0", "1", "12"))  # width < 5 → hours only
  result6 <- fputn(c(0, 3600, 45000), "time6.")
  expect_equal(result6, c("0:00", "1:00", "12:30"))  # 5 ≤ width < 8 → H:MM
  result9 <- fputn(c(0, 3600, 45000), "time9.")
  expect_equal(result9, c("0:00:00", "1:00:00", "12:30:00"))  # 8 ≤ width < 11
  fclear()
})

test_that("fput datetime format with POSIXct", {
  fmt <- fnew_date("DATETIME20.", name = "dtfmt")
  dt <- as.POSIXct("2020-01-15 12:30:45", tz = "UTC")
  result <- fput(dt, fmt)
  expect_equal(result, "15JAN2020:12:30:45")
  fclear()
})

test_that("fput datetime with R-epoch numeric", {
  fmt <- fnew_date("DATETIME20.", name = "dtfmt")
  # R epoch: seconds since 1970-01-01 00:00:00
  r_dt <- as.numeric(as.POSIXct("2020-01-01 12:00:00", tz = "UTC"))
  result <- fput(r_dt, fmt)
  expect_equal(result, "01JAN2020:12:00:00")
  fclear()
})

test_that("fputn auto-resolves SAS format names", {
  # No need to create format first
  result <- fputn(as.Date("2020-01-01"), "DATE9.")
  expect_equal(result, "01JAN2020")
  fclear()
})

test_that("fputn with R-epoch numeric dates", {
  r_date <- as.numeric(as.Date("2020-06-15"))
  result <- fputn(r_date, "MMDDYY10.")
  expect_equal(result, "06/15/2020")
  fclear()
})

test_that("fputn with R-epoch numeric dates (default)", {
  r_date <- as.numeric(as.Date("2020-06-15"))
  result <- fputn(r_date, "MMDDYY10.")
  expect_equal(result, "06/15/2020")
  fclear()
})

test_that("fputc with date format", {
  result <- fputc("2020-01-01", "DATE9.")
  expect_equal(result, "01JAN2020")
  fclear()
})

test_that("fputn with time format", {
  result <- fputn(45000, "TIME8.")
  expect_equal(result, "12:30:00")
  fclear()
})

test_that("fputn and fputc don't warn for datetime types", {
  expect_silent(fputn(as.Date("2020-01-01"), "DATE9."))
  expect_silent(fputc("2020-01-01", "YYMMDD10."))
  fclear()
})

test_that("fput with NULL returns empty character", {
  fmt <- fnew_date("DATE9.", name = "dfmt")
  expect_equal(fput(NULL, fmt), character(0))
  fclear()
})

test_that("fput QTR format", {
  fmt <- fnew_date("QTR.", name = "qfmt")
  dates <- as.Date(c("2020-01-15", "2020-04-15", "2020-07-15", "2020-10-15"))
  result <- fput(dates, fmt)
  expect_equal(result, c("1", "2", "3", "4"))
  fclear()
})

test_that("print.ks_format for datetime format", {
  fmt <- fnew_date("DATE9.", name = "pfmt")
  out <- capture.output(print(fmt))
  expect_true(any(grepl("date", out)))
  expect_true(any(grepl("Pattern:", out)))
  expect_true(any(grepl("DATE9", out)))
  fclear()
})

test_that("fprint shows datetime format info", {
  fnew_date("DATE9.", name = "printdate")
  out <- capture.output(fprint())
  expect_true(any(grepl("date", out)))
  fclear()
})


# ===========================================================================
# Multilabel Format Tests
# ===========================================================================

test_that("fnew with multilabel flag", {
  fmt <- fnew("1" = "A", "2" = "B", name = "ml_test",
              type = "character", multilabel = TRUE)
  expect_true(fmt$multilabel)
  fclear()
})

test_that("print.ks_format shows multilabel flag", {
  fmt <- fnew("1" = "A", "2" = "B", name = "ml_test",
              type = "character", multilabel = TRUE)
  out <- capture.output(print(fmt))
  expect_true(any(grepl("multilabel", out)))
  fclear()
})

test_that("fput_all returns all matching labels for multilabel ranges", {
  fmt <- fnew(
    "0,5,TRUE,TRUE" = "Infant",
    "6,11,TRUE,TRUE" = "Child",
    "12,17,TRUE,TRUE" = "Teen",
    "0,17,TRUE,TRUE" = "Minor",
    "18,64,TRUE,TRUE" = "Adult",
    "65,Inf,TRUE,TRUE" = "Senior",
    name = "age_ml", type = "numeric", multilabel = TRUE
  )

  result <- fput_all(c(3, 15, 25), fmt)
  expect_type(result, "list")
  expect_length(result, 3)
  expect_true("Infant" %in% result[[1]])
  expect_true("Minor" %in% result[[1]])
  expect_true("Teen" %in% result[[2]])
  expect_true("Minor" %in% result[[2]])
  expect_equal(result[[3]], "Adult")
  fclear()
})

test_that("fput_all handles NA values", {
  fmt <- fnew(
    "0,10,TRUE,TRUE" = "Low",
    "0,100,TRUE,TRUE" = "All",
    .missing = "Unknown",
    name = "ml_na", type = "numeric", multilabel = TRUE
  )

  result <- fput_all(c(5, NA), fmt)
  expect_true("Low" %in% result[[1]])
  expect_true("All" %in% result[[1]])
  expect_equal(result[[2]], "Unknown")
  fclear()
})

test_that("fput_all with keep_na=TRUE", {
  fmt <- fnew(
    "0,10,TRUE,TRUE" = "Low",
    .missing = "Unknown",
    name = "ml_kna", type = "numeric"
  )
  result <- fput_all(c(5, NA), fmt, keep_na = TRUE)
  expect_true(is.na(result[[2]]))
  fclear()
})

test_that("fput_all with NULL returns empty list", {
  fmt <- fnew("1" = "A", name = "ml_null")
  result <- fput_all(NULL, fmt)
  expect_equal(result, list())
  fclear()
})

test_that("fput_all with other_label", {
  fmt <- fnew("1" = "A", .other = "Unknown",
              name = "ml_other", type = "character")
  result <- fput_all(c("1", "X"), fmt)
  expect_equal(result[[1]], "A")
  expect_equal(result[[2]], "Unknown")
  fclear()
})

test_that("fput returns first match for multilabel format", {
  fmt <- fnew(
    "0,5,TRUE,TRUE" = "Infant",
    "0,17,TRUE,TRUE" = "Minor",
    name = "ml_first", type = "numeric", multilabel = TRUE
  )
  result <- fput(3, fmt)
  # fput should return first match only
  expect_length(result, 1)
  expect_true(result %in% c("Infant", "Minor"))
  fclear()
})

test_that("fput_all with format name string", {
  fnew("0,10,TRUE,TRUE" = "Low", "0,100,TRUE,TRUE" = "All",
       name = "ml_name", type = "numeric", multilabel = TRUE)
  result <- fput_all(5, "ml_name")
  expect_true("Low" %in% result[[1]])
  expect_true("All" %in% result[[1]])
  fclear()
})

test_that("fput_all with date format returns list", {
  fnew_date("DATE9.", name = "dt_ml")
  result <- fput_all(as.Date("2020-01-01"), "dt_ml")
  expect_type(result, "list")
  expect_equal(result[[1]], "01JAN2020")
  fclear()
})


# ===========================================================================
# Parser: Date/Time and Multilabel Support
# ===========================================================================

test_that("fparse: date format block", {
  txt <- '
VALUE mydate (date)
  pattern = "DATE9."
;
'
  result <- fparse(text = txt)
  expect_s3_class(result$mydate, "ks_format")
  expect_equal(result$mydate$type, "date")
  expect_equal(result$mydate$dt_pattern, "%d%b%Y")
  fclear()
})

test_that("fparse: time format block", {
  txt <- '
VALUE mytime (time)
  pattern = "TIME8."
;
'
  result <- fparse(text = txt)
  expect_equal(result$mytime$type, "time")
  fclear()
})

test_that("fparse: datetime format block", {
  txt <- '
VALUE mydt (datetime)
  pattern = "DATETIME20."
;
'
  result <- fparse(text = txt)
  expect_equal(result$mydt$type, "datetime")
  fclear()
})

test_that("fparse: date format with .missing", {
  txt <- '
VALUE mydate (date)
  pattern = "DATE9."
  .missing = "No Date"
;
'
  result <- fparse(text = txt)
  expect_equal(result$mydate$missing_label, "No Date")
  fclear()
})

test_that("fparse: date format block without pattern errors", {
  txt <- '
VALUE mydate (date)
  "x" = "y"
;
'
  expect_error(fparse(text = txt), "pattern")
  fclear()
})

test_that("fparse: multilabel block", {
  txt <- '
VALUE ageml (numeric, multilabel)
  [0, 5] = "Infant"
  [0, 17] = "Minor"
  [18, 64] = "Adult"
;
'
  result <- fparse(text = txt)
  expect_true(result$ageml$multilabel)
  expect_equal(result$ageml$type, "numeric")
  fclear()
})

test_that("fparse: multilabel without type", {
  txt <- '
VALUE test (multilabel)
  "A" = "Label A"
  "B" = "Label B"
;
'
  result <- fparse(text = txt)
  expect_true(result$test$multilabel)
  fclear()
})

test_that("fexport: datetime format round-trip", {
  fnew_date("DATE9.", name = "rtdate")
  fmt <- ksformat:::.format_get("rtdate")
  txt <- fexport(rtdate = fmt)
  expect_true(grepl("VALUE rtdate (date)", txt, fixed = TRUE))
  expect_true(grepl("pattern = \"DATE9.\"", txt, fixed = TRUE))
  fclear()
})

test_that("fexport: multilabel format includes flag", {
  fmt <- fnew("1" = "A", "2" = "B", name = "ml_exp",
              type = "character", multilabel = TRUE)
  txt <- fexport(ml_exp = fmt)
  expect_true(grepl("multilabel", txt))
  fclear()
})

test_that("fexport: date format parse round-trip", {
  txt_in <- '
VALUE mydate (date)
  pattern = "DATE9."
;
'
  fparse(text = txt_in)
  fmt <- ksformat:::.format_get("mydate")
  txt_out <- fexport(mydate = fmt)
  expect_true(grepl("pattern", txt_out))
  expect_true(grepl("DATE9", txt_out))

  # Round-trip: parse the exported text
  fclear()
  result <- fparse(text = txt_out)
  expect_equal(result$mydate$type, "date")
  fclear()
})

test_that("custom strftime date format with fput", {
  fmt <- fnew_date("%d.%m.%Y", name = "ru_date", type = "date")
  result <- fput(as.Date("2020-06-15"), fmt)
  expect_equal(result, "15.06.2020")
  fclear()
})


# ===========================================================================
# Case-Insensitive Matching Tests
# ===========================================================================

test_that("ignore_case in fnew creates format with flag", {
  fmt <- fnew("M" = "Male", "F" = "Female", ignore_case = TRUE)
  expect_true(fmt$ignore_case)
  fclear()
})

test_that("fput with ignore_case matches case-insensitively", {
  fmt <- fnew("M" = "Male", "F" = "Female", ignore_case = TRUE)
  result <- fput(c("m", "f", "M", "F"), fmt)
  expect_equal(result, c("Male", "Female", "Male", "Female"))
  fclear()
})

test_that("fput without ignore_case is case-sensitive (default)", {
  fmt <- fnew("M" = "Male", "F" = "Female")
  result <- fput(c("m", "M"), fmt)
  expect_equal(result, c("m", "Male"))
  fclear()
})

test_that("fput_all with ignore_case", {
  fmt <- fnew("a" = "Alpha", "b" = "Beta",
              type = "character", ignore_case = TRUE, multilabel = TRUE)
  result <- fput_all(c("A", "b"), fmt)
  expect_equal(result[[1]], "Alpha")
  expect_equal(result[[2]], "Beta")
  fclear()
})

test_that("fparse with nocase keyword", {
  txt <- '
VALUE sex (character, nocase)
  "M" = "Male"
  "F" = "Female"
;
'
  result <- fparse(text = txt)
  expect_true(result$sex$ignore_case)

  # Test case-insensitive apply
  expect_equal(fput(c("m", "F"), "sex"), c("Male", "Female"))
  fclear()
})

test_that("fexport with nocase flag", {
  fmt <- fnew("M" = "Male", "F" = "Female",
              name = "sex_nc", type = "character", ignore_case = TRUE)
  txt <- fexport(sex_nc = fmt)
  expect_true(grepl("nocase", txt))
  fclear()
})

test_that("print.ks_format shows nocase flag", {
  fmt <- fnew("M" = "Male", name = "nc_test", ignore_case = TRUE)
  out <- capture.output(print(fmt))
  expect_true(any(grepl("nocase", out)))
  fclear()
})

test_that("ignore_case combined with .other and .missing", {
  fmt <- fnew("Y" = "Yes", "N" = "No",
              .missing = "Unknown", .other = "Invalid",
              ignore_case = TRUE)
  result <- fput(c("y", "N", NA, "x"), fmt)
  expect_equal(result, c("Yes", "No", "Unknown", "Invalid"))
  fclear()
})


# ===========================================================================
# Expression Label Tests
# ===========================================================================

test_that("expression label detected by .is_expr_label", {
  expect_true(ksformat:::.is_expr_label("sprintf('%s', .x1)"))
  expect_true(ksformat:::.is_expr_label("paste0(.x1, '%')"))
  expect_false(ksformat:::.is_expr_label("Male"))
  expect_false(ksformat:::.is_expr_label("x1"))  # no dot
  fclear()
})

test_that("fput with expression label and positional arg", {
  fmt <- fnew("n" = "sprintf('%s', .x1)",
              name = "stat", type = "character")
  result <- fput(c("n", "n"), fmt, c(42, 100))
  expect_equal(result, c("42", "100"))
  fclear()
})

test_that("fput with multiple expression labels", {
  fmt <- fnew(
    "n" = "sprintf('%s', .x1)",
    "pct" = "sprintf('%.1f%%', .x1 * 100)",
    name = "stat2", type = "character"
  )
  result <- fput(c("n", "pct", "n", "pct"), fmt, c(42, 0.15, 100, 0.255))
  expect_equal(result, c("42", "15.0%", "100", "25.5%"))
  fclear()
})

test_that("fput with mixed static and expression labels", {
  fmt <- fnew(
    "label" = "LABEL",
    "n" = "sprintf('%s', .x1)",
    name = "mixed", type = "character"
  )
  result <- fput(c("label", "n", "label"), fmt, c(0, 42, 0))
  expect_equal(result, c("LABEL", "42", "LABEL"))
  fclear()
})

test_that("fput expression with two extra args", {
  fmt <- fnew(
    "ratio" = "sprintf('%s/%s', .x1, .x2)",
    name = "two_arg", type = "character"
  )
  result <- fput(c("ratio", "ratio"), fmt, c(3, 7), c(10, 20))
  expect_equal(result, c("3/10", "7/20"))
  fclear()
})

test_that("fput expression with scalar extra arg (recycled)", {
  fmt <- fnew(
    "val" = "sprintf('%s (N=%s)', .x1, .x2)",
    name = "scalar", type = "character"
  )
  result <- fput(c("val", "val"), fmt, c(42, 55), 100)
  expect_equal(result, c("42 (N=100)", "55 (N=100)"))
  fclear()
})

test_that("fput expression with no extra args warns", {
  fmt <- fnew("n" = "sprintf('%s', .x1)", type = "character")
  expect_warning(result <- fput("n", fmt))
  expect_true(is.na(result))
  fclear()
})

test_that("fput expression preserves NA handling", {
  fmt <- fnew(
    "n" = "sprintf('%s', .x1)",
    .missing = "MISS",
    name = "expr_na", type = "character"
  )
  result <- fput(c("n", NA), fmt, c(42, 0))
  expect_equal(result[1], "42")
  expect_equal(result[2], "MISS")
  fclear()
})

test_that("fputn passes extra args to expression labels", {
  fnew("1" = "sprintf('%.1f', .x1)", name = "expr_num", type = "numeric")
  result <- fputn(1, "expr_num", 3.14159)
  expect_equal(result, "3.1")
  fclear()
})

test_that("fputc passes extra args to expression labels", {
  fnew("a" = "paste0(.x1, '!')", name = "expr_chr", type = "character")
  result <- fputc("a", "expr_chr", "hello")
  expect_equal(result, "hello!")
  fclear()
})

test_that("fput_all with expression labels", {
  fmt <- fnew(
    "0,100,TRUE,TRUE" = "sprintf('Score: %s', .x1)",
    name = "expr_ml", type = "numeric"
  )
  result <- fput_all(50, fmt, 99)
  expect_type(result, "list")
  expect_equal(result[[1]], "Score: 99")
  fclear()
})

test_that("expression with ifelse", {
  fmt <- fnew(
    "val" = "ifelse(.x1 > 0, paste0('+', .x1), as.character(.x1))",
    name = "sign_fmt", type = "character"
  )
  result <- fput(c("val", "val", "val"), fmt, c(5, 0, -3))
  expect_equal(result, c("+5", "0", "-3"))
  fclear()
})

test_that("expression with .other label", {
  fmt <- fnew(
    "known" = "YES",
    .other = "sprintf('Unknown: %s', .x1)",
    name = "other_expr", type = "character"
  )
  result <- fput(c("known", "xyz"), fmt, c(1, 2))
  expect_equal(result, c("YES", "Unknown: 2"))
  fclear()
})

test_that("fparse roundtrip preserves expression labels", {
  txt <- '
VALUE stat (character)
  "n" = "sprintf(\'%s\', .x1)"
  "pct" = "sprintf(\'%.1f%%\', .x1 * 100)"
;
'
  result <- fparse(text = txt)
  expect_equal(result$stat$mappings[["n"]], "sprintf('%s', .x1)")

  # Apply the parsed format
  out <- fput(c("n", "pct"), "stat", c(42, 0.15))
  expect_equal(out, c("42", "15.0%"))
  fclear()
})


# ===========================================================================
# Explicit (eval) / e() Expression Label Tests
# ===========================================================================

test_that("e() sets eval attribute on a string", {
  label <- e("get_dttm('ru')")
  expect_equal(as.character(label), "get_dttm('ru')")
  expect_true(isTRUE(attr(label, "eval")))
  fclear()
})

test_that("e() errors on non-string input", {
  expect_error(e(42))
  expect_error(e(c("a", "b")))
  fclear()
})

test_that(".has_eval_attr detects eval attribute", {
  expect_true(ksformat:::.has_eval_attr(e("test")))
  expect_false(ksformat:::.has_eval_attr("test"))
  fclear()
})

test_that("fnew with e() wrapper creates eval-marked labels", {
  fmt <- fnew(
    "ts" = e("format(Sys.time(), '%Y')"),
    "static" = "Hello"
  )
  expect_true(isTRUE(attr(fmt$mappings[["ts"]], "eval")))
  expect_null(attr(fmt$mappings[["static"]], "eval"))
  fclear()
})

test_that("fput evaluates e() label calling user-defined function", {
  my_greet <- function(lang) {
    if (lang == "ru") "Привет" else "Hello"
  }
  fmt <- fnew(
    "ru" = e("my_greet('ru')"),
    "en" = e("my_greet('en')")
  )
  result <- fput(c("ru", "en"), fmt)
  expect_equal(result, c("\u041f\u0440\u0438\u0432\u0435\u0442", "Hello"))
  fclear()
})

test_that("fput with mixed e() and .xN labels", {
  fmt <- fnew(
    "ts" = e("format(Sys.time(), '%Y')"),
    "n"  = "sprintf('%s', .x1)"
  )
  result <- fput(c("ts", "n"), fmt, c(0, 42))
  expect_equal(result[1], format(Sys.time(), "%Y"))
  expect_equal(result[2], "42")
  fclear()
})

test_that("fput with e() label and .xN args combined", {
  fmt <- fnew(
    "val" = e("sprintf('%s items', .x1)")
  )
  result <- fput("val", fmt, 5)
  expect_equal(result, "5 items")
  fclear()
})

test_that("fparse with (eval) marker creates eval-marked labels", {
  result <- fparse(text = '
VALUE d_footer
  "left_ru" = "paste(\"A\", \"B\")" (eval)
  "left_en" = "Hello"
;
')
  expect_true(isTRUE(attr(result$d_footer$mappings[["left_ru"]], "eval")))
  expect_null(attr(result$d_footer$mappings[["left_en"]], "eval"))
  fclear()
})

test_that("fparse (eval) label is evaluated by fput", {
  my_suffix <- function() "!!!"
  fparse(text = '
VALUE eval_test
  "k" = "my_suffix()" (eval)
;
')
  result <- fput("k", "eval_test")
  expect_equal(result, "!!!")
  fclear()
})

test_that("fparse (eval) with .missing and .other", {
  result <- fparse(text = '
VALUE eval_mo
  "a" = "Static"
  .missing = "paste(\"M\", \"V\")" (eval)
  .other = "paste(\"O\", \"V\")" (eval)
;
')
  fmt <- result$eval_mo
  expect_true(isTRUE(attr(fmt$missing_label, "eval")))
  expect_true(isTRUE(attr(fmt$other_label, "eval")))

  res <- fput(c("a", NA, "zzz"), fmt)
  expect_equal(res[1], "Static")
  expect_equal(res[2], "M V")
  expect_equal(res[3], "O V")
  fclear()
})

test_that("fexport roundtrip preserves (eval) flag", {
  fmt <- fnew(
    "ts" = e("format(Sys.time(), '%Y')"),
    "static" = "Hello",
    name = "rt_test"
  )
  txt <- fexport(rt_test = fmt)
  expect_match(txt, '(eval)', fixed = TRUE)

  fclear()
  reparsed <- fparse(text = txt)
  expect_true(isTRUE(attr(reparsed$rt_test$mappings[["ts"]], "eval")))
  expect_null(attr(reparsed$rt_test$mappings[["static"]], "eval"))
  fclear()
})

test_that("fexport roundtrip preserves (eval) on .missing/.other", {
  fmt <- fnew(
    "a" = "Static",
    .missing = e("paste('miss')"),
    .other = e("paste('other')"),
    name = "rt_mo"
  )
  txt <- fexport(rt_mo = fmt)
  expect_match(txt, '.missing = "paste(\'miss\')" (eval)', fixed = TRUE)
  expect_match(txt, '.other = "paste(\'other\')" (eval)', fixed = TRUE)

  fclear()
  reparsed <- fparse(text = txt)
  expect_true(isTRUE(attr(reparsed$rt_mo$missing_label, "eval")))
  expect_true(isTRUE(attr(reparsed$rt_mo$other_label, "eval")))
  fclear()
})

test_that("fput_all with e() eval labels", {
  my_tag <- function() "TAG"
  fmt <- fnew(
    "a" = e("my_tag()"),
    "b" = "Static"
  )
  result <- fput_all(c("a", "b"), fmt)
  expect_equal(result[[1]], "TAG")
  expect_equal(result[[2]], "Static")
  fclear()
})

test_that("existing .xN expressions still work without (eval)", {
  fmt <- fnew("n" = "sprintf('%s', .x1)")
  result <- fput("n", fmt, 42)
  expect_equal(result, "42")
  fclear()
})


# ===========================================================================
# Edge Case Tests
# ===========================================================================

context("Edge Cases: Format Creation")

test_that("fnew errors with no mappings", {
  expect_error(fnew(), "At least one")
})

test_that("fnew with default parameter overrides .other", {
  fmt <- fnew("A" = "Alpha", .other = "Ignored", default = "Default-Win")
  result <- fput(c("A", "Z"), fmt)
  expect_equal(result, c("Alpha", "Default-Win"))
})

test_that("fnew with only .missing and .other", {
  fmt <- fnew("X" = "Found", .missing = "Missing", .other = "Other")
  result <- fput(c("X", NA, "Z"), fmt)
  expect_equal(result, c("Found", "Missing", "Other"))
})

test_that("detect_format_type returns character for empty keys", {
  expect_equal(detect_format_type(character(0)), "character")
})

test_that("detect_format_type returns numeric for comma keys", {
  expect_equal(detect_format_type(c("0,18")), "numeric")
})

test_that("detect_format_type returns numeric for NA keys", {
  expect_equal(detect_format_type(c(NA_character_)), "numeric")
})

test_that("detect_format_type returns numeric for empty string keys", {
  expect_equal(detect_format_type(c("")), "numeric")
})

test_that("print.ks_format shows both multilabel and nocase flags", {
  fmt <- fnew("A" = "Alpha", name = "dual",
              type = "character", multilabel = TRUE, ignore_case = TRUE)
  output <- capture.output(print(fmt))
  expect_true(any(grepl("multilabel", output)))
  expect_true(any(grepl("nocase", output)))
  fclear()
})

test_that("print.ks_format shows range keys in interval notation", {
  fmt <- fnew("0,18,TRUE,FALSE" = "Child",
              "18,65,TRUE,FALSE" = "Adult",
              name = "ranges_print", type = "numeric")
  output <- capture.output(print(fmt))
  expect_true(any(grepl("\\[0, 18\\)", output)))
  expect_true(any(grepl("\\[18, 65\\)", output)))
  fclear()
})


context("Edge Cases: Format Application")

test_that("fput with empty vector returns empty character", {
  fmt <- fnew("M" = "Male")
  result <- fput(character(0), fmt)
  expect_equal(result, character(0))
})

test_that("fput with all-NA vector uses missing label", {
  fmt <- fnew("M" = "Male", .missing = "N/A")
  result <- fput(c(NA, NA, NA), fmt)
  expect_equal(result, c("N/A", "N/A", "N/A"))
})

test_that("fput with NaN uses missing label", {
  fmt <- fnew("1" = "One", .missing = "Miss", type = "numeric")
  result <- fput(c(1, NaN), fmt)
  expect_equal(result, c("One", "Miss"))
})

test_that("fput with NaN and keep_na preserves NA", {
  fmt <- fnew("1" = "One", .missing = "Miss", type = "numeric")
  result <- fput(c(1, NaN), fmt, keep_na = TRUE)
  expect_equal(result[1], "One")
  expect_true(is.na(result[2]))
})

test_that("fput with single element vector", {
  fmt <- fnew("A" = "Alpha")
  expect_equal(fput("A", fmt), "Alpha")
  expect_equal(fput("Z", fmt), "Z")
  expect_equal(fput(NA, fmt), NA_character_)
})

test_that("fput errors on invalid format type", {
  expect_error(fput(c("a", "b"), 42), "ks_format")
  expect_error(fput(c("a", "b"), list(a = 1)), "ks_format")
})

test_that("fput with non-numeric strings in numeric format range matching", {
  fmt <- fnew("0,100,TRUE,FALSE" = "Valid", type = "numeric",
              .other = "Other")
  result <- fput(c("50", "abc", "200"), fmt)
  expect_equal(result, c("Valid", "Other", "Other"))
})


context("Edge Cases: fput_df")

test_that("fput_df with replace = TRUE", {
  df <- data.frame(sex = c("M", "F"), stringsAsFactors = FALSE)
  fmt <- fnew("M" = "Male", "F" = "Female")
  result <- fput_df(df, sex = fmt, replace = TRUE)

  expect_equal(result$sex, c("Male", "Female"))
  expect_false("sex_fmt" %in% names(result))
})

test_that("fput_df warns on missing column", {
  df <- data.frame(x = 1:3)
  fmt <- fnew("1" = "One", type = "numeric")
  expect_warning(fput_df(df, nonexistent = fmt), "not found")
})

test_that("fput_df with custom suffix", {
  df <- data.frame(sex = c("M", "F"), stringsAsFactors = FALSE)
  fmt <- fnew("M" = "Male", "F" = "Female")
  result <- fput_df(df, sex = fmt, suffix = "_label")

  expect_true("sex_label" %in% names(result))
  expect_equal(result$sex_label, c("Male", "Female"))
})

test_that("fput_df errors on non-data.frame", {
  expect_error(fput_df(list(a = 1), a = fnew("1" = "x", type = "numeric")),
               "data frame")
})


context("Edge Cases: Invalue")

test_that(".invalue_apply with na_if parameter", {
  inv <- finput("Yes" = 1, "No" = 0)
  result <- ksformat:::.invalue_apply(c("Yes", "No", "Unknown", "N/A"), inv,
                          na_if = c("Unknown", "N/A"))
  expect_equal(result[1], 1)
  expect_equal(result[2], 0)
  expect_true(is.na(result[3]))
  expect_true(is.na(result[4]))
})

test_that(".invalue_apply falls back to numeric conversion for unknown labels", {
  inv <- finput("Low" = 1, "High" = 2)
  result <- ksformat:::.invalue_apply(c("Low", "42", "High"), inv)
  expect_equal(result, c(1, 42, 2))
})

test_that(".invalue_apply with custom missing_value", {
  inv <- finput("A" = 1, "B" = 2, missing_value = -999)
  result <- ksformat:::.invalue_apply(c("A", NA, "B"), inv)
  expect_equal(result, c(1, -999, 2))
})

test_that(".invalue_apply with NULL returns empty typed vector", {
  inv <- finput("A" = 1)
  result <- ksformat:::.invalue_apply(NULL, inv)
  expect_equal(length(result), 0)
  expect_true(is.numeric(result))

  inv_chr <- finput("A" = "x", target_type = "character")
  result_chr <- ksformat:::.invalue_apply(NULL, inv_chr)
  expect_equal(length(result_chr), 0)
  expect_true(is.character(result_chr))
})

test_that(".invalue_apply with target_type integer", {
  inv <- finput("One" = 1L, "Two" = 2L, target_type = "integer")
  result <- ksformat:::.invalue_apply(c("One", "Two"), inv)
  expect_equal(result, c(1L, 2L))
  expect_true(is.integer(result))
})

test_that(".invalue_apply with target_type logical", {
  inv <- finput("Yes" = TRUE, "No" = FALSE, target_type = "logical")
  result <- ksformat:::.invalue_apply(c("Yes", "No"), inv)
  expect_equal(result, c(TRUE, FALSE))
  expect_true(is.logical(result))
})

test_that("finputn errors for non-invalue", {
  fnew("M" = "Male", name = "not_inv")
  expect_error(finputn(c("Male"), "not_inv"), "INVALUE")
  fclear()
})

test_that("finputc errors for non-invalue", {
  fnew("M" = "Male", name = "not_inv2")
  expect_error(finputc(c("Male"), "not_inv2"), "INVALUE")
  fclear()
})

test_that(".invalue_apply errors on invalid invalue type", {
  expect_error(ksformat:::.invalue_apply(c("a"), 123), "ks_invalue")
})

test_that("fnew_bid without name", {
  bi <- fnew_bid("X" = "Ex", "Y" = "Why")
  expect_s3_class(bi$format, "ks_format")
  expect_s3_class(bi$invalue, "ks_invalue")
  expect_null(bi$format$name)
  expect_null(bi$invalue$name)
})

test_that("print.ks_invalue shows custom missing_value", {
  inv <- finput("A" = 1, missing_value = -99)
  output <- capture.output(print(inv))
  expect_true(any(grepl("-99", output)))
})

test_that("print.ks_invalue hides NA missing_value", {
  inv <- finput("A" = 1)
  output <- capture.output(print(inv))
  expect_false(any(grepl("Missing value", output)))
})


context("Edge Cases: Utilities and Validation")

test_that("is_missing with NaN", {
  expect_true(is_missing(NaN))
})

test_that("is_missing with empty string and include_empty", {
  expect_true(is_missing(""))
})

test_that("is_missing with vector input", {
  result <- is_missing(c(1, NA, NaN, 3))
  expect_equal(result, c(FALSE, TRUE, TRUE, FALSE))
})

test_that("range_spec errors with non-numeric bounds", {
  expect_error(range_spec("a", 10, "lbl"), "numeric")
  expect_error(range_spec(1, "b", "lbl"), "numeric")
})

test_that("range_spec errors when low > high", {
  expect_error(range_spec(100, 1, "lbl"), "less than")
})

test_that("in_range returns FALSE for non-range_spec", {
  expect_false(in_range(5, list(low = 0, high = 10)))
})

test_that(".format_validate warns on duplicate keys", {
  expect_warning(fnew("A" = "First", "A" = "Second"), "Duplicate")
})

test_that(".format_validate warns on invalid numeric keys", {
  expect_warning(fnew("abc" = "Label", type = "numeric"),
                 "not a valid numeric")
})

test_that("fclear warns for non-existent format name", {
  fclear()
  expect_warning(fclear("no_such_fmt"), "not found")
})

test_that("fprint errors for non-existent format name", {
  fclear()
  expect_error(fprint("no_such_fmt"), "not found")
})

test_that("fprint shows empty library message", {
  fclear()
  output <- capture.output(fprint())
  expect_true(any(grepl("empty", output)))
})

test_that(".format_register errors with overwrite = FALSE", {
  fnew("A" = "Alpha", name = "ow_test")
  fmt2 <- fnew("B" = "Beta")
  expect_error(
    ksformat:::.format_register(fmt2, name = "ow_test", overwrite = FALSE),
    "already exists"
  )
  fclear()
})

test_that(".format_register silently ignores NULL/empty name", {
  fmt <- fnew("A" = "Alpha")
  # Should not error
  expect_silent(ksformat:::.format_register(fmt, name = NULL))
  expect_silent(ksformat:::.format_register(fmt, name = ""))
})

test_that(".format_get errors with clear message on empty library", {
  fclear()
  expect_error(ksformat:::.format_get("nonexistent"), "empty")
})

test_that(".format_get errors listing available formats", {
  fnew("A" = "Alpha", name = "avail")
  expect_error(ksformat:::.format_get("nonexistent"), "avail")
  fclear()
})


context("Edge Cases: Expression Labels")

test_that("eval_expr_label warns and returns NA on error", {
  fmt <- fnew("x" = "stop(.x1)", type = "character")
  expect_warning(
    result <- fput("x", fmt, "bad"),
    "failed"
  )
  expect_true(is.na(result))
})

test_that("eval_expr_label with expression referencing .x1", {
  fmt <- fnew("a" = "paste0('val=', .x1)", type = "character")
  result <- fput(c("a", "a"), fmt, c(10, 20))
  expect_equal(result, c("val=10", "val=20"))
})


context("Edge Cases: Format Parsing")

test_that("fparse with text as character vector", {
  txt <- c(
    "VALUE test (character)",
    "  \"A\" = \"Alpha\"",
    "  \"B\" = \"Beta\"",
    ";"
  )
  result <- fparse(text = txt)
  expect_equal(result$test$mappings[["A"]], "Alpha")
  expect_equal(result$test$mappings[["B"]], "Beta")
  fclear()
})

test_that("fparse warns on unclosed block", {
  txt <- '
VALUE unclosed (character)
  "A" = "Alpha"
'
  expect_warning(fparse(text = txt), "Closing automatically")
  fclear()
})

test_that("fparse warns on new block before closing previous", {
  txt <- '
VALUE first (character)
  "A" = "Alpha"
VALUE second (character)
  "B" = "Beta"
;
'
  expect_warning(fparse(text = txt), "New block started")
  fclear()
})

test_that("fparse handles inline comments", {
  txt <- '
VALUE test (character)
  "A" = "Alpha"  // this is a comment
  "B" = "Beta"   /* another comment */
;
'
  result <- fparse(text = txt)
  expect_equal(result$test$mappings[["A"]], "Alpha")
  expect_equal(result$test$mappings[["B"]], "Beta")
  fclear()
})

test_that("fparse handles multilabel + nocase together", {
  txt <- '
VALUE combo (numeric, multilabel, nocase)
  [0, 50) = "Low"
  [50, 100] = "High"
;
'
  result <- fparse(text = txt)
  expect_true(result$combo$multilabel)
  expect_true(result$combo$ignore_case)
  fclear()
})

test_that("fparse handles .missing in INVALUE block", {
  txt <- '
INVALUE inv_miss
  "Cat" = 1
  "Dog" = 2
  .missing = 0
;
'
  result <- fparse(text = txt)
  # .missing in invalue should produce a missing_value
  expect_true("inv_miss" %in% ls(envir = ksformat:::.format_library))
  fclear()
})


context("Edge Cases: Format Export")

test_that("fexport with formats parameter instead of ...", {
  fmt1 <- fnew("A" = "Alpha", name = "exp1")
  fmt2 <- fnew("B" = "Beta", name = "exp2")
  txt <- fexport(formats = list(fmt1, fmt2))
  expect_true(grepl("exp1", txt))
  expect_true(grepl("exp2", txt))
  fclear()
})

test_that("fexport warns for invalid object", {
  fmt <- fnew("A" = "Alpha", name = "ok")
  expect_warning(fexport(ok = fmt, bad = list(x = 1)), "not a.*ks_format")
  fclear()
})

test_that("fexport errors with no objects", {
  expect_error(fexport(), "At least one")
})


context("Edge Cases: Datetime Formats")

test_that("fnew_date errors for invalid type", {
  expect_error(fnew_date("%Y", type = "integer"), "date.*time.*datetime")
})

test_that("fnew_date with .missing parameter", {
  fmt <- fnew_date("DATE9.", .missing = "No Date")
  result <- fput(c(0, NA), fmt)
  expect_equal(result[2], "No Date")
})

test_that("fput date with character date strings", {
  fmt <- fnew_date("DATE9.")
  result <- fput(as.Date("2024-01-15"), fmt)
  expect_true(nchar(result) > 0)
})

test_that("fput date with POSIXct conversion to date", {
  fmt <- fnew_date("DATE9.")
  dt <- as.POSIXct("2024-01-15 10:30:00")
  result <- fput(dt, fmt)
  expect_true(nchar(result) > 0)
})

test_that("fput time with character time strings", {
  fmt <- fnew_date("TIME8.", type = "time")
  result <- fput("12:30:00", fmt)
  expect_true(nchar(result) > 0)
})

test_that("fput datetime with numeric R-epoch (default)", {
  fmt <- fnew_date("DATETIME20.")
  # R epoch: 0 = 1970-01-01
  result <- fput(0, fmt)
  expect_true(grepl("1970", result))
})

test_that("SAS format name DDMMYY8 resolves correctly", {
  fmt <- fnew_date("DDMMYY8.")
  result <- fput(as.Date("2024-03-15"), fmt)
  expect_true(nchar(result) > 0)
})

test_that("SAS format DOWNAME resolves correctly", {
  fmt <- fnew_date("DOWNAME.")
  result <- fput(as.Date("2024-01-15"), fmt)  # Monday
  expect_true(nchar(result) > 0)
})

test_that("SAS format MONNAME resolves correctly", {
  fmt <- fnew_date("MONNAME.")
  result <- fput(as.Date("2024-03-15"), fmt)
  expect_true(nchar(result) > 0)
})

test_that("fput_all with datetime format returns list", {
  fmt <- fnew_date("DATE9.")
  result <- fput_all(c(as.Date("2024-01-01"), as.Date("2024-06-15")), fmt)
  expect_true(is.list(result))
  expect_equal(length(result), 2)
})


context("Edge Cases: fput_all")

test_that("fput_all with empty vector returns empty list", {
  fmt <- fnew("A" = "Alpha", type = "character")
  result <- fput_all(character(0), fmt)
  expect_equal(result, list())
})

test_that("fput_all with all-NA vector", {
  fmt <- fnew("A" = "Alpha", .missing = "N/A")
  result <- fput_all(c(NA, NA), fmt)
  expect_equal(result[[1]], "N/A")
  expect_equal(result[[2]], "N/A")
})

test_that("fput_all with NaN uses missing label", {
  fmt <- fnew("1" = "One", .missing = "Miss", type = "numeric")
  result <- fput_all(NaN, fmt)
  expect_equal(result[[1]], "Miss")
})

test_that("fput_all unmatched without .other returns original value", {
  fmt <- fnew("A" = "Alpha", type = "character")
  result <- fput_all("Z", fmt)
  expect_equal(result[[1]], "Z")
})

test_that("fput_all errors on invalid format", {
  expect_error(fput_all(c(1, 2), 42), "ks_format")
})


context("Edge Cases: Library Management")

test_that("fclear on already empty library does not error", {
  fclear()
  expect_message(fclear(), "cleared")
})

test_that("fclear single format leaves others intact", {
  fnew("A" = "Alpha", name = "keep_me")
  fnew("B" = "Beta", name = "remove_me")
  expect_message(fclear("remove_me"), "removed")
  expect_true("keep_me" %in% ls(envir = ksformat:::.format_library))
  expect_false("remove_me" %in% ls(envir = ksformat:::.format_library))
  fclear()
})

test_that("fnew_bid registers both format and invalue", {
  bi <- fnew_bid("X" = "Ex", name = "bi_test")
  expect_true("bi_test" %in% ls(envir = ksformat:::.format_library))
  expect_true("bi_test_inv" %in% ls(envir = ksformat:::.format_library))
  fclear()
})

test_that(".format_get resolves SAS datetime format on the fly", {
  fclear()
  # SAS format name not registered, but should auto-create via fallback
  result <- fput(as.Date("2024-01-15"), "DATE9.")
  expect_true(nchar(result) > 0)
  fclear()
})

# ============================================================
# Code review fixes — regression tests
# ============================================================

test_that("fput expression: scalar x with scalar extra arg", {
  fparse(text = '
  VALUE stat (character)
    "mean" = "sprintf(\"%.2f\", .x1)"
  ;
  ')
  result <- fput("mean", "stat", 1.23456)
  expect_equal(result, "1.23")
  fclear()
})

test_that("fput expression: scalar x with vector extra arg", {
  fparse(text = '
  VALUE stat (character)
    "mean" = "sprintf(\"%.2f\", .x1)"
  ;
  ')
  result <- fput("mean", "stat", c(1.23456, 2.34567, 3.45678))
  expect_equal(result, c("1.23", "2.35", "3.46"))
  fclear()
})

test_that("fput expression: matching-length x and extra arg", {
  fparse(text = '
  VALUE stat (character)
    "mean" = "sprintf(\"%.2f\", .x1)"
  ;
  ')
  result <- fput(c("mean", "mean", "mean"), "stat", c(1.23456, 2.34567, 3.45678))
  expect_equal(result, c("1.23", "2.35", "3.46"))
  fclear()
})

test_that("fput vector recycling: length mismatch errors", {
  fparse(text = '
  VALUE stat
    "mean" = "sprintf(\"%.2f\", .x1)"
  ;
  ')
  # x has length 2, extra arg has length 3 → error

  expect_error(
    fput(c("mean", "mean"), "stat", c(1.23, 2.34, 3.45)),
    "Length mismatch"
  )
  # Multiple extra args with different non-scalar lengths → error
  expect_error(
    fput("mean", "stat", c(1, 2), c(3, 4, 5)),
    "Length mismatch in extra arguments"
  )
  fclear()
})

test_that(".eval_expr_label handles unparseable expression gracefully", {
  # Labels with .xN trigger expression evaluation
  fmt <- fnew("x" = "this is not valid R {{{{ .x1", name = "bad_expr")
  expect_warning(
    result <- fput("x", fmt, 1),
    "parse failed"
  )
  expect_equal(result, NA_character_)
  fclear()
})

test_that(".eval_expr_label handles NULL/empty expression result", {
  # NULL expression via .xN pattern to trigger eval
  fmt <- fnew("x" = "{ .x1; NULL }", name = "null_expr")
  expect_warning(
    result <- fput("x", fmt, 1),
    "empty result"
  )
  expect_equal(result, NA_character_)
  fclear()
})

test_that(".invalue_apply preserves explicitly-mapped NA values", {
  inv <- finput("Known" = 1, "Unknown" = NA, name = "na_test_inv")
  result <- ksformat:::.invalue_apply(c("Known", "Unknown"), inv)
  expect_equal(result, c(1, NA_real_))
  fclear()
})

test_that("fnew_bid warns on duplicate values (ambiguous reverse)", {
  expect_warning(
    fnew_bid("M" = "Adult", "F" = "Adult", name = "dup_val"),
    "Multiple keys map to the same value"
  )
  fclear()
})

test_that("finputn warns on non-numeric target_type", {
  finput("Male" = "M", "Female" = "F",
         name = "char_inv", target_type = "character")
  expect_warning(
    finputn(c("Male", "Female"), "char_inv"),
    "not numeric"
  )
  fclear()
})

test_that(".format_get errors on ambiguous case-insensitive matches", {
  fnew("M" = "Male", name = "Sex")
  fnew("1" = "One", name = "sex")
  expect_error(
    fput("M", "SEX"),
    "Ambiguous"
  )
  fclear()
})

test_that("in_range returns FALSE for NA input", {
  rs <- range_spec(0, 10, "test")
  expect_false(in_range(NA, rs))
})

test_that("finput rejects non-scalar mapping values", {
  expect_error(
    finput("A" = c(1, 2), name = "bad_inv"),
    "must be scalar"
  )
})

test_that("fimport reads SAS CNTLOUT CSV", {
  csv_file <- system.file("testdata", "test_cntlout.csv", package = "ksformat")
  skip_if(!nzchar(csv_file), "test_cntlout.csv not found")
  imported <- fimport(csv_file)
  expect_true(length(imported) > 0)
  fclear()
})

# --- fputk (composite key lookup) ---

test_that("fputk pastes two vectors and applies format", {
  fnew("A|1" = "X", "A|2" = "Y", "B|1" = "Z",
       name = "fputk_test", type = "character")
  result <- fputk(c("A", "A", "B"), c(1, 2, 1), format = "fputk_test")
  expect_equal(result, c("X", "Y", "Z"))
  fclear()
})

test_that("fputk uses .other for unmatched composite keys", {
  fnew("A|1" = "X", .other = "MISS",
       name = "fputk_other", type = "character")
  result <- fputk(c("A", "B"), c(1, 1), format = "fputk_other")
  expect_equal(result, c("X", "MISS"))
  fclear()
})

test_that("fputk supports custom separator", {
  fnew("A-1" = "X", name = "fputk_sep", type = "character")
  result <- fputk("A", 1, format = "fputk_sep", sep = "-")
  expect_equal(result, "X")
  fclear()
})

test_that("fputk passes keep_na through", {
  fnew("A|1" = "X", .missing = "NA_LABEL",
       name = "fputk_na", type = "character")
  result_mapped <- fputk(NA, 1, format = "fputk_na")
  result_kept   <- fputk(NA, 1, format = "fputk_na", keep_na = TRUE)
  expect_equal(result_mapped, "NA_LABEL")
  expect_true(is.na(result_kept))
  fclear()
})

test_that("fputk works with three key components", {
  fnew("A|1|X" = "found", .other = "nope",
       name = "fputk_3", type = "character")
  result <- fputk("A", 1, "X", format = "fputk_3")
  expect_equal(result, "found")
  fclear()
})

test_that("fputk accepts a ks_format object directly", {
  fmt <- fnew("A|1" = "hit", .other = "miss", type = "character")
  result <- fputk(c("A", "B"), c(1, 1), format = fmt)
  expect_equal(result, c("hit", "miss"))
})

test_that("fputk na_as_string matches paste()/fmap()-built keys", {
  # Format built via fmap(paste(...), values) — paste() stores literal "NA"
  fnew(
    fmap(
      paste(c("CHEM", "COAG", "COAG"), c("ALB", "FIBRINO", "INR"),
            c("g/L", "g/L", NA), sep = "|"),
      c("ALB", "FIBRINO", "INR")
    ),
    .other = NA, name = "fputk_na_string", type = "character"
  )

  # With na_as_string = TRUE the literal "NA" propagates and matches
  res_keep <- fputk(c("CHEM", "COAG", "COAG"),
                    c("ALB", "FIBRINO", "INR"),
                    c("g/L", "g/L", NA),
                    format = "fputk_na_string", na_as_string = TRUE)
  expect_equal(res_keep, c("ALB", "FIBRINO", "INR"))

  # Default (FALSE) restores NA so the third row falls through to .other
  res_default <- fputk(c("CHEM", "COAG", "COAG"),
                       c("ALB", "FIBRINO", "INR"),
                       c("g/L", "g/L", NA),
                       format = "fputk_na_string")
  expect_equal(res_default, c("ALB", "FIBRINO", NA_character_))
  fclear()
})

test_that("fputk na_as_string default still routes NA via .missing", {
  fnew("A|1" = "hit", .missing = "MISS",
       name = "fputk_default_na", type = "character")
  # Default behavior unchanged
  expect_equal(fputk(NA, 1, format = "fputk_default_na"), "MISS")
  # na_as_string = TRUE keeps the literal "NA|1" key — no .missing routing
  expect_equal(
    fputk(NA, 1, format = "fputk_default_na", na_as_string = TRUE),
    "NA|1"
  )
  fclear()
})


# --- finputk (composite label invalue lookup) ---

test_that("finputk pastes labels and applies invalue", {
  finput(
    fmap(paste(c("A", "A", "B"), c(1, 2, 1), sep = "|"), c(10, 20, 30)),
    name = "finputk_test"
  )
  result <- finputk(c("A", "A", "B"), c(1, 2, 1),
                    invalue_name = "finputk_test")
  expect_equal(result, c(10, 20, 30))
  fclear()
})

test_that("finputk supports custom separator", {
  finput("A-1" = 7, name = "finputk_sep")
  result <- finputk("A", 1, invalue_name = "finputk_sep", sep = "-")
  expect_equal(result, 7)
  fclear()
})

test_that("finputk na_as_string matches paste()/fmap()-built labels", {
  finput(
    fmap(paste(c("CHEM", "COAG"), c("INR", "INR"),
               c("g/L", NA), sep = "|"),
         c(1, 2)),
    name = "finputk_na_string"
  )
  res_keep <- finputk(c("CHEM", "COAG"), c("INR", "INR"), c("g/L", NA),
                      invalue_name = "finputk_na_string",
                      na_as_string = TRUE)
  expect_equal(res_keep, c(1, 2))

  res_default <- finputk(c("CHEM", "COAG"), c("INR", "INR"), c("g/L", NA),
                         invalue_name = "finputk_na_string")
  expect_equal(res_default, c(1, NA_real_))
  fclear()
})

test_that("finputk respects target_type from stored invalue", {
  finput("A|1" = "X", name = "finputk_char", target_type = "character")
  expect_type(finputk("A", 1, invalue_name = "finputk_char"), "character")
  fclear()
})

test_that("finputk errors on non-ks_invalue lookup", {
  fnew("A|1" = "X", name = "finputk_not_inv", type = "character")
  expect_error(
    finputk("A", 1, invalue_name = "finputk_not_inv"),
    "not an INVALUE format"
  )
  fclear()
})

test_that("finputk errors when no components are provided", {
  finput("A" = 1, name = "finputk_empty")
  expect_error(finputk(invalue_name = "finputk_empty"),
               "At least one label component")
  fclear()
})


# ===========================================================================
# Value Type System (Date, POSIXct, logical)
# ===========================================================================

context("Value Type Formats")

# --- fnew with value types ---

test_that("fnew creates Date format from named Date vector", {
  dates <- setNames(
    as.Date(c("2025-01-15", "2025-02-20", "2025-03-10")),
    c("A|1", "A|2", "B|1")
  )
  fmt <- fnew(dates, type = "Date")
  expect_equal(fmt$type, "Date")
  expect_equal(length(fmt$mappings), 3L)
  expect_s3_class(fmt$mappings[["A|1"]], "Date")
  expect_equal(fmt$mappings[["A|1"]], as.Date("2025-01-15"))
  expect_null(fmt$missing_label)
  expect_null(fmt$other_label)
})

test_that("fnew creates Date format from direct named args", {
  fmt <- fnew(
    "A" = as.Date("2025-01-15"),
    "B" = as.Date("2025-02-20"),
    type = "Date"
  )
  expect_equal(fmt$type, "Date")
  expect_s3_class(fmt$mappings[["A"]], "Date")
  expect_equal(fmt$mappings[["B"]], as.Date("2025-02-20"))
})

test_that("fnew auto-detects Date type from values", {
  dates <- setNames(
    as.Date(c("2025-01-01", "2025-06-01")),
    c("key1", "key2")
  )
  fmt <- fnew(dates)
  expect_equal(fmt$type, "Date")
})

test_that("fnew auto-detects logical type from values", {
  fmt <- fnew("yes" = TRUE, "no" = FALSE)
  expect_equal(fmt$type, "logical")
  expect_true(fmt$mappings[["yes"]])
  expect_false(fmt$mappings[["no"]])
})

test_that("fnew warns when .missing is non-NA for value types", {
  expect_warning(
    fnew("A" = as.Date("2025-01-01"), .missing = "Missing", type = "Date"),
    "ignored"
  )
})

test_that("fnew warns when .other is non-NA for value types", {
  expect_warning(
    fnew("A" = as.Date("2025-01-01"), .other = "Other", type = "Date"),
    "ignored"
  )
})

test_that("fnew allows .missing = NA and .other = NA without warning for value types", {
  expect_silent(
    fnew("A" = as.Date("2025-01-01"), .missing = NA, .other = NA, type = "Date")
  )
})

test_that("fnew creates POSIXct format", {
  fmt <- fnew(
    "event1" = as.POSIXct("2025-01-15 10:30:00"),
    "event2" = as.POSIXct("2025-02-20 14:00:00"),
    type = "POSIXct"
  )
  expect_equal(fmt$type, "POSIXct")
  expect_s3_class(fmt$mappings[["event1"]], "POSIXct")
})

test_that("fnew creates logical format", {
  fmt <- fnew("Y" = TRUE, "N" = FALSE, type = "logical")
  expect_equal(fmt$type, "logical")
  expect_true(fmt$mappings[["Y"]])
  expect_false(fmt$mappings[["N"]])
})

test_that("fnew stores and retrieves Date format by name", {
  fnew(
    "A" = as.Date("2025-01-15"),
    "B" = as.Date("2025-02-20"),
    name = "test_date_fmt", type = "Date"
  )
  fmt <- format_get("test_date_fmt")
  expect_equal(fmt$type, "Date")
  expect_s3_class(fmt$mappings[["A"]], "Date")
  fclear()
})

# --- fput with value types ---

test_that("fput returns Date vector for Date format", {
  fmt <- fnew(
    "A" = as.Date("2025-01-15"),
    "B" = as.Date("2025-02-20"),
    type = "Date"
  )
  result <- fput(c("A", "B"), fmt)
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date(c("2025-01-15", "2025-02-20")))
})

test_that("fput returns typed NA for unmatched values in Date format", {
  fmt <- fnew("A" = as.Date("2025-01-15"), type = "Date")
  result <- fput(c("A", "X"), fmt)
  expect_s3_class(result, "Date")
  expect_equal(result[1], as.Date("2025-01-15"))
  expect_true(is.na(result[2]))
})

test_that("fput returns typed NA for missing values in Date format", {
  fmt <- fnew("A" = as.Date("2025-01-15"), type = "Date")
  result <- fput(c("A", NA), fmt)
  expect_s3_class(result, "Date")
  expect_equal(result[1], as.Date("2025-01-15"))
  expect_true(is.na(result[2]))
})

test_that("fput returns logical vector for logical format", {
  fmt <- fnew("Y" = TRUE, "N" = FALSE, type = "logical")
  result <- fput(c("Y", "N", "Y"), fmt)
  expect_type(result, "logical")
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("fput returns NA for unmatched in logical format", {
  fmt <- fnew("Y" = TRUE, "N" = FALSE, type = "logical")
  result <- fput(c("Y", "X", NA), fmt)
  expect_type(result, "logical")
  expect_true(result[1])
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that("fput returns POSIXct vector for POSIXct format", {
  t1 <- as.POSIXct("2025-01-15 10:30:00")
  t2 <- as.POSIXct("2025-02-20 14:00:00")
  fmt <- fnew("e1" = t1, "e2" = t2, type = "POSIXct")
  result <- fput(c("e1", "e2"), fmt)
  expect_s3_class(result, "POSIXct")
  expect_equal(result, c(t1, t2))
})

test_that("fput preserves POSIXct tzone from source values (UTC)", {
  t1 <- as.POSIXct("2021-02-09 11:35", format = "%Y-%m-%d %H:%M", tz = "UTC")
  t2 <- as.POSIXct("2021-02-09 12:35", format = "%Y-%m-%d %H:%M", tz = "UTC")
  fmt <- fnew(fmap(c("001|909", "002|910"), c(t1, t2)),
              type = "POSIXct", name = "trtstart_dttn")
  on.exit(fclear(), add = TRUE)

  result <- fputk(c("001", "002"), c("909", "910"), format = "trtstart_dttn")
  expect_s3_class(result, "POSIXct")
  expect_equal(attr(result, "tzone"), "UTC")
  # Display in UTC must match the source instants regardless of system TZ
  expect_equal(format(result, tz = "UTC", format = "%Y-%m-%d %H:%M"),
               c("2021-02-09 11:35", "2021-02-09 12:35"))
  expect_equal(as.numeric(result), as.numeric(c(t1, t2)))
})

test_that("fput handles empty input for value types", {
  fmt <- fnew("A" = as.Date("2025-01-15"), type = "Date")
  result <- fput(NULL, fmt)
  expect_s3_class(result, "Date")
  expect_equal(length(result), 0L)
})

test_that("fput handles all-NA input for value types", {
  fmt <- fnew("A" = as.Date("2025-01-15"), type = "Date")
  result <- fput(c(NA, NA), fmt)
  expect_s3_class(result, "Date")
  expect_true(all(is.na(result)))
})

test_that("fput with keep_na for value types", {
  fmt <- fnew("A" = as.Date("2025-01-15"), type = "Date")
  result <- fput(c("A", NA), fmt, keep_na = TRUE)
  expect_s3_class(result, "Date")
  expect_equal(result[1], as.Date("2025-01-15"))
  expect_true(is.na(result[2]))
})

# --- fputk with value types ---

test_that("fputk returns Date vector for Date format", {
  fnew(
    "A|1" = as.Date("2025-01-15"),
    "A|2" = as.Date("2025-02-20"),
    "B|1" = as.Date("2025-03-10"),
    name = "visit_dt", type = "Date"
  )
  result <- fputk(c("A", "A", "B"), c(1, 2, 1), format = "visit_dt")
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date(c("2025-01-15", "2025-02-20", "2025-03-10")))
  fclear()
})

test_that("fputk returns typed NA for unmatched composite keys", {
  fmt <- fnew(
    "A|1" = as.Date("2025-01-15"),
    type = "Date"
  )
  result <- fputk(c("A", "B"), c(1, 1), format = fmt)
  expect_s3_class(result, "Date")
  expect_equal(result[1], as.Date("2025-01-15"))
  expect_true(is.na(result[2]))
})

# --- fput_df with value types ---

test_that("fput_df creates typed column for Date format", {
  fmt <- fnew("A" = as.Date("2025-01-15"), "B" = as.Date("2025-02-20"),
              type = "Date")
  df <- data.frame(code = c("A", "B", "A"), stringsAsFactors = FALSE)
  result <- fput_df(df, code = fmt)
  expect_s3_class(result$code_fmt, "Date")
  expect_equal(result$code_fmt, as.Date(c("2025-01-15", "2025-02-20", "2025-01-15")))
})

# --- fput_all with value types ---

test_that("fput_all returns list of typed values for Date format", {
  fmt <- fnew("A" = as.Date("2025-01-15"), "B" = as.Date("2025-02-20"),
              type = "Date")
  result <- fput_all(c("A", "B"), fmt)
  expect_type(result, "list")
  expect_s3_class(result[[1]], "Date")
  expect_equal(result[[1]], as.Date("2025-01-15"))
})

# --- Date range matching ---

test_that("fput matches Date ranges", {
  fmt <- fnew(
    "2020-01-01,2025-12-31,TRUE,FALSE" = as.Date("2022-06-15"),
    "specific" = as.Date("2030-01-01"),
    type = "Date"
  )
  # Input date "2022-07-01" falls in the range [2020-01-01, 2025-12-31)
  result <- fput(c("specific", "2022-07-01"), fmt)
  expect_s3_class(result, "Date")
  expect_equal(result[1], as.Date("2030-01-01"))
  expect_equal(result[2], as.Date("2022-06-15"))
})

# --- ignore_case with value types ---

test_that("fput with ignore_case for Date format", {
  fmt <- fnew("a" = as.Date("2025-01-15"), type = "Date", ignore_case = TRUE)
  result <- fput(c("A", "a"), fmt)
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date(c("2025-01-15", "2025-01-15")))
})

# --- print for value types ---

test_that("print.ks_format works for Date format", {
  fmt <- fnew("A" = as.Date("2025-01-15"), "B" = as.Date("2025-02-20"),
              type = "Date")
  output <- capture.output(print(fmt))
  expect_true(any(grepl("Date", output)))
  expect_true(any(grepl("2025-01-15", output)))
})

test_that("print.ks_format works for logical format", {
  fmt <- fnew("Y" = TRUE, "N" = FALSE, type = "logical")
  output <- capture.output(print(fmt))
  expect_true(any(grepl("logical", output)))
  expect_true(any(grepl("TRUE", output)))
})

# --- fparse with value types ---

test_that("fparse creates Date format from text", {
  fparse(text = '
  VALUE dtest (Date, format: %Y-%m-%d)
    "A" = "2025-01-15"
    "B" = "2025-02-20"
  ;
  ')
  fmt <- format_get("dtest")
  expect_equal(fmt$type, "Date")
  expect_s3_class(fmt$mappings[["A"]], "Date")
  expect_equal(fmt$mappings[["A"]], as.Date("2025-01-15"))
  expect_equal(fmt$date_format, "%Y-%m-%d")

  # Apply
  result <- fput(c("A", "B"), "dtest")
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date(c("2025-01-15", "2025-02-20")))
  fclear()
})

test_that("fparse creates logical format from text", {
  fparse(text = '
  VALUE bool_test (logical)
    "Y" = "TRUE"
    "N" = "FALSE"
  ;
  ')
  fmt <- format_get("bool_test")
  expect_equal(fmt$type, "logical")
  expect_true(fmt$mappings[["Y"]])
  expect_false(fmt$mappings[["N"]])
  fclear()
})

# --- fexport roundtrip with value types ---

test_that("fexport/fparse roundtrip for Date format", {
  fmt_orig <- fnew("A" = as.Date("2025-01-15"), "B" = as.Date("2025-02-20"),
                   name = "dt_rt", type = "Date")
  txt <- fexport(dt_rt = fmt_orig)
  fclear()

  fparse(text = txt)
  fmt <- format_get("dt_rt")
  expect_equal(fmt$type, "Date")
  expect_equal(fmt$mappings[["A"]], as.Date("2025-01-15"))
  expect_equal(fmt$mappings[["B"]], as.Date("2025-02-20"))
  fclear()
})

# ============================================================================
# Range table fast path (findInterval-based numeric range matching)
# ============================================================================

test_that("range_table is attached at format creation (fparse)", {
  fparse(text = '
  VALUE age_rt (numeric)
    [0, 18)    = "Child"
    [18, 65)   = "Adult"
    [65, HIGH] = "Senior"
  ;
  ')
  fmt <- format_get("age_rt")
  rt <- fmt$range_table
  expect_false(is.null(rt))
  expect_equal(rt$low, c(0, 18, 65))
  expect_equal(rt$high, c(18, 65, Inf))
  expect_true(all(rt$inc_low))
  expect_equal(rt$inc_high, c(FALSE, FALSE, TRUE))
  expect_true(rt$sorted_disjoint)
  fclear()
})

test_that("range_table is attached by fparse for VALUE blocks", {
  fparse(text = '
  VALUE age_p (numeric)
    [0, 18)    = "Child"
    [18, 65)   = "Adult"
    [65, HIGH] = "Senior"
  ;
  ')
  fmt <- format_get("age_p")
  expect_false(is.null(fmt$range_table))
  expect_true(fmt$range_table$sorted_disjoint)
  fclear()
})

test_that("fput numeric range fast path matches generic path", {
  fparse(text = '
  VALUE age_fp (numeric)
    [0, 18)    = "Child"
    [18, 65)   = "Adult"
    [65, HIGH] = "Senior"
    .missing   = "Unknown"
  ;
  ')
  vals <- c(-1, 0, 5, 17, 18, 30, 64, 65, 100, NA)
  out <- fput(vals, "age_fp")
  expect_equal(out, c("-1", "Child", "Child", "Child", "Adult",
                      "Adult", "Adult", "Senior", "Senior", "Unknown"))
  fclear()
})

test_that("fput respects gaps between ranges (fast path)", {
  fparse(text = '
  VALUE gap_rt (numeric)
    [0, 10)  = "Low"
    [20, 30) = "High"
  ;
  ')
  fmt <- format_get("gap_rt")
  expect_true(fmt$range_table$sorted_disjoint)
  out <- fput(c(5, 15, 25, 10, 20), "gap_rt")
  # 15 falls in gap; 10 excluded by high of first range — both returned as-is
  expect_equal(out, c("Low", "15", "High", "10", "High"))
  fclear()
})

test_that("fput with overlapping multilabel ranges falls back to general path", {
  fparse(text = '
  VALUE risk_rt (numeric, multilabel)
    [0, 3]  = "Low"
    [0, 7]  = "Monitored"
    (3, 7]  = "Medium"
    (7, 10] = "High"
  ;
  ')
  rt <- format_get("risk_rt")$range_table
  expect_false(rt$sorted_disjoint)
  # fput_all should return all matching labels
  out <- fput_all(c(2, 5, 8), "risk_rt")
  expect_setequal(out[[1]], c("Low", "Monitored"))
  expect_setequal(out[[2]], c("Monitored", "Medium"))
  expect_setequal(out[[3]], "High")
  fclear()
})

test_that("fput with exclusive lower bound on first range uses general path", {
  fparse(text = '
  VALUE excl_rt (numeric)
    (0, 10)  = "Mid"
    [10, 20) = "High"
  ;
  ')
  out <- fput(c(0, 5, 10, 15), "excl_rt")
  # 0 has inc_low=FALSE on first range → no match → returned as-is
  expect_equal(out, c("0", "Mid", "High", "High"))
  fclear()
})

test_that("fput with point range [5,5] matches single value", {
  fparse(text = '
  VALUE pt_rt (numeric)
    [5, 5]   = "Exactly5"
    [10, 20) = "Teens"
  ;
  ')
  out <- fput(c(4, 5, 6, 15), "pt_rt")
  expect_equal(out, c("4", "Exactly5", "6", "Teens"))
  fclear()
})

test_that("fput numeric ranges work without cached range_table (fallback)", {
  fparse(text = '
  VALUE fb_rt (numeric)
    [0, 10)  = "Low"
    [10, 20) = "High"
  ;
  ')
  fmt <- format_get("fb_rt")
  # Strip the cached table to exercise the fallback path
  fmt$range_table <- NULL
  out <- fput(c(5, 15, 25), fmt)
  expect_equal(out[1:2], c("Low", "High"))
  fclear()
})

test_that("range_table sorts entries so fast path triggers on out-of-order ranges", {
  # Define ranges in reverse order: still disjoint, should be canonicalised
  fparse(text = '
  VALUE rev_rt (numeric)
    [65, HIGH] = "Senior"
    [18, 65)   = "Adult"
    [0, 18)    = "Child"
  ;
  ')
  fmt <- format_get("rev_rt")
  rt <- fmt$range_table
  expect_true(rt$sorted_disjoint)
  expect_equal(rt$low, c(0, 18, 65))
  expect_equal(rt$label, c("Child", "Adult", "Senior"))
  out <- fput(c(5, 30, 70), "rev_rt")
  expect_equal(out, c("Child", "Adult", "Senior"))
  fclear()
})


# ============================================================================
# Date range / datetime range bucketing (date_range, datetime_range)
# ============================================================================

context("Date Range Bucketing")

test_that("fnew creates a date_range format and buckets Date input", {
  fmt <- fnew(
    "2024-01-01,2025-01-01,TRUE,FALSE" = "FY24",
    "2025-01-01,2026-01-01,TRUE,FALSE" = "FY25",
    type = "date_range"
  )
  expect_equal(fmt$type, "date_range")
  expect_true(fmt$range_table$sorted_disjoint)
  out <- fput(as.Date(c("2024-06-15", "2025-03-01", "2023-12-31")), fmt)
  expect_equal(out, c("FY24", "FY25", "2023-12-31"))
})

test_that("date_range accepts POSIXct input by truncating to date", {
  fmt <- fnew(
    "2024-01-01,2025-01-01,TRUE,FALSE" = "Y24",
    type = "date_range"
  )
  x <- as.POSIXct(c("2024-06-15 10:30:00", "2025-02-01 00:00:00"), tz = "UTC")
  out <- fput(x, fmt)
  expect_equal(out, c("Y24", "2025-02-01"))
})

test_that("date_range supports LOW / HIGH bounds via fparse", {
  fparse(text = '
VALUE era (date_range)
  [LOW, 2000-01-01)        = "Past"
  [2000-01-01, 2025-01-01) = "Present"
  [2025-01-01, HIGH]       = "Future"
;
')
  out <- fput(as.Date(c("1990-01-01", "2010-06-15", "2030-01-01")), "era")
  expect_equal(out, c("Past", "Present", "Future"))
  fclear()
})

test_that("date_range exclusive lower bound (parens) works", {
  fparse(text = '
VALUE strict (date_range)
  (2024-01-01, 2025-01-01) = "Y24"
;
')
  out <- fput(as.Date(c("2024-01-01", "2024-06-01", "2025-01-01")), "strict")
  expect_equal(out, c("2024-01-01", "Y24", "2025-01-01"))
  fclear()
})

test_that("date_range honors .missing and .other", {
  fparse(text = '
VALUE bucket (date_range)
  [2024-01-01, 2025-01-01) = "Y24"
  .missing = "MISS"
  .other   = "OTHER"
;
')
  out <- fput(as.Date(c("2024-06-15", NA, "2030-01-01")), "bucket")
  expect_equal(out, c("Y24", "MISS", "OTHER"))
  fclear()
})

test_that("fput_all returns all matching labels for overlapping date_range", {
  fparse(text = '
VALUE risk (date_range, multilabel)
  [2024-01-01, 2024-07-01) = "H1"
  [2024-04-01, 2024-10-01) = "Mid"
  [2024-07-01, 2025-01-01) = "H2"
;
')
  out <- fput_all(as.Date(c("2024-02-01", "2024-05-15", "2024-08-15")), "risk")
  expect_setequal(out[[1]], "H1")
  expect_setequal(out[[2]], c("H1", "Mid"))
  expect_setequal(out[[3]], c("Mid", "H2"))
  fclear()
})

test_that("auto-detect picks date_range when bounds are ISO dates", {
  fparse(text = '
VALUE auto_dr
  [2024-01-01, 2025-01-01) = "Y24"
;
')
  expect_equal(format_get("auto_dr")$type, "date_range")
  fclear()
})

test_that("fexport/fparse roundtrip for date_range", {
  fparse(text = '
VALUE fyq (date_range)
  [2024-01-01, 2024-04-01) = "Q1-24"
  [2024-04-01, 2024-07-01) = "Q2-24"
;
')
  fmt <- format_get("fyq")
  txt <- fexport(fmt)
  expect_true(grepl("date_range", txt))
  expect_true(grepl("\\[2024-01-01, 2024-04-01\\)", txt))
  fclear()
  fparse(text = txt)
  out <- fput(as.Date(c("2024-02-15", "2024-05-15")), "fyq")
  expect_equal(out, c("Q1-24", "Q2-24"))
  fclear()
})

context("Datetime Range Bucketing")

test_that("datetime_range buckets POSIXct (UTC) input", {
  fparse(text = '
VALUE shift (datetime_range)
  [2024-01-01 00:00:00, 2024-01-01 08:00:00) = "Night"
  [2024-01-01 08:00:00, 2024-01-01 16:00:00) = "Day"
  [2024-01-01 16:00:00, 2024-01-02 00:00:00) = "Evening"
;
')
  y <- as.POSIXct(c("2024-01-01 03:00:00",
                    "2024-01-01 12:00:00",
                    "2024-01-01 20:00:00"), tz = "UTC")
  expect_equal(fput(y, "shift"), c("Night", "Day", "Evening"))
  fclear()
})

test_that("auto-detect picks datetime_range when bounds include time", {
  fparse(text = '
VALUE auto_dtr
  [2024-01-01 00:00:00, 2024-01-01 12:00:00) = "AM"
;
')
  expect_equal(format_get("auto_dtr")$type, "datetime_range")
  fclear()
})

test_that("date_range fast path engages for sorted disjoint bounds", {
  fparse(text = '
VALUE fast (date_range)
  [2024-01-01, 2024-04-01) = "Q1"
  [2024-04-01, 2024-07-01) = "Q2"
  [2024-07-01, 2024-10-01) = "Q3"
  [2024-10-01, 2025-01-01) = "Q4"
;
')
  rt <- format_get("fast")$range_table
  expect_true(rt$sorted_disjoint)
  expect_equal(length(rt$low), 4L)
  fclear()
})


# ===========================================================================
# Stratified Range Builders
# ===========================================================================

context("Stratified Range Builders")

test_that("fmap_ranges builds canonical numeric keys", {
  r <- fmap_ranges(c(0, 7, 14), c(7, 14, 30),
                   c("Wk0", "Wk1", "Wk2"))
  expect_s3_class(r, "ks_fmap")
  expect_equal(names(r), c("0,7,TRUE,FALSE", "7,14,TRUE,FALSE",
                            "14,30,TRUE,FALSE"))
  expect_equal(unname(unclass(r)), c("Wk0", "Wk1", "Wk2"))
})

test_that("fmap_ranges supports Date and POSIXct bounds (ISO format)", {
  d <- fmap_ranges(
    low   = as.Date(c("2024-01-01", "2024-02-01")),
    high  = as.Date(c("2024-02-01", "2024-03-01")),
    label = c("Jan", "Feb")
  )
  expect_equal(names(d), c("2024-01-01,2024-02-01,TRUE,FALSE",
                            "2024-02-01,2024-03-01,TRUE,FALSE"))

  p <- fmap_ranges(
    low   = as.POSIXct(c("2024-01-01 00:00:00"), tz = "UTC"),
    high  = as.POSIXct(c("2024-01-02 00:00:00"), tz = "UTC"),
    label = "Day1"
  )
  expect_true(grepl("^2024-01-01 00:00:00,2024-01-02 00:00:00,TRUE,FALSE$",
                    names(p)))
})

test_that("fmap_ranges validates length and ordering", {
  expect_error(fmap_ranges(c(0, 7), c(7), c("A", "B")),
               "same length")
  expect_error(fmap_ranges(c(7), c(0), "X"),
               "<= corresponding")
})

test_that("fmap_ranges recycles scalar inclusivity", {
  r <- fmap_ranges(c(0, 7), c(7, 14), c("A", "B"), inc_high = TRUE)
  expect_equal(names(r), c("0,7,TRUE,TRUE", "7,14,TRUE,TRUE"))
})

test_that("fmap_strata prefixes stratum and carries strata_sep attribute", {
  s <- fmap_strata(c("A", "A", "B"), c(0, 7, 0), c(7, 14, 7),
                   c("V1", "V2", "V1"), sep = "/")
  expect_s3_class(s, "ks_fmap")
  expect_equal(attr(s, "strata_sep"), "/")
  expect_equal(names(s), c("A/0,7,TRUE,FALSE", "A/7,14,TRUE,FALSE",
                            "B/0,7,TRUE,FALSE"))
})


# ===========================================================================
# Stratified Range Lookup
# ===========================================================================

context("Stratified Range Lookup")

test_that("fnew builds per-stratum range tables", {
  fclear()
  fmt <- fnew(
    fmap_strata(c("A", "A", "B"), c(0, 7, 0), c(7, 14, 10),
                c("V1", "V2", "V1")),
    type = "stratified_range", name = "vw"
  )
  expect_equal(fmt$type, "stratified_range")
  expect_equal(fmt$range_subtype, "numeric")
  expect_equal(names(fmt$range_tables), c("A", "B"))
  expect_true(fmt$range_tables$A$sorted_disjoint)
  expect_equal(length(fmt$range_tables$A$low), 2L)
  fclear()
})

test_that("fputk applies stratified format with numeric values", {
  fclear()
  fnew(
    fmap_strata(c("A", "A", "B"), c(0, 7, 0), c(7, 14, 10),
                c("V1", "V2", "V1")),
    type = "stratified_range", name = "vw", default = "OOR"
  )
  res <- fputk(c("A", "A", "B", "B", "C"), c(3, 10, 5, 99, 1),
               format = "vw")
  expect_equal(res, c("V1", "V2", "V1", "OOR", "OOR"))
  fclear()
})

test_that("fputk applies stratified format with date subtype", {
  fclear()
  s <- fmap_strata(
    stratum = c("S1", "S1", "S2"),
    low     = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    high    = as.Date(c("2024-02-01", "2024-03-01", "2024-04-01")),
    label   = c("Jan", "Feb", "Mar")
  )
  fnew(s, type = "stratified_range", range_subtype = "date", name = "win")
  v <- as.Date(c("2024-01-15", "2024-02-15", "2024-03-15", "2024-04-15"))
  res <- fputk(c("S1", "S1", "S2", "S2"), v, format = "win")
  expect_equal(res, c("Jan", "Feb", "Mar", "2024-04-15"))
  fclear()
})

test_that("per-stratum .missing and .other override globals", {
  fclear()
  fnew(
    fmap_strata(c("A", "A", "B"), c(0, 7, 0), c(7, 14, 7),
                c("V1", "V2", "V1")),
    ".other|A" = "A_OOR",
    ".missing|A" = "A_MISS",
    .other = "GLOBAL_OOR",
    .missing = "GLOBAL_MISS",
    type = "stratified_range", name = "vw"
  )
  expect_equal(
    fputk(c("A", "A", "A", "B", "C"), c(3, 99, NA, 99, 99), format = "vw"),
    c("V1", "A_OOR", "A_MISS", "GLOBAL_OOR", "GLOBAL_OOR")
  )
  fclear()
})

test_that("fput() refuses stratified formats and fputk requires 2+ args", {
  fclear()
  fnew(
    fmap_strata("A", 0, 7, "V1"),
    type = "stratified_range", name = "vw"
  )
  expect_error(fput("foo", "vw"), "fputk")
  expect_error(fputk(c("A"), format = "vw"),
               "at least 2 arguments")
  fclear()
})

test_that("fparse parses stratified_range with friendly intervals", {
  fclear()
  txt <- '
VALUE visits (stratified_range, range_subtype: numeric)
  "ARM_A"|[0, 7)  = "Wk1"
  "ARM_A"|[7, 14) = "Wk2"
  "ARM_B"|[0, 10) = "Baseline"
  ".other|ARM_A"  = "A_OOR"
  .other          = "GLOBAL_OOR"
  ;
'
  fparse(text = txt)
  fmt <- format_get("visits")
  expect_equal(fmt$type, "stratified_range")
  expect_equal(fmt$range_subtype, "numeric")
  expect_equal(names(fmt$other_by_stratum), "ARM_A")
  expect_equal(fmt$other_label, "GLOBAL_OOR")
  expect_equal(
    fputk(c("ARM_A", "ARM_A", "ARM_B", "ARM_C"),
          c(3, 99, 5, 1), format = "visits"),
    c("Wk1", "A_OOR", "Baseline", "GLOBAL_OOR")
  )
  fclear()
})

test_that("fparse handles custom strata_sep and date subtype", {
  fclear()
  txt <- '
VALUE dwin (stratified_range, range_subtype: date, strata_sep: /)
  "S1"/[2024-01-01, 2024-02-01) = "Jan"
  "S1"/[2024-02-01, 2024-03-01) = "Feb"
  "S2"/[2024-03-01, 2024-04-01) = "Mar"
  ;
'
  fparse(text = txt)
  fmt <- format_get("dwin")
  expect_equal(fmt$strata_sep, "/")
  expect_equal(fmt$range_subtype, "date")
  res <- fputk(
    c("S1", "S1", "S2"),
    as.Date(c("2024-01-15", "2024-02-15", "2024-03-15")),
    format = "dwin"
  )
  expect_equal(res, c("Jan", "Feb", "Mar"))
  fclear()
})

test_that("fexport / fparse roundtrip preserves stratified format", {
  fclear()
  fmt <- fnew(
    fmap_strata(c("A", "A", "B"), c(0, 7, 0), c(7, 14, 7),
                c("V1", "V2", "V1")),
    ".other|A" = "A_OOR",
    .other = "GLOBAL_OOR",
    type = "stratified_range", name = "vw"
  )
  txt <- fexport(fmt)
  fclear()
  fparse(text = txt)
  fmt2 <- format_get("vw")
  expect_equal(fmt2$mappings, fmt$mappings)
  expect_equal(fmt2$range_subtype, fmt$range_subtype)
  expect_equal(fmt2$strata_sep, fmt$strata_sep)
  expect_equal(fmt2$other_by_stratum, fmt$other_by_stratum)
  expect_equal(fmt2$other_label, fmt$other_label)
  fclear()
})

test_that("print.ks_format renders stratified groups with headers", {
  fclear()
  fmt <- fnew(
    fmap_strata(c("A", "A", "B"), c(0, 7, 0), c(7, 14, 7),
                c("V1", "V2", "V1")),
    type = "stratified_range", name = "vw"
  )
  out <- capture.output(print(fmt))
  expect_true(any(grepl('Stratum "A":', out)))
  expect_true(any(grepl('Stratum "B":', out)))
  expect_true(any(grepl("\\[0, 7\\) => V1", out)))
  fclear()
})

test_that("fmap_strata strata_sep is auto-picked up by fnew", {
  fclear()
  s <- fmap_strata(c("A", "B"), c(0, 0), c(7, 7), c("V1", "V1"),
                   sep = ":")
  fmt <- fnew(s, type = "stratified_range", name = "vw")
  expect_equal(fmt$strata_sep, ":")
  expect_equal(fputk(c("A", "B"), c(3, 3), format = "vw"),
               c("V1", "V1"))
  fclear()
})
