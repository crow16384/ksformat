test_that(".format_library_snapshot returns empty schema when library is empty", {
  fclear()

  snap <- ksformat:::.format_library_snapshot()
  expect_s3_class(snap, "data.frame")
  expect_equal(nrow(snap), 0L)
  expect_equal(
    names(snap),
    c("name", "object_kind", "type", "mappings", "flags", "created")
  )
})

test_that(".format_library_snapshot includes VALUE and INVALUE objects", {
  on.exit(fclear(), add = TRUE)
  fclear()

  fnew("M" = "Male", "F" = "Female", name = "sex")
  finput("Male" = 1, "Female" = 2, name = "sex_inv")

  snap <- ksformat:::.format_library_snapshot()

  expect_true(all(c("sex", "sex_inv") %in% snap$name))

  sex_row <- snap[snap$name == "sex", , drop = FALSE]
  expect_equal(sex_row$object_kind, "VALUE")
  expect_equal(sex_row$type, "character")
  expect_equal(sex_row$mappings, 2L)

  inv_row <- snap[snap$name == "sex_inv", , drop = FALSE]
  expect_equal(inv_row$object_kind, "INVALUE")
  expect_equal(inv_row$type, "numeric")
  expect_equal(inv_row$mappings, 2L)
})

test_that("entry detail and mapping table helpers return expected content", {
  on.exit(fclear(), add = TRUE)
  fclear()

  fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")

  details <- ksformat:::.format_library_entry_details("sex")
  expect_equal(details$Name, "sex")
  expect_equal(details$Kind, "VALUE")
  expect_equal(details$Mappings, 2L)

  mapping_tbl <- ksformat:::.format_library_mapping_table("sex")
  expect_s3_class(mapping_tbl, "data.frame")
  expect_true(all(c("Rule", "Value", "Category") %in% names(mapping_tbl)))
  expect_true(any(mapping_tbl$Rule == "M" & mapping_tbl$Value == "Male"))
  expect_true(any(mapping_tbl$Rule == ".missing" & mapping_tbl$Value == "Unknown"))
})

test_that(".format_library_snapshot reflects remove and clear operations", {
  on.exit(fclear(), add = TRUE)
  fclear()

  fnew("M" = "Male", name = "sex")
  finput("Male" = 1, name = "sex_inv")

  before <- ksformat:::.format_library_snapshot()
  expect_true("sex" %in% before$name)
  expect_true("sex_inv" %in% before$name)

  fclear("sex")
  after_remove <- ksformat:::.format_library_snapshot()
  expect_false("sex" %in% after_remove$name)
  expect_true("sex_inv" %in% after_remove$name)

  fclear()
  after_clear <- ksformat:::.format_library_snapshot()
  expect_equal(nrow(after_clear), 0L)
})

test_that("format_library_app fails fast when shiny is unavailable", {
  testthat::local_mocked_bindings(
    .has_shiny = function() FALSE,
    .package = "ksformat"
  )

  expect_error(
    ksformat::format_library_app(),
    "requires the.*shiny"
  )
})

test_that(".format_library_shiny_app returns a shiny app object", {
  testthat::skip_if_not_installed("shiny")

  app <- ksformat:::.format_library_shiny_app()
  expect_true(inherits(app, "shiny.appobj"))
})
