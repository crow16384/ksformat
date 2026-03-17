#!/usr/bin/env Rscript
# Sync package version from DESCRIPTION into files that reference it.
# Run after changing Version in DESCRIPTION:
#   Rscript scripts/sync-version.R
# Or from package root: source("scripts/sync-version.R")

pkg_root <- if (file.exists("DESCRIPTION")) "." else {
  d <- Sys.getenv("R_PACKAGE_DIR", ".")
  if (!file.exists(file.path(d, "DESCRIPTION"))) stop("Run from package root or set R_PACKAGE_DIR")
  d
}
desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
version <- desc[1L, "Version"]
if (is.na(version)) stop("No Version field in DESCRIPTION")

# cran-comments.md: "Package: ksformat X.Y.Z" and "ksformat_X.Y.Z.tar.gz"
cc_path <- file.path(pkg_root, "cran-comments.md")
if (file.exists(cc_path)) {
  txt <- readLines(cc_path, encoding = "UTF-8")
  txt <- gsub("ksformat [0-9]+[.][0-9]+[.][0-9]+", paste0("ksformat ", version), txt)
  txt <- gsub("ksformat_[0-9]+[.][0-9]+[.][0-9]+[.]tar[.]gz", paste0("ksformat_", version, ".tar.gz"), txt)
  writeLines(txt, cc_path)
  message("Updated ", cc_path, " to version ", version)
} else {
  message("cran-comments.md not found, skipping")
}

message("Version ", version, " synced.")
