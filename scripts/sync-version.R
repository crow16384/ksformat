#!/usr/bin/env Rscript
# Sync package version from DESCRIPTION into the files that must always carry
# the CURRENT version, then audit the repo for stale references.
#
#   Rscript scripts/sync-version.R           # sync carriers from DESCRIPTION
#   Rscript scripts/sync-version.R --audit   # report stale refs, make no edits
#
# Version bump workflow:
#   1. edit Version: in DESCRIPTION
#   2. add a NEW top section to NEWS.md by hand (never rewritten mechanically;
#      old sections are history)
#   3. Rscript scripts/sync-version.R        # rewrites cran-comments.md
#      ("Package: ksformat X (from Y)": Y is the previous release and stays)
#   4. rebuild the pkgdown site (docs/ chrome carries the version)
#   5. R CMD build && R CMD check
#
# Audit: reads the PREVIOUS version from git (HEAD:DESCRIPTION, falling back to
# the second NEWS.md section) and greps the working tree for it, excluding
# generated/build artifacts (docs/, doc/, Meta/, *.Rcheck, tarballs, .git) and
# intentional history (NEWS.md, and cran-comments.md lines about older
# releases). Everything else still naming the previous version is STALE.

args <- commandArgs(trailingOnly = TRUE)
audit_only <- "--audit" %in% args

pkg_root <- if (file.exists("DESCRIPTION")) "." else {
  d <- Sys.getenv("R_PACKAGE_DIR", ".")
  if (!file.exists(file.path(d, "DESCRIPTION")))
    stop("Run from package root or set R_PACKAGE_DIR")
  d
}
desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
version <- desc[1L, "Version"]
if (is.na(version) || !nzchar(version)) stop("No Version field in DESCRIPTION")

# --- previous version: git HEAD DESCRIPTION, else NEWS.md second section ------
prev <- {
  hd <- tryCatch(suppressWarnings(system2("git",
    c("-C", shQuote(pkg_root), "show", "HEAD:DESCRIPTION"),
    stdout = TRUE, stderr = TRUE)), error = function(e) NULL)
  ok <- !is.null(hd) && !any(grepl("^fatal:", hd)) && any(grepl("^Version:", hd))
  if (ok) {
    v <- sub("^Version:[[:space:]]*", "", grep("^Version:", hd, value = TRUE)[1])
    if (nzchar(v) && v != version) v else NA_character_
  } else NA_character_
}
if (is.na(prev)) {
  nh <- file.path(pkg_root, "NEWS.md")
  if (file.exists(nh)) {
    heads <- grep("^# ksformat [0-9.]+", readLines(nh, warn = FALSE), value = TRUE)
    if (length(heads) >= 2) prev <- sub("^# ksformat ", "", heads[2])
  }
}

# --- sync carriers ------------------------------------------------------------
if (!audit_only) {
  cc_path <- file.path(pkg_root, "cran-comments.md")
  if (file.exists(cc_path)) {
    txt <- readLines(cc_path, encoding = "UTF-8", warn = FALSE)
    # ONLY the current-version slots; historical lines are never touched:
    #   header "## Package: ksformat X.Y.Z ..."
    txt <- sub("^(## Package: ksformat )[0-9.]+", paste0("\\1", version), txt)
    #   check-result references "ksformat_X.Y.Z.tar.gz"
    txt <- gsub("ksformat_[0-9]+\\.[0-9]+\\.[0-9.]+\\.tar\\.gz",
                paste0("ksformat_", version, ".tar.gz"), txt)
    writeLines(txt, cc_path)
    message("Updated ", cc_path, " to version ", version)
  } else {
    message("cran-comments.md not found, skipping")
  }

  # NEWS.md guard: the top section must announce the current version
  nh <- file.path(pkg_root, "NEWS.md")
  if (file.exists(nh)) {
    first <- grep("^# ", readLines(nh, n = 5, warn = FALSE), value = TRUE)[1]
    if (is.na(first) || !grepl(paste0("^# ksformat ", version, "$"), first))
      warning("NEWS.md top section is not '# ksformat ", version,
              "' — add the new section by hand (see workflow in this script).",
              call. = FALSE)
  }
}

# --- audit: stale references to the previous version --------------------------
stale <- character(0)
if (!is.na(prev)) {
  hits <- suppressWarnings(system2("grep",
    c("-rIn", shQuote(prev), ".",
      "--exclude-dir=docs", "--exclude-dir=doc", "--exclude-dir=Meta",
      "--exclude-dir=.git", "--exclude-dir=.Rproj.user",
      "--exclude=*.tar.gz", "--exclude=*.log",
      sprintf("--exclude-dir=%s", "*.Rcheck")),
    stdout = TRUE, stderr = FALSE))
  if (is.null(hits)) hits <- character(0)
  # intentional history: NEWS.md sections; cran-comments lines referencing
  # older releases ("(from X)", "from X to Y", "Version X —")
  keep <- grepl("^\\./NEWS\\.md:", hits) |
    grepl("^\\./cran-comments\\.md:.*([Ff]rom|Version) ", hits)
  stale <- hits[!keep]
  cat("Audit against previous version ", prev, ":\n", sep = "")
  if (length(stale)) {
    cat(paste(stale, collapse = "\n"), "\n")
    cat("=> STALE references found; fix them before releasing.\n")
  } else {
    cat("  (no stale references outside historical sections)\n")
  }
} else {
  cat("Audit skipped: previous version not determined.\n")
}

message("Version ", version, if (audit_only) " audited." else " synced.")
quit(status = if (length(stale) > 0) 1L else 0L)
