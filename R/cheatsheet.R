#' Open the ksformat cheat sheet
#'
#' Opens the package cheat sheet in the default browser (HTML) or viewer (PDF).
#' The files are installed under \code{system.file("doc", ..., package = "ksformat")}.
#'
#' @param format Character: \code{"html"} (default) or \code{"pdf"}. Which version to open.
#' @return Invisibly, the path to the opened file. If the file is not found, an error is thrown.
#' @export
#' @examples
#' \donttest{
#' ksformat_cheatsheet()           # open HTML in browser
#' ksformat_cheatsheet("pdf")      # open PDF
#' }
ksformat_cheatsheet <- function(format = c("html", "pdf")) {
  format <- match.arg(format)
  basename <- if (format == "html") "ksformat-cheatsheet.html" else "ksformat-Cheat-Sheet.pdf"
  path <- system.file("doc", basename, package = "ksformat")
  if (!nzchar(path)) {
    cli::cli_abort("Cheat sheet not found: {basename}. Reinstall the package.")
  }
  if (format == "html") {
    utils::browseURL(path)
  } else {
    utils::browseURL(path)
  }
  invisible(path)
}
