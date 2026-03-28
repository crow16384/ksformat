#' @noRd
.find_cheatsheet_path <- function(format) {
  basenames <- switch(
    format,
    html = "ksformat-cheatsheet.html",
    pdf = c("ksformat-cheatsheet.pdf", "ksformat-Cheat-Sheet.pdf")
  )

  for (basename in basenames) {
    path <- system.file("doc", basename, package = "ksformat")
    if (nzchar(path)) {
      return(path)
    }
  }

  ""
}

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
  path <- .find_cheatsheet_path(format)
  if (!nzchar(path)) {
    cli::cli_abort(c(
      "Cheat sheet not found for format {.val {format}}.",
      "i" = "Try reinstalling the package."
    ))
  }
  utils::browseURL(path)
  invisible(path)
}
