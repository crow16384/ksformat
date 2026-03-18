#' ksformat: SAS-Style PROC FORMAT for R
#'
#' @description
#' Provides SAS PROC FORMAT-like functionality for creating and applying
#' value formats in R. The package supports mapping values to labels,
#' range-based formatting, reverse formatting (invalue), and proper handling
#' of missing values (NA, NULL, NaN).
#'
#' @details
#' Main features:
#' \itemize{
#'   \item Create value-to-label mappings (formats) with \code{\link{fnew}}
#'   \item Apply formats to vectors using \code{\link{fput}}
#'   \item Apply formats by name: \code{\link{fputn}} (numeric), \code{\link{fputc}} (character)
#'   \item Reverse formatting with \code{\link{finput}}, \code{\link{finputn}}, \code{\link{finputc}}
#'   \item Apply invalues by name: \code{\link{finputn}} (numeric), \code{\link{finputc}} (character)
#'   \item Parse SAS-like format definitions from text with \code{\link{fparse}}
#'   \item Global format library with auto-registration
#'   \item Range-based formatting for numeric data
#' }
#'
#' Cheat sheet: run \code{\link{ksformat_cheatsheet}()} to open the HTML version in your browser, or see the files in \code{system.file("doc", package = "ksformat")}.
#'
#' @examples
#' fnew("M" = "Male", "F" = "Female", .missing = "Unknown", name = "sex")
#' fput(c("M", "F", NA), "sex")
#'
#' fparse(text = '
#' VALUE age (numeric)
#'   [0, 18) = "Child"
#'   [18, 65) = "Adult"
#' ;
#' ')
#' fputn(c(5, 25), "age")
#' fclear()
#'
#' @docType package
#' @name ksformat-package
#' @aliases ksformat
#' @keywords package
#' @importFrom cli cli_abort cli_warn cli_inform
"_PACKAGE"
