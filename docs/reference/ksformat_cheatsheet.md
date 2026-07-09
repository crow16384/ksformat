# Open the ksformat cheat sheet

Opens the package cheat sheet in the default browser (HTML) or viewer
(PDF). The files are installed under
`system.file("doc", ..., package = "ksformat")`.

## Usage

``` r
ksformat_cheatsheet(format = c("html", "pdf"))
```

## Arguments

- format:

  Character: `"html"` (default) or `"pdf"`. Which version to open.

## Value

Invisibly, the path to the opened file. If the file is not found, an
error is thrown.

## Examples

``` r
if (FALSE) { # \dontrun{
ksformat_cheatsheet()           # open HTML in browser
ksformat_cheatsheet("pdf")      # open PDF
} # }
```
