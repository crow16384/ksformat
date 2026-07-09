# Launch Shiny Browser for Format Library

Opens an interactive Shiny app for browsing and managing objects
currently registered in the global ksformat format library.

## Usage

``` r
format_library_app(port = getOption("shiny.port"), launch.browser = TRUE)
```

## Arguments

- port:

  Integer or NULL. Port passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
  Default: `getOption("shiny.port")`.

- launch.browser:

  Logical. Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
  Default: `TRUE`.

## Value

Invisibly returns `NULL`.

## Details

The app displays both `ks_format` (VALUE) and `ks_invalue` (INVALUE)
objects, supports filtering and name search, shows object details with a
formatted mapping table, and provides management actions to remove one
object, clear the full library, or quit the app.

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
  fnew("M" = "Male", "F" = "Female", name = "sex")
  finput("Male" = 1, "Female" = 2, name = "sex_inv")
  format_library_app()
}
} # }
```
