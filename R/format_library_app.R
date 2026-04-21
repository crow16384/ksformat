#' Build Format Library Snapshot (Internal)
#'
#' Converts the current global format library state into a tabular data frame
#' used by the Shiny browser UI and tests.
#'
#' @return A data frame with one row per registered object.
#' @keywords internal
#' @noRd
.format_library_snapshot <- function() {
  nms <- flist()

  if (length(nms) == 0L) {
    return(data.frame(
      name = character(0),
      object_kind = character(0),
      type = character(0),
      mappings = integer(0),
      flags = character(0),
      created = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(nms, function(nm) {
    obj <- format_get(nm)

    if (inherits(obj, "ks_format")) {
      flags <- character(0)
      if (isTRUE(obj$multilabel)) flags <- c(flags, "multilabel")
      if (isTRUE(obj$ignore_case)) flags <- c(flags, "nocase")
      if (!is.null(obj$missing_label)) flags <- c(flags, "missing")
      if (!is.null(obj$other_label)) flags <- c(flags, "other")

      data.frame(
        name = nm,
        object_kind = "VALUE",
        type = obj$type,
        mappings = length(obj$mappings),
        flags = if (length(flags) > 0L) paste(flags, collapse = ", ") else "",
        created = as.character(obj$created),
        stringsAsFactors = FALSE
      )
    } else if (inherits(obj, "ks_invalue")) {
      flags <- character(0)
      if (!is.null(obj$missing_value) && !identical(obj$missing_value, NA)) {
        flags <- c(flags, "missing_value")
      }

      data.frame(
        name = nm,
        object_kind = "INVALUE",
        type = obj$target_type,
        mappings = length(obj$mappings),
        flags = if (length(flags) > 0L) paste(flags, collapse = ", ") else "",
        created = as.character(obj$created),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        name = nm,
        object_kind = "UNKNOWN",
        type = NA_character_,
        mappings = NA_integer_,
        flags = "",
        created = "",
        stringsAsFactors = FALSE
      )
    }
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Build Entry Details (Internal)
#'
#' @param name Character scalar. Registered object name.
#' @return Named list with detail fields for UI display.
#' @keywords internal
#' @noRd
.format_library_entry_details <- function(name) {
  obj <- format_get(name)

  if (inherits(obj, "ks_format")) {
    flags <- character(0)
    if (isTRUE(obj$multilabel)) flags <- c(flags, "multilabel")
    if (isTRUE(obj$ignore_case)) flags <- c(flags, "nocase")

    return(list(
      Name = name,
      Kind = "VALUE",
      Type = obj$type,
      Mappings = length(obj$mappings),
      Flags = if (length(flags) > 0L) paste(flags, collapse = ", ") else "none",
      Missing = if (!is.null(obj$missing_label)) as.character(obj$missing_label) else "<none>",
      Other = if (!is.null(obj$other_label)) as.character(obj$other_label) else "<none>",
      Created = as.character(obj$created)
    ))
  }

  if (inherits(obj, "ks_invalue")) {
    return(list(
      Name = name,
      Kind = "INVALUE",
      Target_Type = obj$target_type,
      Mappings = length(obj$mappings),
      Missing_Value = if (!is.null(obj$missing_value)) as.character(obj$missing_value) else "<none>",
      Created = as.character(obj$created)
    ))
  }

  list(
    Name = name,
    Kind = "UNKNOWN"
  )
}

#' Build Entry Mapping Table (Internal)
#'
#' @param name Character scalar. Registered object name.
#' @return A data frame with formatted mapping rows.
#' @keywords internal
#' @noRd
.format_library_mapping_table <- function(name) {
  obj <- format_get(name)

  make_row <- function(rule, value, category = "mapping") {
    data.frame(
      Rule = as.character(rule),
      Value = as.character(value),
      Category = as.character(category),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  out <- data.frame(
    Rule = character(0),
    Value = character(0),
    Category = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (inherits(obj, "ks_format")) {
    if (obj$type %in% c("date", "time", "datetime")) {
      pattern <- obj$dt_pattern
      if (!is.null(obj$sas_name)) {
        pattern <- paste0(pattern, " (", obj$sas_name, ".)")
      }
      out <- rbind(out, make_row("pattern", pattern, "format"))
    } else if (length(obj$mappings) > 0L) {
      rows <- lapply(seq_along(obj$mappings), function(i) {
        key <- names(obj$mappings)[i]
        value <- obj$mappings[[i]]

        parsed <- if (.is_value_type(obj$type) && obj$type %in% c("Date", "POSIXct")) {
          .parse_date_range_key(key, obj$date_format)
        } else {
          .parse_range_key(key)
        }

        rule <- key
        if (!is.null(parsed)) {
          left_bracket <- if (parsed$inc_low) "[" else "("
          right_bracket <- if (parsed$inc_high) "]" else ")"

          low_num <- suppressWarnings(as.numeric(parsed$low))
          high_num <- suppressWarnings(as.numeric(parsed$high))

          low_str <- if (!is.na(low_num) && is.infinite(low_num) && low_num < 0) {
            "LOW"
          } else {
            as.character(parsed$low)
          }

          high_str <- if (!is.na(high_num) && is.infinite(high_num) && high_num > 0) {
            "HIGH"
          } else {
            as.character(parsed$high)
          }

          rule <- paste0(left_bracket, low_str, ", ", high_str, right_bracket)
        }

        value_str <- if (.is_value_type(obj$type)) {
          .typed_value_to_string(value, obj$type, obj$date_format)
        } else {
          as.character(value)
        }

        make_row(rule, value_str, if (is.null(parsed)) "mapping" else "range")
      })

      out <- do.call(rbind, rows)
    }

    if (!is.null(obj$missing_label)) {
      out <- rbind(out, make_row(".missing", obj$missing_label, "special"))
    }

    if (!is.null(obj$other_label)) {
      out <- rbind(out, make_row(".other", obj$other_label, "special"))
    }

    if (nrow(out) == 0L) {
      out <- make_row("<none>", "<no mappings>", "info")
    }

    rownames(out) <- NULL
    return(out)
  }

  if (inherits(obj, "ks_invalue")) {
    if (length(obj$mappings) > 0L) {
      rows <- lapply(seq_along(obj$mappings), function(i) {
        make_row(names(obj$mappings)[i], obj$mappings[[i]], "mapping")
      })
      out <- do.call(rbind, rows)
    }

    if (!is.null(obj$missing_value) && !identical(obj$missing_value, NA)) {
      out <- rbind(out, make_row(".missing input", obj$missing_value, "special"))
    }

    if (nrow(out) == 0L) {
      out <- make_row("<none>", "<no mappings>", "info")
    }

    rownames(out) <- NULL
    return(out)
  }

  make_row("<unknown>", "<unsupported object>", "info")
}

#' Check Shiny Availability (Internal)
#'
#' @return Logical scalar. TRUE when shiny is installed.
#' @keywords internal
#' @noRd
.has_shiny <- function() {
  requireNamespace("shiny", quietly = TRUE)
}

#' Build Shiny App Object for Format Browser (Internal)
#'
#' @return A shiny app object.
#' @keywords internal
#' @noRd
.format_library_shiny_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("ksformat Library Browser"),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::textInput("name_search", "Search by name", value = ""),
          shiny::selectInput(
            "kind_filter",
            "Object kind",
            choices = c("All", "VALUE", "INVALUE"),
            selected = "All"
          ),
          shiny::selectInput("type_filter", "Type", choices = "All", selected = "All"),
          shiny::selectInput("selected_name", "Selected entry", choices = character(0)),
          shiny::hr(),
          shiny::actionButton("remove_selected", "Remove selected", class = "btn-warning"),
          shiny::actionButton("clear_all", "Clear all", class = "btn-danger"),
          shiny::actionButton("quit_app", "Quit", class = "btn-default")
        )
      ),
      shiny::column(
        width = 8,
        shiny::h4("Library entries"),
        shiny::tableOutput("library_table"),
        shiny::h4("Selected entry details"),
        shiny::tableOutput("selected_details"),
        shiny::h4("Formatted mapping table"),
        shiny::tableOutput("selected_mappings")
      )
    )
  )

  server <- function(input, output, session) {
    refresh_tick <- shiny::reactiveVal(0L)

    snapshot <- shiny::reactive({
      refresh_tick()
      .format_library_snapshot()
    })

    shiny::observe({
      dat <- snapshot()
      type_choices <- if (nrow(dat) == 0L) {
        "All"
      } else {
        c("All", sort(unique(dat$type[!is.na(dat$type)])))
      }

      selected_type <- input$type_filter
      if (is.null(selected_type) || !(selected_type %in% type_choices)) {
        selected_type <- "All"
      }

      shiny::updateSelectInput(
        session,
        "type_filter",
        choices = type_choices,
        selected = selected_type
      )
    })

    filtered <- shiny::reactive({
      dat <- snapshot()
      if (nrow(dat) == 0L) {
        return(dat)
      }

      if (!is.null(input$kind_filter) && !identical(input$kind_filter, "All")) {
        dat <- dat[dat$object_kind == input$kind_filter, , drop = FALSE]
      }

      if (!is.null(input$type_filter) && !identical(input$type_filter, "All")) {
        dat <- dat[dat$type == input$type_filter, , drop = FALSE]
      }

      query <- trimws(if (is.null(input$name_search)) "" else input$name_search)
      if (nzchar(query)) {
        dat <- dat[grepl(query, dat$name, fixed = TRUE, ignore.case = TRUE), , drop = FALSE]
      }

      dat
    })

    shiny::observe({
      dat <- filtered()
      selected <- input$selected_name

      if (nrow(dat) == 0L) {
        shiny::updateSelectInput(
          session,
          "selected_name",
          choices = character(0),
          selected = character(0)
        )
        return()
      }

      if (is.null(selected) || !nzchar(selected) || !(selected %in% dat$name)) {
        selected <- dat$name[[1]]
      }

      shiny::updateSelectInput(
        session,
        "selected_name",
        choices = dat$name,
        selected = selected
      )
    })

    selected_name <- shiny::reactive({
      dat <- filtered()
      if (nrow(dat) == 0L) {
        return(NULL)
      }

      nm <- input$selected_name
      if (is.null(nm) || !nzchar(nm) || !(nm %in% dat$name)) {
        return(dat$name[[1]])
      }

      nm
    })

    output$library_table <- shiny::renderTable({
      dat <- filtered()
      if (nrow(dat) == 0L) {
        return(data.frame(
          Message = "Format library is empty or no entries match the current filters.",
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      dat
    }, striped = TRUE, hover = TRUE, rownames = FALSE)

    output$selected_details <- shiny::renderTable({
      nm <- selected_name()
      if (is.null(nm)) {
        return(data.frame(
          Field = "Status",
          Value = "No entry selected",
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      details <- .format_library_entry_details(nm)
      data.frame(
        Field = names(details),
        Value = unname(vapply(details, as.character, character(1L))),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, rownames = FALSE)

    output$selected_mappings <- shiny::renderTable({
      nm <- selected_name()
      if (is.null(nm)) {
        return(data.frame(
          Rule = "Status",
          Value = "No entry selected",
          Category = "info",
          stringsAsFactors = FALSE,
          check.names = FALSE
        ))
      }

      .format_library_mapping_table(nm)
    }, striped = TRUE, hover = TRUE, rownames = FALSE)

    shiny::observeEvent(input$remove_selected, {
      nm <- selected_name()
      shiny::req(!is.null(nm))

      shiny::showModal(shiny::modalDialog(
        title = sprintf("Remove '%s'?", nm),
        "This action removes the selected object from the global format library.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_remove", "Remove", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_remove, {
      nm <- selected_name()
      shiny::removeModal()
      shiny::req(!is.null(nm))

      tryCatch(
        {
          fclear(nm)
          refresh_tick(refresh_tick() + 1L)
          shiny::showNotification(
            sprintf("Removed '%s' from the library.", nm),
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        }
      )
    })

    shiny::observeEvent(input$clear_all, {
      shiny::showModal(shiny::modalDialog(
        title = "Clear entire library?",
        "This action removes all registered formats and invalues.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_clear_all", "Clear all", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_clear_all, {
      shiny::removeModal()

      tryCatch(
        {
          fclear()
          refresh_tick(refresh_tick() + 1L)
          shiny::showNotification("All library entries were removed.", type = "message")
        },
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        }
      )
    })

    shiny::observeEvent(input$quit_app, {
      shiny::showModal(shiny::modalDialog(
        title = "Quit app?",
        "This action closes the format library browser.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_quit", "Quit", class = "btn-default")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_quit, {
      shiny::removeModal()
      shiny::stopApp(invisible(NULL))
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Launch Shiny Browser for Format Library
#'
#' Opens an interactive Shiny app for browsing and managing objects currently
#' registered in the global ksformat format library.
#'
#' The app displays both \code{ks_format} (VALUE) and \code{ks_invalue}
#' (INVALUE) objects, supports filtering and name search, shows object details
#' with a formatted mapping table, and provides management actions to remove one
#' object, clear the full library, or quit the app.
#'
#' @param port Integer or NULL. Port passed to \code{shiny::runApp()}.
#'   Default: \code{getOption("shiny.port")}. 
#' @param launch.browser Logical. Passed to \code{shiny::runApp()}.
#'   Default: \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   fnew("M" = "Male", "F" = "Female", name = "sex")
#'   finput("Male" = 1, "Female" = 2, name = "sex_inv")
#'   format_library_app()
#' }
#' }
format_library_app <- function(port = getOption("shiny.port"), launch.browser = TRUE) {
  if (!.has_shiny()) {
    cli_abort(c(
      "{.fn format_library_app} requires the {.pkg shiny} package.",
      "i" = "Install it with {.code install.packages('shiny')}."
    ))
  }

  shiny::runApp(
    .format_library_shiny_app(),
    port = port,
    launch.browser = launch.browser
  )

  invisible(NULL)
}
