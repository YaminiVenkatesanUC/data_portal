download_data_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("show"), "Download data")
}

download_data_server <- function(input, output, session, download_modal_vars) {
  state <- reactiveValues(
    lower_date = download_modal_vars$min,
    upper_date = download_modal_vars$max,
    selected_range = "full_range"
  )

  downloadModal <- function(failed = FALSE, failedUnique = FALSE) {
    ns <- session$ns

    modalDialog(
      useShinyjs(),
      span(h3(strong(paste(CONFIG$title, 'data download')))),
      selectizeInput(
        inputId = ns("indicator_selector"),
        label = "Select indicators",
        choices = setNames(DOWNLOADABLE_INDICATORS, gsub("_", " - ", DOWNLOADABLE_INDICATORS)),
        selected = "select indicators from the list",
        multiple = TRUE,
        width = '100%',
        options = list(
          placeholder = 'Download all indicators (or click here to search and select)',
          onInitialize = I('function() { this.setValue(""); }'),
          'plugins' = list('remove_button'),
          'persist' = FALSE
        )
      ),
      radioButtons(
        ns("pick_range"),
        "Select a date range",
        c(
          "Last week" = "last_week",
          "Last month" = "last_month",
          "Last year" = "last_year",
          "Full range" = "full_range"
        ),
        selected = "full_range",
        inline = TRUE
      ),
      p(paste("Last updated:", get_most_recent_update_date(DATA_STORE))),
      includeHTML(CONFIG$download_modal_html),
      footer = tagList(
        modalButton("Close"),
        downloadButton(ns("downloadData"), "Download")
      )
    )
  }

  observeEvent(input$show, {
    showModal(downloadModal())
  })

  get_indicator_definitions_filtered <- function(indicators) {
    if (is.null(indicators) || length(indicators) == 0) {
      return(indicator_definitions)
    }
    return(indicator_definitions[names(indicator_definitions) %in% indicators])
  }

  get_data_store_filtered <- function(indicators) {
    if (is.null(indicators) || length(indicators) == 0) {
      return(DATA_STORE)
    }
    re <- paste(indicators, collapse = "|^")
    re <- paste0("^", re)
    re <- gsub("\\(", "\\\\(", re)
    re <- gsub(")", "\\\\)", re)
    result <- DATA_STORE[names(DATA_STORE) %like% re]
    return(result)
  }

  get_date_range <- reactive({
    return(input$pick_range)
  })

  observe({
    range <- get_date_range()
    state$lower_date <- case_when(
      range == "last_week" ~ today() - weeks(1),
      range == "last_month" ~ today() - months(1),
      range == "last_year" ~today() - years(1),
      range == "full_range" ~ download_modal_vars$min,
      TRUE ~ download_modal_vars$min
    )
    state$upper_date <- download_modal_vars$max

    updateSliderInput(session, "range_selector", value = c(state$lower_date, state$upper_date))
  })

  observeEvent(input$ok, {
    okay <- TRUE
    if (okay) {
      removeModal()
    } else if (isUnique) {
      showModal(dataModal(failedUnique = TRUE))
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("covid_19_data_portal", ".csv", sep = "")
    },
    content = function(file) {

      shiny::withProgress(
        message = paste0("Preparing indicators for download - this may take a moment"),
        value = 0,
        {
          ns <- session$ns
          disable(ns("downloadData"))
          selected_keys <- input$indicator_selector
          data <- get_download_csv(
            get_data_store_filtered(selected_keys),
            get_indicator_definitions_filtered(selected_keys),
            c(state$lower_date, state$upper_date)
          )
          data <- apply(data, 2, as.character)
          enable(ns("downloadData"))
          write_excel_csv(as.data.frame(data), file)
        }
      )
    }
  )
}
