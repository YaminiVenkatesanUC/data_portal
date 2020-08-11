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
    date_range <- get_data_store_data_range(DATA_STORE)

    modalDialog(
      useShinyjs(),
      span(h3(strong(paste(CONFIG$title, 'data download')))),
      p(paste("Last updated:", get_most_recent_update_date(DATA_STORE))),
      includeHTML(CONFIG$download_modal_html),
      selectizeInput(
        inputId = ns("indicator_selector"),
        label = "Select indicators",
        choices = names(indicator_definitions),
        selected = "select indicators from the list",
        multiple = TRUE,
        width = '100%',
        options = list(
          placeholder = 'All indicators',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      radioButtons(
        ns("pick_range"),
        "Select a range",
        c("Last week" = "last_week", "Last year" = "last_year", "Full range" = "full_range"),
        selected = "full_range",
        inline = TRUE
      ),
      sliderInput(
        ns("range_selector"),
        label = "Select date range",
        min = download_modal_vars$min,
        max = download_modal_vars$max,
        value = c(download_modal_vars$min, download_modal_vars$max),
        timeFormat = "%d-%b-%y"
      ),
      footer = tagList(
        modalButton("Close"),
        #actionButton(ns("ok"), "Download"),
        downloadButton(ns("downloadData"), "Download")
      )
    )
  }

  observeEvent(input$show, {
    showModal(downloadModal())
  })
  
  get_indicator_definitions_filtered <- function(indicators) {
    if (is.null(indicators)) {
      return (indicator_definitions)
    }
    return (indicator_definitions[names(indicator_definitions) %in% indicators])
  }
  
  get_data_store_filtered <- function(indicators) {
    if (is.null(indicators)) {
      return (DATA_STORE)
    }
    re <- paste(indicators, collapse = "|^")
    re <- paste0("^", re)
    re <- gsub("\\(", "\\\\(", re)
    re <- gsub(")", "\\\\)", re)
    result <- DATA_STORE[names(DATA_STORE) %like% re]
    return (result)
  }
  
  observeEvent(input$indicator_selector, {
    if (length(input$indicator_selector) > 0) {
      date_range <- get_data_store_date_range(get_data_store_filtered(input$indicator_selector))
      updateSliderInput(
        session,
        "range_selector",
        min = date_range$min_date,
        max = state$max_date,
        value = c(date_range$min_date, state$max_date)
      )
    } else {
      updateSliderInput(
        session,
        "range_selector",
        min = download_modal_vars$min,
        max = download_modal_vars$max,
        value = c(download_modal_vars$min, download_modal_vars$max)
      )
    }
  })

  get_date_range <- reactive({
    return (input$pick_range)
  })

  observe({
    range <- get_date_range()
    state$lower_date <- case_when(
      range == "last_week" ~ today() - weeks(1),
      range == "last_year" ~today() - years(1),
      range == "full_range" ~ download_modal_vars$min,
      TRUE ~ download_modal_vars$min
    )
    state$upper_date <-download_modal_vars$max

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

  country_filter <- function(dataset, countries) {
    return (dataset[dataset$country %in% countries,])
  }

  date_filter <- function(dataset, date_range) {
    years <- date_range[[1]]:date_range[[2]]
    return (dataset[dataset$year %in% years,])
  }

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("covid_19_data_portal", ".csv", sep = "")
    },
    content = function(file) {
      
      shiny::withProgress(
        message = paste0("Downloading", input$dataset, " Data"),
        value = 0,
        {
          ns <- session$ns
          disable(ns("downloadData"))
          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          data <- get_download_csv(
            get_data_store_filtered(input$indicator_selector),
            get_indicator_definitions_filtered(input$indicator_selector),
            c(state$lower_date, state$upper_date)
          )
          data <- apply(data,2,as.character)
          enable(ns("downloadData"))
          write.csv(data, file, row.names = FALSE)
        }
      )
    }
  )
}