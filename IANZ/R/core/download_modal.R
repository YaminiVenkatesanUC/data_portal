download_data_ui <- function(id) {  
  ns <- NS(id)
  actionButton(ns("show"), "Download data")
}

download_data_server <- function(input, output, session, download_modal_vars) {
  downloadModal <- function(failed = FALSE, failedUnique = FALSE) {
    ns <- session$ns
    modalDialog(
      useShinyjs(),
      span(h3(strong(paste(CONFIG$title, 'data download')))),
      p(paste("Last updated:", get_most_recent_update_date(DATA_STORE))),
      includeHTML(CONFIG$download_modal_html),
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
      data <- get_download_csv()
      data <- apply(data,2,as.character)
      write.csv(data, file, row.names = FALSE)
    }
  )
}