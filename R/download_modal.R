download_data_ui <- function(id) {  
  ns <- NS(id)
  actionButton(ns("show"), "Download data")
}

download_data_server <- function(input, output, session, download_modal_vars) {
  downloadModal <- function(failed = FALSE, failedUnique = FALSE) {
    ns <- session$ns
    modalDialog(
      useShinyjs(),
      span(h3(strong('COVID-19 data download'))),
      p(paste("Last updated:", get_most_recent_update_date(DATA_STORE))),
      h4(strong('What is included in this data download?')),
      
      p("This file contains the data behind many of the indicators displayed through this portal. We are unable to make all datasets available in this file."),
      p("The data is presented in a long-table csv format for easy filtering."),

      h4(strong('Columns included')),

      tags$ul(class= "aboutullist",
              tags$li(strong('class:'), "The indicator class, such as economic, social or health"),
              tags$li(strong('category:'), "The indicator category, such as activity or confidence "),
              tags$li(strong('indicator_name:'), "The indicator name"),
              tags$li(strong('series_name : '), "The series name"),
              tags$li(strong('sub_series_name : '), "The sub-series name (from series with multiple lines)"),
              tags$li(strong('date :'), "The date (in the format dd/mm/yyyy)"),
              tags$li(strong('value:'), "The value of the series"),
              tags$li(strong('units:'), "The units associated with this series."),
              tags$li(strong('last_update_date:'), "The date we last updated this indicator (not the last date in the series).")
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