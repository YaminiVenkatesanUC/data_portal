about_ui <- function(id) {  
  ns <- NS(id)
  actionButton(ns("show"), "About")
}

about_ui_mobile <- function(id) {  
  ns <- NS(id)
  actionButton(ns("show"), "About")
}

about_dialog_server <- function(input, output, session, most_recent_update_date) {
  aboutModal <- function(failed = FALSE, failedUnique = FALSE) {
    ns <- session$ns
    modalDialog(
      size = "l",
      easyClose = TRUE,
      span(h3(strong('COVID-19 data portal'))),

      p(paste("Last updated:", get_most_recent_update_date(DATA_STORE))),
      h4(strong('About')),
      
      p("Stats NZ's COVID-19 data portal gathers key high-frequency and near real-time economic indicators to help track the impact of COVID-19 on the economy."),
      p("It features graphs from various government agencies and private sector sources to allow users a glance at what's happening to different components of the economy in near real-time."),
      p("The data will be updated as frequently as possible, in some cases on a daily or weekly basis. Other time series may have a longer time lag."),
      p("The data is interactive and users can capture graph images and download selected data series in a CSV file. Time series do vary from indicator to indicator."),
      p("The functionality, format and range of indicators will be developed in further iterations."),
      p("Externally-sourced data has not been verified by Stats NZ. External sources of data are clearly identified, with an acknowledgement of the original source on each graph."),
      p("The data is from well-established and reliable sources, for example sentiment surveys by large New Zealand banks. However, Stats NZ has not attempted to review the methodology of such surveys."),
      p("Decision-makers using the data portal should refer to the original indicator source to understand how the data was collected and issues such as sample error."),
      p("Data from external sources are not official statistics."),
      
      h4(strong('Feedback')),
      
      p("Email communications@stats.govt.nz to let us know what you think of the COVID-19 data portal and how it could be improved."),

      h4(strong('Select graph descriptions')),
      
      tags$ul(class= "aboutullist",
              tags$li("Performance of manufacturing index (PMI) is a monthly survey that provides an early indicator of levels of activity in the New Zealand manufacturing sector."),
              tags$li("Trade weighted index (TWI) is an index measuring the value of the NZD relative to other countries' currencies."),
              tags$li("Commodity price index (CPI) is an index that tracks a basket of commodities to measure their performance. These indexes are often traded on exchange. The constituents in a CPI can be broadly grouped into the categories of: energy, metals, and agriculture."),
              tags$li("Sentix Sentiment index represent investors' market expectations over the next month. The sentix Sentiment is obtained from the weekly sentix Survey (sentix Global Investor Survey) of currently more than 5,000 private and institutional investors' estimations of 14 financial markets."),
              tags$li("Market volatility index (VIX) is a real-time market index that represents the market's expectations of volatility over the coming 30 days. Investors use the VIX to measure the level of risk, fear, or stress in the market when making investment decisions."),
              tags$li("Baltic dry index (BDI) is a shipping and trade index created by the London-based Baltic Exchange. The BDI measures changes in the cost of transporting various raw materials, such as coal and steel. Many investors consider a rising or contracting BDI to be a leading indicator of future economic growth.")
      ),
      footer = tagList(
          modalButton("Close")
      )
    )
  }
  
  observeEvent(input$show, {
    showModal(aboutModal())
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
}