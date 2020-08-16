server <- function(input, output, session) {
  values <- reactiveValues(filtered_indicator_definitions = indicator_definitions)
  
  observe({
    data <- parseQueryString(session$clientData$url_search)
    session$sendCustomMessage(type='updateSelections', data)
  })
  lapply(1:length(INDICATOR_CLASSES), function(i) {
    class_name <- gsub(" ", "_", INDICATOR_CLASSES[[i]])
    callModule(main_plot_server, paste0("main_plot_", class_name), INDICATOR_CLASSES[[i]], values$filtered_indicator_definitions)
  })
  
  callModule(
    download_data_server,
    "download_data",
    get_data_store_date_range(DATA_STORE)
  )
  callModule(about_dialog_server, "about_dialog", get_most_recent_update_date(DATA_STORE))
  #callModule(region_filter_server, "regional_filter")
  
  observeEvent(input$region_selector, {
    print(input$region_selector)
    if (length(input$region_selector) > 0) {
      values$filtered_indicator_definitions <- filter_indicators_by_region(indicator_definitions, input$region_selector)
    } else {
      values$filtered_indicator_definitions <- indicator_definitions
    }
    
    lapply(1:length(INDICATOR_CLASSES), function(i) {
      class_name <- gsub(" ", "_", INDICATOR_CLASSES[[i]])
      callModule(main_plot_server, paste0("main_plot_", class_name), INDICATOR_CLASSES[[i]], values$filtered_indicator_definitions)
    })
  })

  observeEvent(input$show_regional_filter, {
    shinyjs::show(id = "region_selector")
    shinyjs::show(id = "hide_regional_filter")
    shinyjs::hide(id = "show_regional_filter")
    updateSelectizeInput(
      session,
      "region_selector",
      choices = setNames(REGION_LABELS, REGION_LABELS),
      selected = NULL,
      server = TRUE
    )
  })
  
  observeEvent(input$hide_regional_filter, {
    shinyjs::hide(id = "region_selector")
    shinyjs::hide(id = "hide_regional_filter")
    shinyjs::show(id = "show_regional_filter")
    
    
    values$filtered_indicator_definitions <- indicator_definitions
    lapply(1:length(INDICATOR_CLASSES), function(i) {
      class_name <- gsub(" ", "_", INDICATOR_CLASSES[[i]])
      callModule(main_plot_server, paste0("main_plot_", class_name), INDICATOR_CLASSES[[i]], values$filtered_indicator_definitions)
    })
  })
}