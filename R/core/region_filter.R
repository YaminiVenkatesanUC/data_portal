region_filter_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      8,
      align="left",
      uiOutput(ns("region_filter"))
    ),
    column(
      2,
      align="right",
      actionButton(ns("hide_region_selector"), "Hide")
    )
  )
}


region_filter_server <- function(input, output, session) {
  output$region_filter <- renderUI({
    selectizeInput(
      inputId = ns("region_selector"),
      label = "Select region",
      choices = setNames(c("Auckland", "Bay of plenty"), c("Auckland", "Bay of plenty")),
      selected = "Select a region",
      multiple = TRUE,
      width = '80%',
      options = list(
        placeholder = 'All regions currently selected',
        onInitialize = I('function() { this.setValue(""); }'),
        'plugins' = list('remove_button'),
        'persist' = FALSE
      )
    )
  })
  
  observeEvent(input$hide_region_selector, {
    shinyjs::hide(id = "region_selector")
    print("HIDE REGIONAL FILTER")
  })
}
