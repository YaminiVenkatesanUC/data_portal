main_plot_ui <- function(id, label) {
  ns <- NS(id)
  fluidRow(
    column(
      12,
      align = "center",
      highchartOutput(outputId = ns("main_plot"))
    ),
    column(
      6,
    conditionalPanel(
      paste0("input['", ns("include_date_slider"), "'] == 'TRUE' "),
        align = "center",
        sliderInput(
          ns("range_selector"),
          label = "Select date range",
          min = 0,
          max = 0,
          value = c(0, 0),
          timeFormat = "%d-%b"
        )
      ),
    offset = 3
    ),
    column(
      3,
      align = "center",
      textOutput(ns("indicator_update_date")),
      htmlOutput(ns("source_html"))
    ),
    column(
      12,
      offset = 0,
      align = "center",
      htmlOutput(ns("caveat_html"))
    ),
    column(
      12,
      offset = 0,
      align = "center",
      htmlOutput(ns("description_html"))
    )
  )
}

top_panel_ui <- function(id, indicator_class) {
  ns <- NS(id)
  fluidRow(
    column(
      4,
      selectInput(
        inputId = ns("type_selector"),
        label = "Select category",
        choices = c(""),
        selected = ""
      )
    ),
    column(
      4,
      selectInput(
        inputId = ns("indicator_selector"),
        label = "Select an indicator",
        choices = c(""),
        selected = ""
      )
    ),
    column(
      4,
      conditionalPanel(
        paste0("input['", ns("multiple_time_series"), "'] == 'NEVER' "),
         selectInput(
           inputId = ns("multiple_time_series"),
           label = "Select multiple",
           choices = c("TRUE", "FALSE"),
           multiple = FALSE,
           selected = "FALSE",
           selectize = FALSE),
        selectInput(
          inputId = ns("include_date_slider"),
          label = "Select multiple",
          choices = c("TRUE", "FALSE"),
          multiple = FALSE,
          selected = "FALSE",
          selectize = FALSE)
          ),
      conditionalPanel(
        paste0("input['", ns("multiple_time_series"), "'] == 'TRUE' "),
        selectInput(
         inputId = ns("line_selector"),
         label = "Select a series",
         choices = "",
         selected = ""
        )
      )
    )
  )
}

main_plot_server <- function(
  input,
  output,
  session,
  indicator_class,
  indicator_definitions,
  regional_filter_on
) {
  get_type_options <- reactive({
    indicators <- get_type_list(indicator_definitions, indicator_class)
    if (length(indicators) == 0) {
      return(c("No indicators"))
    } else {
      types <- get_type_list(indicators, indicator_class, transform = function(x) x$type)
      return(sort(unique(types)))
    }
  })

  get_indicator_options <- reactive({
    indicators <- get_indicator_list(indicator_definitions, indicator_class, input$type_selector)
    if (length(indicators) == 0) {
      return(c("No indicators"))
    } else {
      domestic_indicators <- get_indicator_list(
        indicators,
        indicator_class,
        input$type_selector,
        international = FALSE,
        transform = function(x) x$indicator_name
      )
      intl_indicators <- get_indicator_list(
        indicators,
        indicator_class,
        input$type_selector,
        international = TRUE,
        transform = function(x) x$indicator_name
      )
      return(list(
        "New Zealand" = wrap_list_if_length_one(domestic_indicators),
        "International" = wrap_list_if_length_one(intl_indicators)
      ))
    }
  })

  get_indicator_definition <- reactive({
    key <- paste(indicator_class, input$type_selector, input$indicator_selector, sep = "_")
    indicator_definition <- indicator_definitions[[key]]
    return(indicator_definition)
  })

  get_group_definition <- reactive({
    indicator_definition <- get_indicator_definition()
    group_index <- which(sapply(
      indicator_definition$groups,
      function(x) x$name) == input$line_selector
    )
    if (length(group_index) > 0) {
        group_definition <- indicator_definition$groups[[group_index]]
        return(group_definition)
    }
    return(NULL)
  })

  get_line_options <- reactive({
    indicator_definition <- get_indicator_definition()
    line_options <- sapply(indicator_definition$groups, function(x) x$name)
    return((line_options))
  })

  observe({
    updateSelectInput(
      session,
      "type_selector",
      choices = get_type_options(),
      selected = NULL
    )
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['category']]) && query[['category']] %in% get_type_options()) {
      updateSelectInput(session, "type_selector", selected = query[['category']])
    }
  })

  observe({
    updateSelectInput(
      session,
      "indicator_selector",
      choices = get_indicator_options(),
      selected = NULL
    )
    query <- parseQueryString(session$clientData$url_search)
    if (
      !is.null(query[['indicator']]) && query[['indicator']] %in% unlist(get_indicator_options())
    ) {
      updateSelectInput(session, "indicator_selector", selected = query[['indicator']])
    }
  })

  observeEvent(input$line_selector, {
    if (input$type_selector != "" && input$indicator_selector != "" && input$line_selector != "") {
      key <- paste(indicator_class, input$type_selector, input$indicator_selector, sep = "_")
      session$sendCustomMessage('indicator_selected', paste0(key, "_", input$line_selector))
    }
  })

  observe({
    updateSelectInput(
      session,
      "line_selector",
      choices = get_line_options(),
      selected = NULL
    )
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['series']]) && query[['series']] %in% get_line_options()) {
      updateSelectInput(session, "line_selector", selected = query[['series']])
    }
  })

  observe({
    indicator_definition <- get_indicator_definition()
    line_options <- get_line_options()
    if (length(line_options) > 0 && get_line_options() == c("undefined_name")) {
      multiple_time_series <- "FALSE"
    } else {
      multiple_time_series <- "TRUE"
    }

    if (multiple_time_series) {
      result <- "TRUE"
    } else {
      result <- "FALSE"
    }
    updateTextInput(
      session,
      "multiple_time_series",
      value = result
    )
  })

  observe({
    indicator_definition <- get_indicator_definition()
    include_date_slider <- !is.null(indicator_definition$include_date_slider) &&
      indicator_definition$include_date_slider == TRUE

    updateTextInput(
      session,
      "include_date_slider",
      value = as.character(include_date_slider)
    )
  })

  get_range_limits <- reactive({
    input_data <- get_data_object()
    if ("Date" %in% names(input_data$data)) {
        dates <- input_data$data$Date
        return(list(min = min(dates), max = max(dates)))
    }
  })

  get_data_object <- reactive({
    indicator_definition <- get_indicator_definition()
    data_object <- fetch_data(indicator_definition, input$line_selector)

    if ("TimeSeries" %in% class(data_object)) {
      dates <- data_object$dates
      range_limits <-  (list(min = min(dates), max = max(dates)))

      if (!is.null(indicator_definition$default_lower_range)) {
        range <- c(
          ymd(indicator_definition$default_lower_range),
          range_limits$max
        )
      } else {
        range <- c(range_limits$min, range_limits$max)
      }

      updateSliderInput(
        session,
        "range_selector",
        min = range_limits$min,
        max = range_limits$max,
        value = range
      )
    }
    return(data_object)
  })

  get_range_selector <- reactive({
    return(input$range_selector)
  })

  get_main_plot <- function() {
    data_object <- get_data_object()
    if (is.null(data_object)) {return(NULL)}
    indicator_definition <- get_indicator_definition()
    plot_function <- get_indicator_parameter(
      "plot_function",
      indicator_definition,
      input$line_selector
    )
    plot <- plot_functions[[plot_function]](data_object, input, indicator_definition)
    return(plot)
  }

  output$main_plot <- renderHighchart({
    get_main_plot()
  })

  output$indicator_update_date <- renderText({
    indicator <- get_data_object()
    if(length(indicator$update_date) > 1){
      update_date <- max(indicator$update_date)
    }
    else {
      update_date <-indicator$update_date
    }

    if (is.null(update_date)) {
        return("")
    }

    return(paste("Last updated: ", format(update_date, "%d %B %Y")))
  })

  output$source_html <- renderUI({
    group_definition <- get_group_definition()
    indicator_definition <- get_indicator_definition()
    source_url <- get_definition_parameter("source_url", indicator_definition, group_definition)
    source_text <- get_definition_parameter("source", indicator_definition, group_definition)

    if (!is.null(source_url)) {
      return(HTML(
        create_source_link(
          paste("Source:", source_text),
          url = source_url,
          id = "url-link"
        ),
        sep = '<br/>'
        )
      )
    } else if (!is.null(source_text)) {
      return(HTML(
        create_source_text_only(paste("Source:", source_text), id = "url-link" ), sep = '<br/>')
      )
    }
    return((HTML("<div></div>", sep = '<br/>')))
  })

  output$caveat_html <- renderUI({
    group_definition <- get_group_definition()
    indicator_definition <- get_indicator_definition()
    caveat <- get_definition_parameter("caveats", indicator_definition, group_definition)

    if (!is.null(caveat)) {
      return(HTML(create_caveat_box(caveat, id = "caveat-box" ), sep = '<br/>'))
    } else {
      return(HTML(NULL))
    }
  })

  output$description_html <- renderUI({
    group_definition <- get_group_definition()
    indicator_definition <- get_indicator_definition()
    description <- get_definition_parameter("description", indicator_definition, group_definition)

    if (!is.null(description)) {
      return(HTML(create_caveat_box(description, id = "description-box" ), sep = '<br/>'))
    } else {
      return(HTML(NULL))
    }
  })

}
