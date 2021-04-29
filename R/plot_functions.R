get_map_plot <- function(data_object, input, indicator_definition) {
  mapdata <- (download_map_data("countries/nz/nz-all"))

  test_data <- data.frame(
    region = REGION_LABELS,
    value = 1:(length(REGION_LABELS)),
    stringsAsFactors = FALSE
  )

  plot <- highchart() %>%
    hc_add_series_map(mapdata, test_data,
                      name = "Fake data",
                      value = "value", joinBy = c("woe-name", "region"),
                      dataLabels = list(
                        enabled = TRUE,
                        format = "{point.name}"
                      )
    ) %>%
    hc_colorAxis(stops = color_stops()) %>%
    hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
    hc_mapNavigation(enabled = TRUE)

  return(plot)
}


get_time_series_plot <- function(
  data_object,
  input,
  indicator_definition,
  type = "line",
  stacking = "normal"
) {
  group_index <- which(sapply(
    indicator_definition$groups,
    function(x) x$name) == input$line_selector
  )
  if (length(group_index) == 0) {
    return(NULL)
  }
  group_definition <- indicator_definition$groups[[group_index]]

  data <- cbind(data_object$dates, data_object$values)
  names(data) <- c("date", data_object$value_names)
  if (!is.null(indicator_definition$include_date_slider) &&
      indicator_definition$include_date_slider
  ) {
    range <- input$range_selector
    time_range_index <- which(data_object$dates <= range[2] & data_object$dates >= range[1])
  } else {
    time_range_index <- 1:length(data_object$dates)
  }

  plot <- highchart()
  plot <- hc_exporting(
    plot,
    enabled = TRUE,
    filename = paste0(input$indicator_selector),
    buttons = list(
      contextButton = list(
        menuItems = list('downloadPNG', 'downloadPDF')
      )
    )
  )

  dates <- data_object$dates[time_range_index]
  year_label <- ""
  if (length(dates) == 0) {
    return(NULL)
  }

    duration <- abs(as.numeric(difftime(
    dates[[1]],
    dates[[length(dates)]],
    units = c("days")
  )))


  if (duration < 7 & length(dates) > 7) {
    categories <- format(dates, "%d-%b %H:%M")
  } else if (length(unique(year(dates))) == 1) {
    year_label <- paste0("(", year(dates)[[1]], ")")
    categories <- format(dates, "%d-%b")
  } else if (duration < 360) {
    categories <- format(dates, "%d-%b-%y")
  } else {
    categories <- format(dates, "%b-%y")
  }

    if (all(data_object$value_names %in% 2010:2030)) {
      year_label <- ""
      categories <- format(dates, "%d-%b")
    }

    if (!is.null(indicator_definition$frequency)) {
      if (indicator_definition$frequency == "Monthly" || indicator_definition$frequency == "Quarterly"){
        categories <- format(dates, "%b-%Y")
        if(!is.null(group_definition$x_axis_label)){
          year_label <- group_definition$x_axis_label
        }
        else{
          year_label <- NULL
        }
      }
    }

  if (!is.null(group_definition$x_axis_label)){
    year_label <- paste0(year_label, group_definition$x_axis_label)
  }

  norm_factor_and_unit <- get_normalisation_factor(data_object$values)

  if (!is.null(group_definition$visible)) {
    visible <- data_object$value_names %in% group_definition$visible
  } else {
    visible <- rep(TRUE, length(data_object$value_names))
  }


  for (i in 1:length(data_object$value_names)) {
    time_series_data <- (data_object$values[, ..i][[1]])[time_range_index]
    plot <- plot %>% hc_add_series(
      round(time_series_data / norm_factor_and_unit$factor, norm_factor_and_unit$digits),
      name = data_object$value_names[[i]],
      showInLegend = TRUE,
      type = type,
      visible = visible[[i]]
    )
  }

  title <- group_definition$title

  plot <- plot %>% hc_title(
    text = render_title(title),
    style = list( color = "black", fontWeight = "bold", fontFamily = "Source Sans Pro")
  )

  y_label <- group_definition$units

  if(!is.null(group_definition$x_axis_label)){
    year_label <- group_definition$x_axis_label
  }
  else{
    year_label <- NULL
  }


  plot <- hc_xAxis(
    plot,
    categories = categories,
    title = list(
      text = year_label,
      style = list(fontSize = "20px", color = "black", fontFamily = "Source Sans Pro")
    ),
    labels = list(style = list(fontSize = "20px", color = "black", fontFamily = "Source Sans Pro")),
    tickInterval = ceiling(length(categories) / 8)
  )

  tool_tip <- get_tool_tip(group_definition$units)
  plot <- plot %>%
    hc_yAxis(
      title = list(
        text = paste(y_label, norm_factor_and_unit$unit),
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      ),
      labels = list(
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      )
    ) %>%
    hc_add_theme(
      hc_theme(
        chart = list(animation = FALSE, zoomType = "xy")
      )
    ) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(enabled = FALSE),
        enableMouseTracking = TRUE,
        animation = FALSE
      ),
      line = list(animation = FALSE),
      column = list(
        dataLabels = list(enabled = FALSE),
        stacking = stacking,
        animation = FALSE,
        enableMouseTracking = TRUE),
      style = list(fontSize = "30px")
    ) %>%
    hc_tooltip(
      table = TRUE,
      sort = TRUE,
      pointFormat = paste0(
        '<br> <span style="color:{point.color}">\u25CF</span>',
        " {series.name}: ",
        tool_tip$prefix,
        "{point.y} ",
        norm_factor_and_unit$unit,
        tool_tip$suffix
      ),
      headerFormat = '<span style="font-size: 13px">{point.key}</span>'
    ) %>%
    hc_colors(get_brand_colours("graph", 1:9))

  if (!is.null(indicator_definition$show_zero) && indicator_definition$show_zero) {
    plot <- hc_yAxis(plot, min = 0)
  }
  return(plot)
}

get_stacked_time_series_plot <- function(
  data_object,
  input,
  indicator_definition,
  type = "line",
  stacking = "normal"
) {
  group_index <- which(sapply(
    indicator_definition$groups,
    function(x) x$name) == input$line_selector
  )
  if (length(group_index) == 0) {
    return(NULL)
  }
  group_definition <- indicator_definition$groups[[group_index]]

  data <- cbind(data_object$categories, data_object$values)
  names(data) <- c("categories", data_object$value_names)

  plot <- highchart()
  plot <- hc_exporting(
    plot,
    enabled = TRUE,
    filename = paste0(input$indicator_selector),
    buttons = list(
      contextButton = list(
        menuItems = list('downloadPNG', 'downloadPDF')
      )
    )
  )

  categories <- data_object$categories
  year_label <- ""

  norm_factor_and_unit <- get_normalisation_factor(data_object$values)

  if (!is.null(group_definition$visible)) {
    visible <- data_object$value_names %in% group_definition$visible
  } else {
    visible <- rep(TRUE, length(data_object$value_names))
  }

  for (i in 1:length(data_object$value_names)) {
    if ("data.table" %in% class(data_object$values)) {
      time_series_data <- (data_object$values[, ..i][[1]])
    } else {
      time_series_data <- data_object$values[, i]
    }

    plot <- plot %>% hc_add_series(
      data = round(time_series_data / norm_factor_and_unit$factor, norm_factor_and_unit$digits),
      name = data_object$value_names[[i]],
      showInLegend = TRUE,
      type = type,
      visible = visible[[i]]
    )
  }


  title <- group_definition$title

  plot <- plot %>% hc_title(
    text = render_title(title),
    style = list( color = "black", fontWeight = "bold", fontFamily = "Source Sans Pro")
  )

  y_label <- group_definition$units

  plot <- hc_xAxis(
    plot,
    categories = categories,
    title = list(
      text = year_label,
      style = list(fontSize = "20px", color = "black", fontFamily = "Source Sans Pro")
    ),
    labels = list(style = list(fontSize = "20px", color = "black", fontFamily = "Source Sans Pro")),
    tickInterval = ceiling(length(categories) / 8)
  )

  tool_tip <- get_tool_tip(group_definition$units)
  plot <- plot %>%
    hc_yAxis(
      title = list(
        text = paste(y_label, norm_factor_and_unit$unit),
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      ),
      labels = list(
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      )
    ) %>%
    hc_add_theme(
      hc_theme(
        chart = list(animation = FALSE, zoomType = "xy")
      )
    ) %>%
    hc_plotOptions(
      bar = list(
        dataLabels = list(enabled = FALSE),
        enableMouseTracking = TRUE,
        animation = FALSE
      ),
      line = list(animation = FALSE),
      column = list(
        dataLabels = list(enabled = FALSE),
        stacking = stacking,
        animation = FALSE,
        enableMouseTracking = TRUE),
      style = list(fontSize = "30px")
    ) %>%
    hc_tooltip(
      table = TRUE,
      sort = TRUE,
      pointFormat = paste0(
        '<br> <span style="color:{point.color}">\u25CF</span>',
        " {series.name}: ",
        tool_tip$prefix,
        "{point.y} ",
        norm_factor_and_unit$unit,
        tool_tip$suffix
      ),
      headerFormat = '<span style="font-size: 13px">{point.key}</span>'
    ) %>%
    hc_colors(get_brand_colours("graph", 1:9))

  if (!is.null(indicator_definition$show_zero) && indicator_definition$show_zero) {
    plot <- hc_yAxis(plot, min = 0)
  }
  return(plot)
}

get_stacked_bar_chart <- function(data, input, indicator_definition) {
  get_time_series_plot(data, input, indicator_definition, type = "column")
}



get_unstacked_bar_chart <- function(data, input, indicator_definition) {
  get_time_series_plot(data, input, indicator_definition, type = "column", stacking = NULL)
}

get_bar_chart <- function(
  data_object,
  input,
  indicator_definition,
  type,
  rotation,
  stacking = "normal"
) {
  plot <- highchart()
  group_index <- which(sapply(
    indicator_definition$groups,
    function(x) x$name) == input$line_selector
  )
  group_definition <- indicator_definition$groups[[group_index]]
  plot <- highchart()

  plot <- hc_exporting(
    plot,
    enabled = TRUE,
    filename = paste0(input$indicator_selector),
    buttons = list(
      contextButton = list(
        menuItems = list('downloadPNG', 'downloadPDF')
      )
    )
  )

  categories <- data_object$categories
  if (length(categories) == 1) {
    categories <- rep(categories,2)
  }

  label_suffix <- ""

  norm_factor_and_unit <- get_normalisation_factor(data_object$values)

  if (!is.null(group_definition$visible)) {
    visible <- data_object$value_names %in% group_definition$visible
  } else {
    visible <- rep(TRUE, length(data_object$value_names))
  }

  for (i in 1:length(data_object$value_names)) {
    if ("data.table" %in% class(data_object$values)) {
      time_series_data <- (data_object$values[, ..i][[1]])
    } else {
      time_series_data <- data_object$values[, i]
    }

    plot <- plot %>% hc_add_series(
      round(time_series_data / norm_factor_and_unit$factor, norm_factor_and_unit$digits),
      name = data_object$value_names[[i]],
      showInLegend = TRUE,
      type = type,
      visible = visible[[i]]
    )
  }

  title <- group_definition$title

  plot <- plot %>% hc_title(
    text = render_title(title),
    style = list( color = "black", fontWeight = "bold", fontFamily = "Source Sans Pro")
  )

  y_label <- group_definition$units

  if (!is.null(group_definition$x_axis_label)) {
    x_label <- group_definition$x_axis_label
  } else {
    x_label <- NULL
  }



  categories <-
    if (label_suffix != "") {
      plot <- hc_xAxis(
        plot,
        title = list(
          text = paste(x_label),
          style = list(
            fontSize = "20px",
            color = "black",
            fontFamily = "Source Sans Pro"
          )
        ),
        categories = categories,
        labels = list(
          style = list(
            fontSize = "14px",
            color = "black",
            fontFamily = "Source Sans Pro"
          ),
          step = 1,
          rotation = rotation
        )
      )
    } else {
      plot <- hc_xAxis(
        plot,
        title = list(
          text = paste(x_label),
          style = list(
            fontSize = "20px",
            color = "black",
            fontFamily = "Source Sans Pro"
          )
        ),
        categories = categories,
        labels = list(
          style = list(
            fontSize = "14px",
            color = "black",
            fontFamily = "Source Sans Pro"
          ),
          step = 1,
          rotation = rotation
        )
      )
    }
  tool_tip <- get_tool_tip(group_definition$units)

  plot <- plot %>%
    hc_yAxis(
      title = list(
        text = paste(y_label, norm_factor_and_unit$unit),
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      ),
      labels = list(
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      )
    ) %>%
    hc_add_theme(
      hc_theme(
        chart = list(animation = FALSE, zoomType = "xy")
      )
    ) %>%
    hc_plotOptions(bar = list(
      dataLabels = list(enabled = FALSE),
      enableMouseTracking = TRUE,
      stacking = stacking,
      animation = FALSE),
      line = list(animation = FALSE),
      column = list(
        dataLabels = list(enabled = FALSE),
        stacking = stacking,
        animation = FALSE,
        enableMouseTracking = TRUE),
      style = list(fontSize = "30px")
    ) %>%
    hc_tooltip(
      table = TRUE,
      sort = TRUE,
      pointFormat = paste0(
        '<br> <span style="color:{point.color}">\u25CF</span>',
        " {series.name}: ",
        tool_tip$prefix,
        "{point.y} ",
        norm_factor_and_unit$unit,
        tool_tip$suffix
      ),
      headerFormat = '<span style="font-size: 13px">{point.key}</span>'
    ) %>%
    hc_colors(get_brand_colours("graph", 1:9))

  if (!is.null(indicator_definition$show_zero) && indicator_definition$show_zero) {
    plot <- hc_yAxis(plot, min = 0)
  }
  return(plot)
}

get_vertical_bar <- function(data, input, indicator_definition) {
  get_bar_chart(data, input, indicator_definition, type = "column", rotation = -45)
}


get_unstacked_vertical_bar <- function(data, input, indicator_definition) {
  group_index <- which(sapply(
    indicator_definition$groups,
    function(x) x$name) == input$line_selector
  )
  if (length(group_index) == 0) {
    return(NULL)
  }
  group_definition <- indicator_definition$groups[[group_index]]
  if (!is.null(group_definition$rotation)) {
    rotation = group_definition$rotation
  } else {
    rotation = -45
  }
  get_bar_chart(
    data,
    input,
    indicator_definition,
    type = "column",
    rotation = rotation,
    stacking = NULL
  )
}


get_horizontal_bar <- function(data, input, indicator_definition) {
  get_bar_chart(data, input, indicator_definition, type = "bar", rotation = 0)
}

get_unstacked_horizontal_bar <- function(data, input, indicator_definition) {
  get_bar_chart(data, input, indicator_definition, type = "bar", rotation = 0, stacking = NULL)
}

get_time_series_plot_with_errors <- function(
  data_object,
  input,
  indicator_definition,
  type = "line",
  stacking = "normal"
) {
  group_index <- which(sapply(
    indicator_definition$groups,
    function(x) x$name) == input$line_selector
  )
  if (length(group_index) == 0) {
    return(NULL)
  }
  group_definition <- indicator_definition$groups[[group_index]]

  data <- cbind(data_object$dates, data_object$values, data_object$lower, data_object$upper)
  names(data) <- c(
    "date",
    data_object$value_names,
    paste0(data_object$value_names, "_lower"),
    paste0(data_object$value_names, "_upper")
  )
  if (!is.null(indicator_definition$include_date_slider) &&
      indicator_definition$include_date_slider) {
    range <- input$range_selector
    time_range_index <- which(data_object$dates <= range[2] & data_object$dates >= range[1])
  } else {
    time_range_index <- 1:(length(data_object$dates))
  }

  plot <- highchart()
  plot <- hc_exporting(
    plot,
    enabled = TRUE,
    filename = paste0(input$indicator_selector),
    buttons = list(
      contextButton = list(
        menuItems = list('downloadPNG', 'downloadPDF')
      ))
    )

  dates <- data_object$dates[time_range_index]
  year_label <- ""
  if (length(dates) == 0) {
    return(NULL)
  }

  duration <- abs(as.numeric(difftime(
    dates[[1]],
    dates[[length(dates)]],
    units = c("days")
  )))

  if (duration < 7 & length(dates) > 7) {
    categories <- format(dates, "%d-%b %H:%M")
  } else if (length(unique(year(dates))) == 1) {
    year_label <- paste0("(", year(dates)[[1]], ")")
    categories <- format(dates, "%d-%b")
  } else if (duration < 360) {
    categories <- format(dates, "%d-%b-%y")
  } else {
    categories <- format(dates, "%b-%y")
  }

  if (all(data_object$value_names %in% 2010:2030)) {
    year_label <- ""
    categories <- format(dates, "%d-%b")
  }

  if (!is.null(indicator_definition$frequency)) {
    if (indicator_definition$frequency == "Monthly" || indicator_definition$frequency == "Quarterly") {
      categories <- format(dates, "%b-%Y")
      categories <- rep(categories,2)
      if(!is.null(group_definition$x_axis_label)){
        year_label <- group_definition$x_axis_label
      }
      else{
        year_label <- NULL
      }

    }
  }


  norm_factor_and_unit <- get_normalisation_factor(data_object$values)
  tool_tip <- get_tool_tip(group_definition$units)

  if (!is.null(group_definition$visible)) {
    visible <- data_object$value_names %in% group_definition$visible
  } else {
    visible <- rep(TRUE, length(data_object$value_names))
  }


  for (i in 1:length(data_object$value_names)) {
    time_series_data <- data_object$values[, i][time_range_index]
    error_limits <- as.data.frame(
      list(
        low = data_object$lower[, i][time_range_index],
        high = data_object$upper[, i][time_range_index])
    )
    plot <- plot %>%
      hc_add_series(
        round(time_series_data / norm_factor_and_unit$factor, norm_factor_and_unit$digits),
        name = data_object$value_names[[i]],
        showInLegend = TRUE,
        type = type,
        visible = visible[[i]],
        tooltip = list(
          table = TRUE,
          sort = TRUE,
          pointFormat = paste0(
            '<br> <span style="color:{point.color}">\u25CF</span>',
            " {series.name}: ",
            tool_tip$prefix,
            "{point.y} ",
            norm_factor_and_unit$unit,
            tool_tip$suffix
          ),
          headerFormat = '<span style="font-size: 13px">{point.key}</span>'
        )
      ) %>%
      hc_add_series(
        data = list_parse(round(error_limits, 1)),
        type = "errorbar",
        color = "black",
        name = paste(data_object$value_names[[i]], "- error"),
        tooltip = list(
          table = TRUE,
          sort = TRUE,
          pointFormat = paste0(
            '<br> <span style="color:{point.color}">\u25CF</span>',
            " {series.name}: ",
            tool_tip$prefix,
            "{point.low}-{point.high}",
            norm_factor_and_unit$unit,
            tool_tip$suffix
          ),
          headerFormat = '<span style="font-size: 13px">{point.key}</span>'
        )
    )
  }

  title <- group_definition$title

  plot <- plot %>% hc_title(
    text = render_title(title),
    style = list( color = "black", fontWeight = "bold", fontFamily = "Source Sans Pro")
  )

  y_label <- group_definition$units

  plot <- hc_xAxis(
    plot,
    categories = categories,
    title = list(
      text = year_label,
      style = list(fontSize = "20px", color = "black", fontFamily = "Source Sans Pro")
    ),
    labels = list(style = list(fontSize = "20px", color = "black", fontFamily = "Source Sans Pro")),
    tickInterval = ceiling(length(categories) / 8)
  )

  plot <- plot %>%
    hc_yAxis(
      title = list(
        text = paste(y_label, norm_factor_and_unit$unit),
        style = list(fontSize = "20px",  color = "black", fontFamily = "Source Sans Pro")
      ),
      labels = list(
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      )
    ) %>%
    hc_add_theme(
      hc_theme(
        chart = list(animation = FALSE, zoomType = "xy")
      )
    ) %>%
    hc_plotOptions(bar = list(
      dataLabels = list(enabled = FALSE),
      enableMouseTracking = TRUE,
      animation = FALSE),
      line = list(animation = FALSE),
      column = list(
        dataLabels = list(enabled = FALSE),
        stacking = stacking,
        animation = FALSE,
        enableMouseTracking = TRUE),
      style = list(fontSize = "30px")
    ) %>%
    hc_tooltip(table = TRUE) %>%
    hc_colors(get_brand_colours("graph", 1:9))


  if (!is.null(indicator_definition$show_zero) && indicator_definition$show_zero) {
    plot <- hc_yAxis(plot, min = 0)
  }

  # if (group_definition$units == "%") {
  #   plot <- hc_yAxis(plot, max = 100)
  # }
  return(plot)
}

get_unstacked_bar_chart_with_errors <- function(data, input, indicator_definition) {
  get_time_series_plot_with_errors(
    data,
    input,
    indicator_definition,
    type = "column",
    stacking = NULL
  )
}

get_unstacked_vertical_bar_with_errors <- function(data, input, indicator_definition) {
  get_bar_chart_with_errors(
    data,
    input,
    indicator_definition,
    type = "column",
    rotation = -45,
    stacking = NULL
  )
}

get_bar_chart_with_errors <- function(
  data_object,
  input,
  indicator_definition,
  type,
  rotation,
  stacking = "normal"
) {

  plot <- highchart()
  group_index <- which(sapply(
    indicator_definition$groups,
    function(x) x$name) == input$line_selector
  )
  group_definition <- indicator_definition$groups[[group_index]]
  plot <- highchart()

  data <- cbind(data_object$dates, data_object$values, data_object$lower, data_object$upper)
  names(data) <- c(
    "parameter",
    data_object$value_names,
    paste0(data_object$value_names, "_lower"),
    paste0(data_object$value_names, "_upper")
  )

  if (!is.null(indicator_definition$include_date_slider) &&
      indicator_definition$include_date_slider) {
    range <- input$range_selector
    time_range_index <- which(data_object$dates <= range[2] & data_object$dates >= range[1])
  } else {
    time_range_index <- 1:(length(data_object$dates))
  }



  plot <- hc_exporting(
    plot,
    enabled = TRUE,
    filename = paste0(input$indicator_selector),
    buttons = list(
      contextButton = list(
        menuItems = list('downloadPNG', 'downloadPDF')
      )
    )
  )

  categories <- data$parameter

  label_suffix <- ""

  norm_factor_and_unit <- get_normalisation_factor(data_object$values)
  tool_tip <- get_tool_tip(group_definition$units)


  if (!is.null(group_definition$visible)) {
    visible <- data_object$value_names %in% group_definition$visible
  } else {
    visible <- rep(TRUE, length(data_object$value_names))
  }

  for (i in 1:length(data_object$value_names)) {
    if ("data.table" %in% class(data_object$values)) {
      time_series_data <- (data_object$values[, ..i][[1]])
    } else {
      time_series_data <- data_object$values[, i]
    }

    error_limits <- as.data.frame(
      list(
        low = data_object$lower[, i][time_range_index],
        high = data_object$upper[, i][time_range_index])
    )


    plot <- plot %>% hc_add_series(
      round(time_series_data / norm_factor_and_unit$factor, norm_factor_and_unit$digits),
      name = data_object$value_names[[i]],
      showInLegend = TRUE,
      type = type,
      visible = visible,
      tooltip = list(
        table = TRUE,
        sort = TRUE,
        pointFormat = paste0(
          '<br> <span style="color:{point.color}">\u25CF</span>',
          " {series.name}: ",
          tool_tip$prefix,
          "{point.y} ",
          norm_factor_and_unit$unit,
          tool_tip$suffix
        ),
        headerFormat = '<span style="font-size: 13px">{point.key}</span>'
      )
    )%>%
      hc_add_series(
        data = list_parse(round(error_limits, 1)),
        type = "errorbar",
        color = "black",
        name = paste(data_object$value_names[[i]], "- error"),
        tooltip = list(
          table = TRUE,
          sort = TRUE,
          pointFormat = paste0(
            '<br> <span style="color:{point.color}">\u25CF</span>',
            " {series.name}: ",
            tool_tip$prefix,
            "{point.low}-{point.high}",
            norm_factor_and_unit$unit,
            tool_tip$suffix
          ),
          headerFormat = '<span style="font-size: 13px">{point.key}</span>'
        )
      )
  }

  title <- group_definition$title

  plot <- plot %>% hc_title(
    text = render_title(title),
    style = list( color = "black", fontWeight = "bold", fontFamily = "Source Sans Pro")
  )

  y_label <- group_definition$units

  if (!is.null(group_definition$x_axis_label)) {
    x_label <- group_definition$x_axis_label
  } else {
    x_label <- NULL
  }

  categories <-
    if (label_suffix != "") {
      plot <- hc_xAxis(
        plot,
        title = list(
          text = paste(x_label),
          style = list(
            fontSize = "20px",
            color = "black",
            fontFamily = "Source Sans Pro"
          )
        ),
        categories = categories,
        labels = list(
          style = list(
            fontSize = "14px",
            color = "black",
            fontFamily = "Source Sans Pro"
          ),
          step = 1,
          rotation = rotation
        )
      )
    } else {
      plot <- hc_xAxis(
        plot,
        title = list(
          text = paste(x_label),
          style = list(
            fontSize = "20px",
            color = "black",
            fontFamily = "Source Sans Pro"
          )
        ),
        categories = categories,
        labels = list(
          style = list(
            fontSize = "14px",
            color = "black",
            fontFamily = "Source Sans Pro"
          ),
          step = 1,
          rotation = rotation
        )
      )
    }
  tool_tip <- get_tool_tip(group_definition$units)

  plot <- plot %>%
    hc_yAxis(
      title = list(
        text = paste(y_label, norm_factor_and_unit$unit),
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      ),
      labels = list(
        style = list(
          fontSize = "20px",
          color = "black",
          fontFamily = "Source Sans Pro"
        )
      )
    ) %>%
    hc_add_theme(
      hc_theme(
        chart = list(animation = FALSE, zoomType = "xy")
      )
    ) %>%
    hc_plotOptions(bar = list(
      dataLabels = list(enabled = FALSE),
      enableMouseTracking = TRUE,
      stacking = stacking,
      animation = FALSE),
      line = list(animation = FALSE),
      column = list(
        dataLabels = list(enabled = FALSE),
        stacking = stacking,
        animation = FALSE,
        enableMouseTracking = TRUE),
      style = list(fontSize = "30px")
    ) %>%
    hc_tooltip(
      table = TRUE,
      sort = TRUE,
      pointFormat = paste0(
        '<br> <span style="color:{point.color}">\u25CF</span>',
        " {series.name}: ",
        tool_tip$prefix,
        "{point.y} ",
        norm_factor_and_unit$unit,
        tool_tip$suffix
      ),
      headerFormat = '<span style="font-size: 13px">{point.key}</span>'
    ) %>%
    hc_colors(get_brand_colours("graph", 1:9))

  if (!is.null(indicator_definition$show_zero) && indicator_definition$show_zero) {
    plot <- hc_yAxis(plot, min = 0)
  }

  plot <- hc_yAxis(plot, max = max(data_object$values) + 5)
  return(plot)
}

plot_functions <- list(
  get_time_series_plot = get_time_series_plot,
  get_time_series_plot_with_errors = get_time_series_plot_with_errors,
  get_stacked_bar_chart = get_stacked_bar_chart,
  get_unstacked_bar_chart = get_unstacked_bar_chart,
  get_unstacked_bar_chart_with_errors = get_unstacked_bar_chart_with_errors,
  get_vertical_bar = get_vertical_bar,
  get_horizontal_bar = get_horizontal_bar,
  get_unstacked_horizontal_bar = get_unstacked_horizontal_bar,
  get_unstacked_vertical_bar = get_unstacked_vertical_bar,
  get_map_plot = get_map_plot,
  get_unstacked_vertical_bar_with_errors = get_unstacked_vertical_bar_with_errors,
  get_bar_chart_with_errors = get_bar_chart_with_errors,
  get_stacked_time_series_plot = get_stacked_time_series_plot
)
