parse_httr_response <- function(response) {
  if (http_type(response) != "application/json") {
    warning("request did not return json")
    return (NULL)
  }
  if (status_code(response) != 200) {
    warning(paste("request failed with response code", status_code(response)))
    return (NULL)
  }
  result <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
  return (result)
}

get_indicator_parameter <- function(parameter, indicator, group_name = NULL) {
  if (!is.null(group_name)) {
    group_index <- which(sapply(indicator$groups, function(x) x$name) == group_name)
    if (length(group_index) > 0) {
      group_definition <- indicator$groups[[group_index]]
      if (!is.null(group_definition[[parameter]])) {
        return (group_definition[[parameter]])
      }
    }
  }
  if (!is.null(indicator[[parameter]])) {
    return (indicator[[parameter]])
  }
  return (CONFIG$default_parameters[[parameter]])
}

data_frame_to_data_object_helper <- function(directory, config, data) {
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")
  data_object_list <- list()
  for (group_name in unique(config$group_names)) {
    indexes <- which(config$group_names == group_name)
    values <- (data[colnames(data) != "Parameter"])[, indexes]
    value_names <- unlist(config$value_names)[indexes]
    temp_data <- data.frame(Parameter = data$Parameter)
    temp_data <- cbind(temp_data, values)
    data_type <- get_indicator_parameter("data_type", config)
    data_object_list[[group_name]] <- DATA_TYPES[[data_type]]$new(temp_data, value_names, update_date)
  }
  return (data_object_list)
}

render_title <- function(text) {
  return (gsub(" - ", " \U2012 ", text))
}

get_tool_tip <- function(units) {
  suffix <- ""
  prefix <- ""
  if (units %in% TOOL_TIP_SUFFIX) {
    suffix <- paste0(" ", units)
  }
  if (units %in% TOOL_TIP_PREFIX) {
    prefix <- units
  }
  return (list(
    suffix = suffix,
    prefix = prefix
  ))
}

get_most_recent_update_date <- function(data_store) {
  if (is.null(data_store)) {
    return (format(now(), "%d/%m/%Y"))
  }
  dates <- as.vector(sapply(data_store, function(x) x$update_date))
  return (format(data_store[[which(dates == max(dates))[[1]]]]$update_date, "%d/%m/%Y"))
}

get_class_names <- function(indicators) {
  class_names <- as.vector(sapply(indicators, function(x) x$class))
  return (unique(class_names))
}

read_config_file <- function(file = "config/config.yaml") {
  config <- tryCatch(
    {
      read.config(file = file)
    },
    warning=function(cond) {
      message(paste("Config file not found - create config.yaml"))
      return(NULL)
    }
  )
  check_config_file(config)
  return(config)
}

wrap_list_if_length_one <- function(x) {
  if (length(x) == 1) {
    result <- list(x)
  } else if (length(x) > 1) {
    result <- sort(x)
  } else {
    result <- NULL
  }
  return (result)
}

get_indicator_list <- function(
  indicators,
  class,
  type,
  international = c(T, F),
  transform = NULL
) {
  selection = as.vector(sapply(
    indicators,
    function(x) (x$class %in% class && x$type %in% type && x$international %in% international)
  ))
  if (!is.null(transform)) {
    result = as.vector(sapply(
      indicators[selection],
      transform
    ))
  } else {
    result <- indicators[selection]
  }

  return (result)
}


get_type_list <- function(
  indicators,
  class,
  transform = NULL
) {
  selection = as.vector(sapply(
    indicators,
    function(x) (x$class %in% class)
  ))
  if (!is.null(transform)) {
    result = as.vector(sapply(
      indicators[selection],
      transform
    ))
  } else {
    result <- indicators[selection]
  }
  
  return (result)
}

expand_data_definition_group_names <- function(data_definition) {
  if (length(data_definition$group_names) == 1) {
    data_definition$group_names <- rep(data_definition$group_names, length(data_definition$value_names))
  }
  return (data_definition)
}


get_definition_parameter <- function(parameter, indicator_definition, group_definition) {
  if (!is.null(group_definition[[parameter]])) {
    return (group_definition[[parameter]])
  }
  return (indicator_definition[[parameter]])
}

create_source_link <- function(buttonText, id, url) {
  class <- "class=\"hidden-xs\""
  buttonClass <- "class=\"btn btn-default\""
  href <- paste0("href=\"", url, "\"")
  buttonId <- paste0("id=\"", id ,"\"")
  target <- "target=\"_blank\""
  
  output <- paste(
    "<a",
    buttonId,
    href,
    target,
    ">",
    buttonText,
    "</a>"
  )
  return (output)
}

create_source_text_only <- function(buttonText, id) {
  class <- "class=\"hidden-xs\""
  buttonClass <- "class=\"btn btn-default\""
  buttonId <- paste0("id=\"", id ,"\"")
  target <- "target=\"_blank\""
  
  output <- paste(
    "<p",
    buttonId,
    ">",
    buttonText,
    "</p>"
  )
  return (output)
}


create_caveat_box <- function(buttonText, id) {
  class <- "class=\"hidden-xs\""
  buttonClass <- "class=\"btn btn-default\""
  buttonId <- paste0("class=\"", id ,"\"")
  
  output <- paste(
    "<div",
    buttonId,
    "><span style=\"color: black;\"><p>",
    buttonText,
    "</p></span></div>"
  )
  return (output)
}


# Similar to get_normalisation_factor below, but for a single
#   value
# TODO: These functions should be combined into one and improved
format_value <- function(value) {
  result <- list()
  if (abs(value) > 1e9) {
    result$value <- value / 1e9
    result$unit <- "(billion)"
  } else if (abs(value) > 1e6) {
    result$value <- value / 1e6
    result$unit <- "(million)"
  } else {
    result$value <- value
    result$unit <- ""
  }
  result$value <- round(result$value, 2)
  return (result)
}

format_values_to_str <- function(values) {
  formated_values <- sapply(values, format_value)
  return (apply(formated_values, 2, function(x) paste0("$", x[1], " ", x[2])))
}

# For styling some of the output boxes
VB_style <- function(msg = "", style = "font-size: 100%;") {
  tags$p(msg, style = style )
}

# For some plots we want to rescale everything to millions
#   or biliions.  This function finds the largest value in
#   the list and determines what factor is required then
#   returns a list containing the factor and the "unit"
get_normalisation_factor <- function(values) {
  max_value = 0
  if (length(values) > 0) {
    max_value <- max(abs(values), na.rm = TRUE)
  }
  result <- list()
  if (max_value > 1e9) {
    result$factor <- 1e9
    result$unit <- "(billion)"
  } else if (max_value > 1e6) {
    result$factor <- 1e6
    result$unit <- "(million)"
  } else {
    result$factor <- 1
    result$unit <- ""
  }
  
  if (max_value < 1) {
    result$digits <- 4
  } else {
    result$digits <- 2
  }

  return (result)
}

# Takes a single value and rounds it into the correct
#   format.  Returns the rounded value and the "unit"
round_and_normalise <- function(value) {
  magnitude <- log10(value)
  unit <- case_when(
    (magnitude <= 6) ~ "",
    (magnitude > 6 & magnitude <= 9) ~ "million",
    (magnitude > 9 ) ~ "billion"
  )
  divisor <- case_when(
    (magnitude <= 6) ~ 1,
    (magnitude > 6 & magnitude <= 9) ~ 1e6,
    (magnitude > 9) ~ 1e9
  )
  return (paste0("$", round(value/divisor, 2), " " , unit))
}

brand_graph_colours <- c(
  "#085c75",
  "#d2ac2f",
  "#ae4e51",
  "#35345d",
  "#76a93f",
  "#6f2e38",
  "#0d94a3",
  "#dd6829",
  "#1a6e5b")

brand_secondary_colours <- c(
  "#172a45",
  "#004d20",
  "#702e01",
  "#005c75",
  "#007f39",
  "#c04124",
  "#00b2c3",
  "#a2c62b",
  "#d0cdbb"
)

brand_primary_colours <- c(
  "#272726",
  "#4d5b61",
  "#ed7218"
)

get_brand_colours <- function(type = "primary", number = 1) {
  upper_limit <- case_when(
    (type == "primary") ~ length(brand_primary_colours),
    (type == "secondary") ~ length(brand_secondary_colours),
    (type == "graph") ~ length(brand_graph_colours)
  )
  if (max(number) > upper_limit) {
    stop("requested number of brand colour above upper limit")
  }
  result <- case_when(
    (type == "primary") ~ brand_primary_colours[number],
    (type == "secondary") ~ brand_secondary_colours[number],
    (type == "graph") ~ brand_graph_colours[number]
  )
  return (result)
}

dollar_axis_label <- function(unit) {
  result <- "$"
  if (unit != "") {
    result <- paste0("$ (" , tools::toTitleCase(unit), ")")
  }
  return (result)
}

createHeaderButton <- function(buttonText, position, id, btn_class) {
  #style <- paste0("style=\"float:right; margin-top: 10px; right: ", position, "px; position: absolute\"")
  style <- paste0("style=\"float:right; margin-top: 10px; right:\"")
  class <- "class=\"hidden-xs\""
  buttonClass <- paste("class=\"btn", btn_class ,"action-button shiny-bound-input\"")
  buttonId <- paste0("id=\"", id ,"\"")
  buttonType <- "type=\"button\""

  output <- paste(
    "<div",
      class,
      style,
    ">",
      "<button",
          buttonId,
          buttonClass,
          buttonType,
        ">",
        buttonText,
      "</button>",
    "</div>"
    )
  return (output)
}

createHeaderLink <- function(buttonText, position, id, url) {
  style <- paste0("style=\"float:right; margin-top: 10px; right: ", position, "px; position: absolute\"")
  class <- "class=\"hidden-xs\""
  buttonClass <- "class=\"btn btn-default\""
  href <- paste0("href=\"", url, "\"")
  buttonId <- paste0("id=\"", id ,"\"")
  target <- "target=\"_blank\""

  output <- paste(
    "<div",
      class,
      style,
    ">",
      "<a",
        buttonId,
        buttonClass,
        href,
        target,
      ">",
        buttonText,
      "</a>",
    "</div>"
  )
  return (output)
}