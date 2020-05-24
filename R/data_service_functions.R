load_from_store <- function(indicator, group_name) {
  key <- paste(indicator$class, indicator$type, indicator$indicator_name, group_name, sep = "_")
  if (length(key) > 0) {
    data_object <- DATA_STORE[[key]]
  } else {
    return (NULL)
  }
  return (data_object)
}

load_from_web_service <- function(indicator, group_name) {
  # this is an example, not actually used here
  request <- list(
    class = indicator$class,
    type = indicator$type,
    indicator_name = indicator$indicator_name,
    series = group_name
  )

  url <- get_indicator_parameter("data_service_url", indicator, group_name)

  # would usually build url of request here based on request list above
  # for now we just call the url provided
  response <- GET(url)
  result <- parse_httr_response(response)
  
  data <- result$result$records
  data_object <- load_functions[[get_indicator_parameter("load_function", indicator, group_name)]](
    data,
    indicator,
    group_name
  )
  return (data_object)
}

load_environmental_data <- function(indicator, group_name) {
  if (is.null(group_name) || group_name == "") {
    return (NULL)
  }
  url <- get_indicator_parameter("data_service_url", indicator, group_name)
  id <- get_indicator_parameter("data_service_id", indicator, group_name)
  response <- POST(
    url,
    body = toJSON(list(action = unbox("get_flows"), id = id)),
    add_headers("Content-Type" = "application/json"),
    encode = "json"
  )
  result <- parse_httr_response(response)
  data <- result$flows %>%
    mutate(Parameter = ymd_hms(time, tz = "NZ")) %>%
    arrange(Parameter) %>%
    unique()
  
  data_object <- TimeSeries$new(
    data %>%
      select(c("Parameter", get_indicator_parameter("data_service_filter", indicator, group_name))),
    c(group_name),
    as.Date(now())
  )
  return (data_object)
}

data_service_functions <- list(
  load_from_store = load_from_store,
  load_from_web_service = load_from_web_service,
  load_environmental_data = load_environmental_data
)