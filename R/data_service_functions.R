load_from_store <- function(indicator, group_name) {
  key <- paste(indicator$class, indicator$type, indicator$indicator_name, group_name, sep = "_")
  if (length(key) > 0) {
    data_object <- DATA_STORE[[key]]
  } else {
    return(NULL)
  }
  return(data_object)
}

stats_odata_api <- function(indicator, group_name) {
  print(indicator$api_resource_id)
  print(paste0(CONFIG$odata_url,
               "Covid-19Indicators/Observations",
               "?$filter=(ResourceID eq '",
               indicator$api_resource_id,
               "')"))
  Observations <- GET(
    URLencode(paste0(CONFIG$odata_url,
                     "Covid-19Indicators/Observations",
                     "?$filter=(ResourceID eq '",
                     indicator$api_resource_id,
                     "')")),
    add_headers("Ocp-Apim-Subscription-Key" = CONFIG$odata_token)) %>%
    content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  Resource <- GET(
    URLencode(paste0(CONFIG$odata_url,
                     "Covid-19Indicators/Resources",
                     "?$filter=(ResourceID eq '",
                     indicator$api_resource_id,
                     "')")),
    add_headers("Ocp-Apim-Subscription-Key" = CONFIG$odata_token))  %>%
    content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  parsed <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
  data  <- parsed$value

  print(data)

  if (length(data$Value) == 0) {return(NULL)}

  data_group <- data %>%
    mutate(Parameter = ymd(data$Period)) %>%
    select(Parameter=Period , Value)

  data_object <- TimeSeries$new(data_group, unique(Resource$value$Title), as.Date(now()))
  return(data_object)
}

load_environmental_data <- function(indicator, group_name) {
  if (is.null(group_name) || group_name == "") {
    return(NULL)
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
  return(data_object)
}

data_service_functions <- list(
  load_from_store = load_from_store,
  stats_odata_api = stats_odata_api,
  load_environmental_data = load_environmental_data
)
