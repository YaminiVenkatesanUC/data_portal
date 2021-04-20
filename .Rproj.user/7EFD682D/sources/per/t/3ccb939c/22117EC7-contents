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
  query <- paste0(
    CONFIG$odata_url,
    "?$apply=filter(Table/subject%20eq%20%27Covid-19%20Portal%27%20and%20",
    "desc_1%20eq%20%27",
    gsub(" ", "%20", indicator$class),
    "%27%20and%20",
    "desc_2%20eq%20%27",
    gsub(" ", "%20", indicator$type),
    "%27%20and%20",
    "desc_3%20eq%20%27",
    gsub(" ", "%20", indicator$indicator_name),
    "%27%20",
    "and%20desc_4%20eq%20%27",
    gsub(" ", "%20", group_name),
    "%27%20",
    ")&$select=desc_5,desc_6,value"
  )
  response <- GET(
    query,
    add_headers("Ocp-Apim-Subscription-Key" = CONFIG$odata_token)
  )
  result <- parse_httr_response(response)
  if (length(result$value) == 0) {return(NULL)}
  table <- result$value %>%
    rename(Parameter = desc_6) %>%
    pivot_wider(names_from = desc_5, values_from = c("value")) %>%
    select(c("Parameter", sort(unique(result$value$desc_5)))) %>%
    mutate(Parameter = dmy(Parameter)) %>%
    arrange(Parameter)

  data_object <- TimeSeries$new(
    table,
    sort(unique(result$value$desc_5)),
    as.Date(now())
  )
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
