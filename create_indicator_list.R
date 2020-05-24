# make indicator list for flow and rainfall data
get_name <- function(x) {
  if (x == "flow") {
    return ("Flow")
  }
  if (x == "temperature") {
    return ("temperature")
  }
  if (x == "stage_height") {
    return ("Stage height")
  }
  if (x == "rainfall") {
    return ("Rainfall")
  }
  return (x)
}
url <- "https://www.openriverdata.com"
response <- POST(
  url,
  body = toJSON(list(action = unbox("get_features"), filters = c("flow", "stage_height", "rainfall"))),
  add_headers("Content-Type" = "application/json"),
  encode = "json"
)
result <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)

indicators <- list()
keys <- c()

for (i in 1:length(result$features)) {
  feature <- result$features[[i]]
  groups_list <- list()
  for (j in 1:length(feature$observables)) {
    observable <- feature$observables[[j]]
    if (observable$type != "rainfall") {
      groups_list[[j]] <- list(
        name = get_name(observable$type),
        units = observable$units,
        data_service_filter = observable$type,
        title = paste(get_name(observable$type), " at ", feature$river_name)
      )
    }
  }

  key <- paste(feature$region, feature$river_name, sep = "_")
  if (!(key %in% keys) && length(groups_list) > 0) {
    keys <- c(keys, key)
    indicators[[length(indicators) + 1]] <- list(
      class = "River flow",
      indicator_name = feature$river_name,
      type = feature$region,
      source = feature$data_source,
      plot_function = "get_time_series_plot",
      international = FALSE,
      source_url = "https://www.riverguide.co.nz",
      download = TRUE,
      include_date_slider = TRUE,
      data_service = "load_environmental_data",
      data_service_url = "https://www.openriverdata.com",
      data_service_id = feature$id,
      groups = I(groups_list)
    )
  }
}

for (i in 1:length(result$features)) {
  feature <- result$features[[i]]
  groups_list <- list()
  for (j in 1:length(feature$observables)) {
    observable <- feature$observables[[j]]
    if (observable$type == "rainfall") {
      groups_list[[j]] <- list(
        name = get_name(observable$type),
        units = observable$units,
        data_service_filter = observable$type,
        title = paste(get_name(observable$type), " at ", feature$river_name)
      )
    }
  }
  
  key <- paste(feature$region, feature$river_name, sep = "_")
  if (!(key %in% keys) && length(groups_list) > 0) {
    keys <- c(keys, key)
    indicators[[length(indicators) + 1]] <- list(
      class = "Rainfall",
      indicator_name = feature$river_name,
      type = feature$region,
      source = feature$data_source,
      plot_function = "get_stacked_bar_chart",
      international = FALSE,
      source_url = "https://www.riverguide.co.nz",
      download = TRUE,
      include_date_slider = TRUE,
      data_service = "load_environmental_data",
      data_service_url = "https://www.openriverdata.com",
      data_service_id = feature$id,
      groups = I(groups_list)
    )
  }
}

write(toJSON(indicators, pretty = TRUE, auto_unbox = TRUE), "environmental_indicators.json")
