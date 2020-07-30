fetch_data <- function(indicator, group_name) {
  method <- data_service_functions[[
    get_indicator_parameter("data_service", indicator, group_name)
  ]]
  return (method(indicator, group_name))
}
