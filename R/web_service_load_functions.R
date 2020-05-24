default_web_service_load_function <- function(data, indicator, group_name) {
  data_type <- get_indicator_parameter("data_type", indicator, group_name)

  data_object <- DATA_TYPES[[data_type]]$new(
    data %>%
      filter(series_name == group_name) %>%
      select(date, value) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date) %>%
      rename(Parameter = date),
    group_name,
    as.Date(now())
  )

  return (data_object)
}

web_service_load_functions <- list(
  default_web_service_load_function = default_web_service_load_function
)