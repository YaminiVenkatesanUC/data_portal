source("R/core/utils_API.R")

CONFIG <- read_config_file()
directory <- CONFIG$data_directory
odata_definitions <- fromJSON(config$odata_definitions)

config_milk <- read_json(CONFIG$data_definitions)[[23]]
config_job <- read_json(CONFIG$data_definitions)[[157]]

dummy_var <- read_from_excel(config_job, odata_definitions, directory)

read_from_excel <- function(config, odata_definitions, directory) {
  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  cols_to_read <- c(config$parameter_col, unlist(config$value_col))
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2 + skip, min(cols_to_read)), c(NA, max(cols_to_read)))
  )) %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter))
  data <- data[, cols_to_read - min(cols_to_read) + 1]

  if (!is.null(config$input_units)) {
    data[, 2:ncol(data)] <- mapply("*", data[, 2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  if (is.null(config$order_parameter) || config$order_parameter) {
    data <- data %>% arrange(Parameter)
  }
  #error when there is not a match or indicator removed
  if(any(config$api_resource_id %in% odata_definitions$ResourceID)){
    print("Adding data to API")
    data_frame_to_json_helper(
      directory,
      config,
      odata_definitions,
      data)
    return(NULL)
  }
  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}