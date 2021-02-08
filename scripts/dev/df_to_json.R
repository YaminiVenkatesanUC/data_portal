source("R/core/utils_API.R")

CONFIG <- read_config_file()
directory <- CONFIG$data_directory
config <- read_json(CONFIG$data_definitions)[[1]]

read_from_csv <- function(config, directory) {

  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }

  data <- as.data.frame(read.csv(
    paste0(directory, config$filename),
    skip = skip
  ))
  names(data) <- paste0("col_", 1:ncol(data))

  data <- data %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter)) %>%
    select(c("Parameter", paste0("col_", config$value_col)))

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

  return(data_frame_to_json_helper(
    directory,
    config,
    data
  ))
}