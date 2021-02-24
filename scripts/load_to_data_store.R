library(R6)
library(dplyr)
library(configr)
library(highcharter)
library(shinyjs)
library(shinycssloaders)
library(readxl)
library(lubridate)
library(tidyr)
library(stringr)
library(jsonlite)
library(foreach)
library(data.table)
library(tibble)
library(gtools)

source("R/data_types.R")
source("R/core/type_checks.R")
source("R/core/utils.R")
source("R/core/consts.R")
source("R/load_functions.R")

add_to_data_store <- function(data_definition, data_store, config) {
  check_data_definition(data_definition)
  data_definition <- expand_data_definition_group_names(data_definition)
  data <- load_functions[[data_definition$load_function]](data_definition, config$data_directory)
  update_date <- as.Date(file.info(paste0(config$data_directory, data_definition$filename))$mtime)

  #for (group_name in unique(data_definition$group_names)) {
  for (group_name in names(data)) {
    print(paste(data_definition$class, data_definition$indicator_name, group_name))

    key <- paste(
      data_definition$class,
      data_definition$type,
      data_definition$indicator_name,
      group_name,
      sep = "_"
    )

    if (key %in% names(data_store)) {
      data_store[[key]] <- data_store[[key]] + data[[group_name]]
    } else {
      data_store[[key]] <- data[[group_name]]
    }
  }

  return(data_store)
}

load_to_data_store <- function(config) {
  data_store <- list()
  data_definitions <- read_json(config$data_definitions)

  for (data_definition in data_definitions) {
    data_store <- add_to_data_store(data_definition, data_store, config)
  }
  saveRDS(data_store, config$data_store_filename)
  return(data_store)
}
