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
library(httr)

source("R/data_types.R")
source("R/core/type_checks.R")
source("R/core/utils.R")
source("R/core/utils_API.R")
source("R/core/consts.R")
source("R/load_functions.R")

add_to_data_service <- function(data_definition, odata_definitions, data_store, config) {
  check_data_definition(data_definition)
  data_definition <- expand_data_definition_group_names(data_definition)
  print(data_definition$indicator_name)
  data <- load_functions[[data_definition$load_function]](data_definition, config$data_directory, odata_definitions)
  update_date <- as.Date(file.info(paste0(config$data_directory, data_definition$filename))$mtime)
  #for (group_name in unique(data_definition$group_names)) {
  for (group_name in names(data)) {
    #print(paste(data_definition$class, data_definition$indicator_name, group_name))

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

load_data <- function(config, odata_load_flag) {
  config <- CONFIG
  data_store <- list()
  data_definitions <- read_json(config$data_definitions)

  if(odata_load_flag == TRUE){
    odata_definitions <- fromJSON(config$odata_definitions)
    create_odata_version()
  } else {
    odata_definitions <- NULL
  }

  for (data_definition in data_definitions) {
    data_store <- add_to_data_service(data_definition, odata_definitions, data_store, config)
    #break
  }
  #saveRDS(data_store, config$data_store_filename)
  #return(data_store)
}
