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
library(R6)
library(httr)

source("R/utils.R")
source("R/download_modal.R")
source("R/about_modal.R")
source("R/main_panel.R")
source("R/consts.R")
source("R/load_functions.R")
source("R/plot_functions.R")
source("R/get_download_csv.R")
source("R/type_checks.R")
source("R/data_interface.R")
source("R/web_service_load_functions.R")
source("R/data_types.R")
source("R/build_ui.R")

options(scipen = 999)

# globally available
CONFIG <- read_config_file()

indicator_definitions_raw <- read_json(CONFIG$indicator_definitions)

indicator_definitions <- list()

for (item in indicator_definitions_raw) {
  check_indicator_definition(item)
  if (is.null(item$disabled) || !item$disabled) {
    key <- paste(item$class, item$type, item$indicator_name, sep = "_")
    if (key %in% names(indicator_definitions)) {
      stop(
        paste(
          DUPLICATE_INDICATOR_ERROR,
          item$class,
          item$type,
          item$indicator_name
        )
      )
    } else {
      indicator_definitions[[key]] <- item
    }
  }
}

INDICATOR_CLASSES <- get_class_names(indicator_definitions)

if (!is.null(CONFIG$production) & !CONFIG$production) {
  warnings(DEV_MODE_WARNING)
  DATA_STORE <- load_to_data_store(CONFIG)
} else if (!is.null(CONFIG$data_store_filename)) {
  DATA_STORE <- readRDS(CONFIG$data_store_filename)
} else {
  DATA_STORE <- NULL
}

tabs <- list()

tabs[["title"]] = ""

for (i in 2:(length(INDICATOR_CLASSES) + 2)) {
  tabs[[i]] = get_tab_panel(INDICATOR_CLASSES, i - 1)
}
