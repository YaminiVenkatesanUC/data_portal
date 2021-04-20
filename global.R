library(dplyr)
library(configr)
library(highcharter)
library(shinyjs)
library(readxl)
library(lubridate)
library(tidyr)
library(stringr)
library(jsonlite)
library(R6)
library(httr)
library(data.table)
library(tibble)
library(gtools)
library(readr)


source("R/core/build_ui.R")
source("R/core/utils.R")
source("R/core/download_modal.R")
source("R/core/about_modal.R")
source("R/core/main_panel.R")
source("R/core/consts.R")
source("R/core/get_download_csv.R")
source("R/core/type_checks.R")
source("R/core/data_interface.R")
source("R/core/filter_indicators.R")

source("R/load_functions.R")
source("R/plot_functions.R")
source("R/data_types.R")
source("R/data_service_functions.R")

options(scipen = 999)

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

DOWNLOADABLE_INDICATORS <- names(indicator_definitions)[
  as.vector(sapply(indicator_definitions, function(x) (!is.null(x$download) && x$download)))
  ]

if (!is.null(CONFIG$production) & !CONFIG$production) {
  warnings(DEV_MODE_WARNING)
  DATA_STORE <- load_data(CONFIG)
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

if (!is.null(CONFIG$tag_manager_html)) {
  tag_manager_html <- includeHTML("www/tag_manager.html")
} else {
  print("No tag manager set.")
  tag_manager_html <- "<div></div>"
}

if (!is.null(CONFIG$filter_dictionary_definitions)) {
  filter_dictionary_raw <- read_json(CONFIG$filter_dictionary_definitions)
  FILTER_DICTIONARY <- list()
  for (item in filter_dictionary_raw) {
    FILTER_DICTIONARY[[item$name]] <- item
  }
}
