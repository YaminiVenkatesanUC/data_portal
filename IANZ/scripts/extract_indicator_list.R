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

source("R/core/build_ui.R")
source("R/core/utils.R")
source("R/core/download_modal.R")
source("R/core/about_modal.R")
source("R/core/main_panel.R")
source("R/core/consts.R")
source("R/core/get_download_csv.R")
source("R/core/type_checks.R")
source("R/core/data_interface.R")

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

output <- data.frame(
  class = c(),
  category = c(),
  indicator_name = c(),
  series_name = c(),
  sub_series_name = c(),
  date_last_updated = c(),
  source = c()
)


for (item_name in names(indicator_definitions)) {
  item <- indicator_definitions[[item_name]]
  sub_series <- data.frame(
    series_name = c(),
    source = c()
  )
  for (i in 1:length(item$groups)) {
    group <- item$groups[[i]]
    sub_sub_series <- data.frame(
      series_name = gsub(" \U2012 ", " - ", group$name),
      source = get_definition_parameter("source", item, group)
    )

    sub_series <- rbind(sub_series, sub_sub_series)
  }
  key <- paste(item$class, item$type, item$indicator_name, group$name, sep = "_")
  data_object <- DATA_STORE[[key]]
  sub_series$indicator_name <- gsub(" \U2012 ", " - ", item$indicator_name)
  sub_series$class <- item$class
  sub_series$category <- item$type
  sub_series$date_last_updated <- format(data_object$update_date, "%d-%m-%y")
  output <- rbind(sub_series, output)
}

output <- select(
  output,
  c("class", "category", "indicator_name", "series_name", "source", "date_last_updated")
)

write.csv(output, "indicator_register.csv", row.names = FALSE)