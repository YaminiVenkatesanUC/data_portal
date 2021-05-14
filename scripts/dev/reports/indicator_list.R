library(jsonlite)

definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)
indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
odata <- read_json(path = "config/covid_19/covid_19_odata_definitions_all.json", simplifyVector = TRUE)

data <- definitions %>%
  select(class, type, indicator_name, value_names, group_names, filename, date_added, sourcing, script)

data$indicator_name[is.na(data$sourcing)]
