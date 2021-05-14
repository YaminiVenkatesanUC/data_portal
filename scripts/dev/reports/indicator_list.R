library(jsonlite)

definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)
indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
odata <- read_json(path = "config/covid_19/covid_19_odata_definitions_v2.json", simplifyVector = TRUE)

data <- definitions %>%
  left_join(indicators, by = c("class", "type", "indicator_name")) %>%
  left_join(odata, by = c("type" = "Subject", "indicator_name" = "Title")) %>%
  select(ResourceID,
         class,
         type,
         indicator_name,
         Description,
         Measure,
         value_names,
         groups,
         filename,
         date_added,
         sourcing,
         source,
         international,
         source_url,
         download,
         frequency,
         Duration,
         caveats) %>%
  unique() %>%
  group_by(indicator_name) %>%
  mutate(value_names = list(value_names)) %>%
  unique() %>%
  mutate(API_loaded = !is.na(ResourceID)) %>%
  select(class,
         type,
         indicator_name,
         source,
         source_url,
         sourcing,
         international,
         download,
         frequency,
         filename,
         value_names,
         groups,
         date_added,
         caveats,
         API_loaded,
         ResourceID,
         Description,
         Measure,
         Duration
         )

for (pattern in c("<p>", "</p>", "<ol>", "</ol>", "<li>", "</li>")) {
  data$caveats <- str_remove_all(data$caveats, pattern)
}


openxlsx::write.xlsx(x = data, "COVID-19 data portal - Indicators.xlsx")
