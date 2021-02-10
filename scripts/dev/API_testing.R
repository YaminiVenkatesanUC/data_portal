CONFIG <- read_config_file()
indicator <- read_json(CONFIG$indicator_definitions)[[1]]

Resource <- GET(
  URLencode(paste0(CONFIG$odata_url,
                   "Covid-19Indicators/Resources",
                   "?$filter=(ResourceID eq '",
                   indicator$api_resource_id,
                   "')")),
  add_headers("Ocp-Apim-Subscription-Key" = CONFIG$odata_token))  %>%
  content("text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE)

Observation <- GET(
  URLencode(paste0(CONFIG$odata_url,
                   "Covid-19Indicators/Observations",
                   "?$filter=(ResourceID eq '",
                   indicator$api_resource_id,
                   "')")),
  add_headers("Ocp-Apim-Subscription-Key" = CONFIG$odata_token))  %>%
  content("text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE)

Resources_all <- GET(
  URLencode(paste0(CONFIG$odata_url,
                   "Covid-19Indicators/Resources")),
  add_headers("Ocp-Apim-Subscription-Key" = CONFIG$odata_token))  %>%
  content("text", encoding = "UTF-8") %>%
  jsonlite::fromJSON(flatten = TRUE)

View(select(Resources_all$value,ResourceID,Subject,Title)) # View resources and IDS