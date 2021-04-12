library(lubridate)
library(jsonlite)

current_date <- today()
latest_report_date <- rollback(current_date)

indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)

#Number of indicators (single point)------------------------------------------------------------
indNumber <- length(unique(indicators$indicator_name[indicators$disabled == FALSE | is.na(indicators$disabled)]))

#Which indicators have been added this month?--------------------------------------------------
added <- unique(definitions$indicator_name[definitions$])
