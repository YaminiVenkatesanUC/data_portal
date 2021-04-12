library(lubridate)
library(jsonlite)

current_date <- today()
latest_report_date <- rollback(current_date)

indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)

#Number of indicators (single point)------------------------------------------------------------
indNumber <- length(unique(indicators$indicator_name[indicators$disabled == FALSE | is.na(indicators$disabled)]))

#Which indicators have been added / dropped this month?--------------------------------------------------
added <- unique(definitions$indicator_name[dmy(definitions$date_added) > latest_report_date %m-% months(1)])
dropped <- unique(definitions$indicator_name[!is.na(definitions$date_dropped) & dmy(definitions$date_dropped) > latest_report_date %m-% months(1)])

#Number of indicators change over time
monthly_count <- definitions %>%
  filter(dmy(date_added) > dmy("31-07-2020")) %>%
  mutate(reporting_period = ceiling_date(date_added, unit = "month")) %>%
  select(indicator_name, reporting_period) %>%
  unique() %>%
  group_by(reporting_period) %>%
  mutate(count = n())
