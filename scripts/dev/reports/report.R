library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyr)

current_date <- today()
latest_report_date <- floor_date(rollback(current_date), unit = "month")

indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)
definitions$date_added <- dmy(definitions$date_added)
print(definitions$indicator_name[is.na(definitions$date_added)])
definitions <- definitions %>% filter(!is.na(date_added))

#Number of indicators (single point)------------------------------------------------------------
indNumber <- length(unique(indicators$indicator_name[indicators$disabled == FALSE | is.na(indicators$disabled)]))

#Which indicators have been added / dropped this month?--------------------------------------------------
added <- unique(definitions$indicator_name[definitions$date_added >= latest_report_date])
dropped <- unique(definitions$indicator_name[!is.na(definitions$date_dropped) & dmy(definitions$date_dropped) >= latest_report_date])

#Number of indicators added each month
monthly_added <- definitions %>%
  mutate(reporting_period = floor_date(date_added, unit = "month")) %>%
  select(indicator_name, reporting_period) %>%
  unique() %>%
  group_by(reporting_period) %>%
  mutate(count = n())

#Total number of indicators each month------------------------------------------
#add dots for indicators added that particular month!!!
monthly_total <- definitions %>%
  mutate(reporting_period = floor_date(date_added, unit = "month")) %>%
  select(indicator_name, reporting_period) %>%
  unique() %>%
  group_by(reporting_period) %>%
  mutate(count = n()) %>%
  select(reporting_period, count) %>%
  unique() %>%
  arrange(reporting_period)

monthly_total <- within(monthly_total, acc_sum <- cumsum(count))

#Total number of unique page views over time-------------------------------------
#download Unique page views over time 01-03-2020 to 31-03-2021 // Google analytics

report_timeseries <- read.csv("scripts/dev/reports/COVID-19 Data portal analytics_Stats NZ web page dashboard_Time series.csv")
report_timeseries$Date <- as.Date(report_timeseries$Date, format = "%b %d, %Y")

monthly_views <- report_timeseries %>%
  mutate(reporting_period = floor_date(Date, unit = "month")) %>%
  group_by(reporting_period) %>%
  mutate(count = sum(Unique.Page.Views)) %>%
  select(-Unique.Page.Views, -Date) %>%
  unique() %>%
  arrange(reporting_period)

monthly_views <- within(monthly_views, acc_sum <- cumsum(count))

#Total number of visits this month +++ Percentage change-------------------------------
latest_views <- monthly_views$count[monthly_views$reporting_period == latest_report_date]
previous_views <- monthly_views$count[monthly_views$reporting_period == latest_report_date %m-% months(1)]
perc_change <- round(latest_views / previous_views * 100 - 100, 1)

#Most visited indicators this month----------------------------------------------------
#download Unique page views over time 01-03-2020 to 31-03-2021 // Google analytics
#visualize as histogram +++ list???
report_month <- read.csv("scripts/dev/reports/COVID-19 Data portal analytics_Stats NZ web page dashboard_Table.csv")
report_month <- report_month %>%
  separate(col = Indicator, into = c("class", "type", "indicator_name", "group"), sep = "_") %>%
  select(indicator_name, Total.Events) %>%
  group_by(indicator_name) %>%
  mutate(count = sum(Total.Events)) %>%
  select(-Total.Events) %>%
  unique()
top_visited <- report_month %>%
  arrange(desc(Total.Events)) %>%
  head(n = 10)

#Indicators not visited this month at all----------------------------------------------
not_visited <- definitions %>%
  select(class, type, indicator_name) %>%
  left_join(report_month) %>%
  filter(is.na(count)) %>%
  unique()



