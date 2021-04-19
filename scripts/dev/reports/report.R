library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyr)

current_date <- today()
latest_report_date <- floor_date(rollback(current_date), unit = "month")

indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)
#download Unique page views over time 01-03-2020 to 31-03-2021 // Google analytics
views_total <- read.csv("~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Maya/reports/total_views.csv")
views_total$Date <- as.Date(views_total$Date, format = "%b %d, %Y")

views_monthly_folder <- "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Maya/reports/ind_views"
downloads_monthly_folder <- "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Maya/reports/downloads"

df <- definitions %>%
  left_join(indicators, by = c("class", "indicator_name", "type")) %>%
  select(indicator_name, group_names, filename, date_added, date_dropped, source, download, frequency, disabled) %>%
  filter(disabled == FALSE | is.na(disabled)) %>%
  select(-disabled) %>%
  mutate(date_added = dmy(date_added)) %>%
  filter(!is.na(date_added)) %>%
  mutate(month = floor_date(date_added, unit = "month"))


#Monthly cumulative count of indicators number------------------------------------------
#add dots for indicators added that particular month!!!
ind_count <- df %>%
  group_by(month) %>%
  mutate(ind_count = n()) %>%
  select(month, ind_count) %>%
  unique() %>%
  arrange(month) %>%
  within(ind_count_cum <- cumsum(ind_count))

views_count <- views_total %>%
  mutate(month = floor_date(Date, unit = "month")) %>%
  group_by(month) %>%
  mutate(views_count = sum(Unique.Page.Views)) %>%
  select(-Unique.Page.Views, -Date) %>%
  unique() %>%
  arrange(month) %>%
  within(views_count_cum <- cumsum(views_count))

df <- df %>%
  left_join(ind_count) %>%
  left_join(views_count)


  ### get monthly views per each indicator

  for (file in rownames(file.info(list.files(views_monthly_folder, full.names = T)))) {
    print(file)
    month <- str_extract(file, "\\d\\d\\d\\d-\\d\\d-\\d\\d")
    views <- as.data.frame(read.csv(file)) %>%
      separate(col = Indicator, into = c("class", "type", "indicator_name", "group"), sep = "_") %>%
      select(indicator_name, Total.Events) %>%
      group_by(indicator_name) %>%
      mutate(ind_views = sum(Total.Events)) %>%
      select(-Total.Events) %>%
      unique()
    names(views)[length(names(views))] <- month
    df <- df %>%
      left_join(views, by = c("indicator_name"))
  }

df <- df %>%
  pivot_longer(cols = starts_with("ind_views"), names_to = "names", values_to = "values")

downloads_count <-
### get download reports for each month starting from March



df <- left_join(df, monthly_total)



#Number of currently available indicators:
indNumber <- df %>%
  select(indicator_name) %>%
  unique() %>%
  nrow()

#Which indicators have been added / dropped this month?--------------------------------------------------
added <- unique(df$indicator_name[df$date_added >= latest_report_date])
dropped <- unique(df$indicator_name[!is.na(df$date_dropped) & dmy(df$date_dropped) >= latest_report_date])


