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

df_indicators <- definitions %>%
  left_join(indicators, by = c("class", "indicator_name", "type")) %>%
  select(indicator_name, group_names, filename, date_added, date_dropped, source, download, frequency, disabled) %>%
  filter(disabled == FALSE | is.na(disabled)) %>%
  select(-disabled) %>%
  mutate(date_added = dmy(date_added)) %>%
  filter(!is.na(date_added)) %>%
  mutate(month = floor_date(date_added, unit = "month"))

series_vec <- c("region", "gender", "age", "ethnic", "sex", "industry")
for (vec in series_vec) {
  df_indicators[[vec]] <- str_detect(df_indicators$indicator_name, vec) | str_detect(df_indicators$group_names, vec)
}
df_indicators$gender <- df_indicators$gender | df_indicators$sex
df_indicators <- select(df_indicators, - sex)
df_indicators$other <- df_indicators$group_names != "undefined_name" & !(df_indicators$region | df_indicators$gender | df_indicators$age | df_indicators$ethnic | df_indicators$industry)

df_indicators$source_internal <- str_detect(pattern = "Stats", string = df_indicators$source)


#Monthly cumulative count of indicators number------------------------------------------
#add dots for indicators added that particular month!!!
ind_count <- df_indicators %>%
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

  ### get download reports for each month starting from March
downloads_count <- data.frame("month" = c(), "download_count" = c())
for (file in rownames(file.info(list.files(downloads_monthly_folder, full.names = T)))) {
  print(file)
  month <- str_extract(file, "\\d\\d\\d\\d-\\d\\d-\\d\\d")
  print(month)
  downloads <- as.data.frame(read.csv(file)) %>%
    filter(Event.Category == "Data Downloaded") %>%
    mutate(month = ymd(month)) %>%
    select(month, Total.Events)
  print(str(downloads))
  names(downloads) <- c("month", "download_count")
  print(str(downloads))
  downloads_count <- rbind(downloads_count, downloads)
}

df_events <- ind_count %>%
  left_join(views_count) %>%
  left_join(downloads_count)



  ### get monthly views per each indicator

df_ind_views <- tibble("indicator_name" = c(), "ind_views" = c(), "month" = c())

  for (file in rownames(file.info(list.files(views_monthly_folder, full.names = T)))) {
    month <- str_extract(file, "\\d\\d\\d\\d-\\d\\d-\\d\\d")
    views <- as.data.frame(read.csv(file)) %>%
      separate(col = Indicator, into = c("class", "type", "indicator_name", "group"), sep = "_") %>%
      select(indicator_name, Total.Events) %>%
      group_by(indicator_name) %>%
      mutate(ind_views = sum(Total.Events)) %>%
      mutate(month = ymd(month)) %>%
      select(-Total.Events) %>%
      ungroup() %>%
      unique()
    #names(views)[length(names(views))] <- month
   df_ind_views <- rbind(df_ind_views, views)
  }


#Number of currently available indicators:
indNumber <- df_indicators %>%
  select(indicator_name) %>%
  unique() %>%
  nrow()

#Which indicators have been added / dropped this month?--------------------------------------------------
added <- unique(df$indicator_name[df$date_added >= latest_report_date])
dropped <- unique(df$indicator_name[!is.na(df$date_dropped) & dmy(df$date_dropped) >= latest_report_date])


