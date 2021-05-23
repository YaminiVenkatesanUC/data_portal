library(lubridate)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)

current_date <- today()
latest_report_date <- floor_date(rollback(current_date), unit = "month")

indicators <- read_json(path = "config/covid_19/covid_19_indicators.json", simplifyVector = TRUE)
definitions <- read_json(path = "config/covid_19/covid_19_data_definitions.json", simplifyVector = TRUE)
odata <- read_json(path = "config/covid_19/covid_19_odata_definitions_v2.json", simplifyVector = TRUE)

#download Unique page views over time 01-03-2020 to 31-03-2021 // Google analytics
views_total <- read.csv("~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Maya/reports/total_views.csv")
views_total$Date <- as.Date(views_total$Date, format = "%b %d, %Y")

views_monthly_folder <- "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Maya/reports/ind_views"
downloads_monthly_folder <- "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Maya/reports/downloads"

df_indicators <- definitions %>%
  left_join(indicators, by = c("class", "indicator_name", "type")) %>%
  left_join(odata, by = c("type" = "Subject", "indicator_name" = "Title")) %>%
  select(
    indicator_name,
    group_names,
    filename,
    date_added,
    date_dropped,
    source,
    download,
    frequency,
    disabled,
    script,
    sourcing,
    ResourceID) %>%
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
df_indicators$no_subseries <- !(df_indicators$region | df_indicators$gender | df_indicators$age | df_indicators$ethnic | df_indicators$industry | df_indicators$other)

df_indicators$source_internal <- str_detect(pattern = "Stats", string = df_indicators$source)

#Monthly cumulative count of indicators number------------------------------------------
#add dots for indicators added that particular month!!!
ind_count <- df_indicators %>%
  select(month, indicator_name) %>%
  unique() %>%
  group_by(month) %>%
  mutate(new_inds = n()) %>%
  select(month, new_inds) %>%
  unique()

views_count <- views_total %>%
  mutate(month = floor_date(Date, unit = "month")) %>%
  group_by(month) %>%
  mutate(views_count = sum(Unique.Page.Views)) %>%
  select(-Unique.Page.Views, -Date) %>%
  unique()

  ### get download reports for each month starting from March
downloads_count <- data.frame("month" = c(), "download_count" = c())
for (file in rownames(file.info(list.files(downloads_monthly_folder, full.names = T)))) {
  month <- str_extract(file, "\\d\\d\\d\\d-\\d\\d-\\d\\d")
  downloads <- as.data.frame(read.csv(file)) %>%
    filter(Event.Category == "Data Downloaded") %>%
    mutate(month = ymd(month)) %>%
    select(month, Total.Events)
  names(downloads) <- c("month", "download_count")
  downloads_count <- rbind(downloads_count, downloads)
}

df_events <- data.frame(month = seq.Date(from = as.Date("2020-03-01"), to = latest_report_date, by = "month")) %>%
  left_join(ind_count)
df_events$new_inds <- ifelse(is.na(df_events$new_inds), 0, df_events$new_inds)
df_events <- df_events %>%
  arrange(month) %>%
  within(ind_count_cum <- cumsum(new_inds))

df_events <- df_events %>%
  left_join(views_count)
df_events$views_count <- ifelse(is.na(df_events$views_count), 0, df_events$views_count)
df_events <- df_events %>%
  arrange(month) %>%
  within(views_count_cum <- cumsum(views_count))

df_events <- df_events %>%
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
   df_ind_views <- rbind(df_ind_views, views)
  }

#Number of currently available indicators:
indNumber <- df_indicators %>%
  select(indicator_name) %>%
  unique() %>%
  nrow()

#Which indicators have been added / dropped this month?--------------------------------------------------
added <- unique(df_indicators$indicator_name[df_indicators$date_added >= latest_report_date])
dropped <- unique(df_indicators$indicator_name[!is.na(df_indicators$date_dropped) & dmy(df_indicators$date_dropped) >= latest_report_date])


