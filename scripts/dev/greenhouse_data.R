# All industries & households (total) - quarterly +++ Broad industry group - quarterly +++
# Industry - quarterly +++ Total households - quarterly


library(readr)

data <- read.csv("~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/Portal Data Supply/SEEA GHG Emissions/Dec-2020-data-for-portal.csv")

data <-
  data %>% mutate(quarter = gsub("\\d{4}", "", as.character(data$YearQ)))  %>% mutate(year = substr(data$YearQ, 1, 4)) %>% mutate(
    Month = case_when(
      quarter == "1" ~ "03-01",
      quarter == "2" ~ "06-01",
      quarter == "3" ~ "09-01",
      quarter == "4" ~ "12-01"
    )
  )


data <- data %>%
  unite(date, c(year, Month), sep = "-", remove = FALSE)  %>% select(date,Series,data_value)


data$date <- as.Date(data$date)

data <- data %>% pivot_wider(names_from = Series, values_from = data_value)

file.rename(from = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 Greenhouse Emissions.csv",
            to = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Previous/COVID-19 Greenhouse Emissions.csv")

write_csv(x = data, file = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 Greenhouse Emissions.csv")

