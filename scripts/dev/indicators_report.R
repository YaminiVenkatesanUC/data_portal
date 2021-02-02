library(jsonlite)
library(writexl)

file <- "config/covid_19/covid_19_indicators.json"

df <- read_json(path = file, simplifyVector = TRUE)

write_xlsx(x = df, path = "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/COVID_indicators_report.xlsx")
