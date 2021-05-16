# Rental bonds by region - active
# Rental bonds by region - closed
# Rental bonds by region - lodged

library(tidyr)
library(dplyr)
library(stringr)
library(writexl)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "MBIE/")
filename <- paste0(path, "rentalbond-data-regional.csv")

sub_series <- c("Active Bonds", "Closed Bonds", "Lodged Bonds", "Average Weekly Rent")

  data <- as.data.frame(read.csv(file = filename, check.names = FALSE)) %>%
    select(`Time Frame`, Location, `Lodged Bonds`, `Active Bonds`, `Closed Bonds`, `Average Weekly Rent`) %>%
    filter(Location != "NA")

  data$`Time Frame` <- as.Date(data$`Time Frame`, "%Y-%m-%d")
  data$Location <- str_remove_all(data$Location, " Region")
  data <- arrange(.data = data, `Time Frame`)

  for (series in sub_series) {
    df <- data %>%
      filter(`Time Frame` > "2015-01-01") %>%
      select(`Time Frame`, Location, series) %>%
      pivot_wider(names_from = Location, values_from = series) %>%
      select(`Time Frame`, `ALL`, everything())

    for (col in 2:ncol(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
    #write_csv(x = df, file = paste0("example_data/COVID-19 MBIE Rental ", series, ".csv"))
    file.rename(from = paste0("~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MBIE Rental ", series, ".csv"),
                to = paste0("~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Previous/COVID-19 MBIE Rental ", series, ".csv"))
    write.csv(x = df, file = paste0("~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MBIE Rental ", series, ".csv"), row.names = FALSE)
  }
