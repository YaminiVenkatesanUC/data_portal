library(tidyr)
library(writexl)

filename <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/MBIE/rentalbond-data-regional.csv"

#filename <- "example_data/rentalbond-data-regional.csv"
sub_series <- c("Active Bonds", "Closed Bonds", "Lodged Bonds", "Average Weekly Rent")

  data <- as.data.frame(read_csv(file = filename)) %>%
    select(`Time Frame`, Location, `Lodged Bonds`, `Active Bonds`, `Closed Bonds`, `Average Weekly Rent`) %>%
    filter(Location != "NA")

  data$`Time Frame` <- as.Date(data$`Time Frame`, "%b %d %Y")
  data$Location <- str_remove_all(data$Location, " Region")
  data <- arrange(.data = data, `Time Frame`)

  for (series in sub_series) {
    print(series)
    df <- data %>%
      filter(`Time Frame` > "2015-01-01") %>%
      select(`Time Frame`, Location, series) %>%
      pivot_wider(names_from = Location, values_from = series) %>%
      select(`Time Frame`, ALL, everything())
    #write_csv(x = df, file = paste0("example_data/COVID-19 MBIE Rental ", series, ".csv"))
    write_csv(x = df, file = paste0("~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MBIE Rental ", series, ".csv"))
  }
