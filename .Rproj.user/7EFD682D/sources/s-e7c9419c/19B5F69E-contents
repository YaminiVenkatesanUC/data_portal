
###### Global flight movements ####
library(xlsx)
path <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/"
filename <- "number-of-commercial-fli.csv"

master_file <- read_excel(paste0(path,"Development/COVID 19 - Flightradar24 Number of commercial flights.xlsx"),skip = 1)

flight_movements <- function(path, filename){
  global_flights <-
    read.csv(
      paste0(path,filename),
      stringsAsFactors = FALSE
    )
  # Split 2020 and 2021 data
  # Append 2020 and 2021
  flight_data_2021 <- global_flights[,1:3]
  names(flight_data_2021) <- c("Date","7-day moving average","Daily number")
  flight_data_2021 <- flight_data_2021 %>% select("Date","Daily number","7-day moving average") %>% na.omit(flight_data_2021)

  flight_data_2020 <- global_flights[,c(1,4,5)]
  flight_data_2020[,"Date"] <- format(as.Date(flight_data_2020$DateTime),"%m-%d")
  flight_data_2020$Date <- sprintf("2020-%s", flight_data_2020$Date)
  flight_data_2020 <- flight_data_2020 %>% select("Date","X2020.Number.of.flights","X2020.7.day.moving.average")
  names(flight_data_2020) <- c("Date","Daily number","7-day moving average")

  global_data <- rbind(flight_data_2020,flight_data_2021)

  global_data$`Daily number` <- formatC(global_data$`Daily number`, format="d", big.mark=",")
  global_data$`7-day moving average` <- formatC(global_data$`7-day moving average`, format="d", big.mark=",")


  write.xlsx(global_data, file = "COVID 19-Flightradar24 Number of commercial flights.xlsx",row.names = FALSE)

}


#### RBNZ Financial Indicators ###
path <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/"
filename <- "hb1-daily.xlsx"

rbnz_data <- function(path, filename) {
  data <- as.data.frame(
    read_excel(
      paste0(path, filename),
      sheet = "Data",
      col_names = c("Date", "TWI"),
      #col_types = "numeric",  ## More warnings()
      range = cell_cols(1:2),
      .name_repair = "unique"
    )
  )
  # skipping rows
  data <- data[-c(1:5), ]
  # Convert column types
  data$Date <- as.numeric(data$Date)
  data$TWI <- as.numeric(data$TWI)
  # changine code to dates
  data$Date <- as.POSIXct(data$Date * (60 * 60 * 24)
                          , origin = "1899-12-30"
                          , tz = "GMT")
  data$TWI <- as.numeric(format(round(data$TWI, 2)))

  data <-
    data %>% mutate(Date = format(as.Date(data$Date), "%d/%m/%Y"))

  write.xlsx(data, file = "COVID 19-RBNZ Financial indicators.xlsx", row.names = FALSE)

}