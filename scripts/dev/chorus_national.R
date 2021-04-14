filename <- "/Chorus data/"
input_files <- list.files(paste0(CONFIG$data_directory, filename))

# only assign colnames according to name in csv
# check to make sure the df appending to same cols
data <- foreach(i = 1:length(input_files), .combine = rbind) %do% {
  print(paste0(CONFIG$data_directory, filename, "/", input_files[[i]]))
  output <- as.data.frame(read_excel(
    paste0(CONFIG$data_directory, filename, "/", input_files[[i]]),
    sheet = 1,
    skip = 4,
    col_names = c("date", "time", "egress", "ingress", "total")
  ), stringAsFactors = FALSE)
  if (!all(!is.na(output$date))) {
    output <- output %>%
      filter(
        !(date %in% c("Grand Total"))
      ) %>%
      mutate(
        date_parsed = ifelse(
          is.na(date),
          as.Date(as.numeric(date[[1]]), origin = "1899-12-30"),
          as.Date(as.numeric(date), origin = "1899-12-30")
        )
      ) %>%
      select(-c("date"))

    output$date <- as.Date(as.numeric(output$date_parsed),  origin = "1970-01-01")
    output <- output %>% select(-c("date_parsed"))
  }
  output
}

data$hour <- substr(data$time, 1, 2)
data <- aggregate(total ~ date, data = data, FUN = sum)
data$Parameter <- ymd(paste0(data$date))
data <- data %>% arrange(Parameter) %>% select("Parameter", "total")

write.csv(x = data, file = paste0(CONFIG$data_directory, "COVID 19 - Chorus national broadband.csv"), row.names = FALSE)

