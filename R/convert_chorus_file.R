library(openxlsx)
library(foreach)
library(readxl)

convert_chorus_file <- function (input_files, output_directory) { 
  data <- foreach(i = 1:length(input_files), .combine=rbind) %do% {
    as.data.frame(read_excel(
      input_files[[i]],
      sheet = 1,
      skip = 6,
      col_names = c("date", "time", "egress", "ingress", "total"),
    ), stringAsFactors = FALSE)
  }
  data$hour <- substr(data$time, 1, 2)
  
  data <- aggregate(total ~ date, data = data, FUN = sum)
  data$date_time <- paste0(data$date)
  data$date_parsed <- parse_date_time(data$date_time, "%Y-%m-%d")
  data <- data %>% arrange(date_parsed) %>% select("date_time", "total")

  write.xlsx(data, paste0(output_directory, "COVID 19 - Chorus broadband.xlsx"), row.names = FALSE)
}

input_files <- c(
  paste0(config$data_directory, "5 Minute Usage Data 20200201-20200522.xlsx")
)


convert_chorus_file(input_files, config$data_directory)

