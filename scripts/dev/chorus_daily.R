# Broadband usage

library(readxl)
library(dplyr)

config <- read_config_file()
path <- paste0(config$data_directory, "Chorus data/")
files <- file.info(list.files(path, full.names = T))

data <- data.frame(Cal.Dt = Date(), total = numeric())

for (file in rownames(files)) {
  df <- read_excel(file,
                   skip = 3,
                   .name_repair = "universal",
                   col_types = c("date", "text", "numeric", "numeric", "numeric")) %>%
    filter(!is.na(Start.Of.5min.Time.24hr))
if (any(is.na(df$Cal.Dt))) {
  df$Cal.Dt <- df$Cal.Dt[[1]]
}
  df <- df %>% select(Cal.Dt, `...5`)
  data <- rbind(data, df)
}

data <- data %>%
  group_by(Cal.Dt) %>%
  summarize(total = sum(`...5`)) %>%
  arrange(Cal.Dt)

names(data) <- c("Date", "Total GB")

file.rename(from = paste0(config$data_directory, "COVID 19 - Chorus daily broadband.csv"),
            to = paste0(config$data_directory, "Previous/COVID 19 - Chorus daily broadband.csv"))
write.csv(x = data, row.names = FALSE, file = paste0(config$data_directory, "COVID 19 - Chorus daily broadband.csv"))
