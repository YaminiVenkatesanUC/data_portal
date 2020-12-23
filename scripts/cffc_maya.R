library(xlsx)

cffc <- as.data.frame(read_excel("example_data/ORD-551132-J1V4_Covid19 Wellbeing W2_MAIN_Final Data_Excel_v1.xlsx", sheet = 1))
cffc <- cffc %>% filter(a1 == 1)
map <- as.data.frame(read_excel("example_data/ORD-551132-J1V4_Covid19 Wellbeing W2_MAIN_Final Data_Excel_v1.xlsx", sheet = 2))

colNames <- c("Code", "Category", "Percentage")
values <- rep(NA, length(colNames))
data_all <- data.frame(colNames = colNames, values = values) %>%
  spread(key = colNames, value = values)
write.xlsx(x = data_all, file = "CFFC_test.xlsx", sheetName = "null", col.names = TRUE, row.names = FALSE)

series <- c("a3_wb", "b18_wb_1", "c101", "c17", "Anxiety")
#series2 <- c("b19_wb", "corona")

create_cffc <- function(cffc, map, series) {
  for (ser in series) {
    df <- cffc %>% select(ser)
    map_index <- str_which(string = map$`[record]: Record number`, pattern = ser)
    values <- map$`[record]: Record number`[map_index+1] %>%
      substr(start = 11, stop = 11) %>%
      as.numeric()
    start <- map_index+2
    end <- start + values - 1
    ser_map <- map %>% slice(start:end) %>% select(...2, ...3)
    names(ser_map) <- c("Code", "Category")
    names(df) <- c("Code")
    ser_map$Code <- as.numeric(ser_map$Code)
    df <- df %>%
      left_join(ser_map) %>%
      mutate(total = n()) %>%
      group_by(Code, Category) %>%
      mutate(count = n()) %>%
      mutate(Percentage = round(count * 100 / total, 1)) %>%
      unique() %>%
      arrange(Code) %>%
      select(-total, -count)
    df <- as.data.frame(df)
    write.xlsx(x = df,
               file = "CFFC_test.xlsx",
               sheetName = ser,
               col.names = TRUE,
               row.names = FALSE,
               append = TRUE)
  }
}

create_cffc(cffc = cffc, map = map, series = series)
