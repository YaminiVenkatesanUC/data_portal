library(xlsx)

cffc <- as.data.frame(read_excel("example_data/ORD-551132-J1V4_Covid19 Wellbeing W2_MAIN_Final Data_Excel_v1.xlsx", sheet = 1))
cffc <- cffc %>% filter(a1 == 1)
map <- as.data.frame(read_excel("example_data/ORD-551132-J1V4_Covid19 Wellbeing W2_MAIN_Final Data_Excel_v1.xlsx", sheet = 2))

colNames <- c("Code", "Category", "Percentage")
values <- rep(NA, length(colNames))
data_all <- data.frame(colNames = colNames, values = values) %>%
  spread(key = colNames, value = values)
write.xlsx(x = data_all, file = "CFFC_test.xlsx", sheetName = "null", col.names = TRUE, row.names = FALSE)

series_unique <- c("a3_wb", "b18_wb_1", "c101", "c17", "Anxiety")
series_corona <- c("corona_1", "corona_2", "corona_3")
series_b19 <- c("b19_wb11", "b19_wb12", "b19_wb13", "b19_wb14", "b19_wb15", "b19_wb16", "b19_wb17", "b19_wb18", "b19_wb19")
#series2 <- c("b19_wb", "corona")

cffc_unique <- function(cffc, map, series) {
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

cffc_corona <- function(cffc, map, series) {
  percents <- c()
  df <- cffc %>% select(starts_with(series)) %>%
    filter(corona_1c4 != 0) %>%
    select(ends_with(c("c1", "c2")))
  percents <- c()
  print(sum(df$))


  for (col in names(df)) {
    sum <- sum(df[[col]])
    total <- nrow(df[col])
    percent <- sum * 100 / total
    percents <- c(percents, percent)

    #print(col)
    #print(sum(df[[col]]))
    #print(sum(df$col))
    #df <- df %>% mutate(paste0(col, "_sum") = sum(df[[col]]))
  }
  print(percents)
}



  # for (ser in series) {
  #   df <- cffc %>% select(ser)
  #   print(names(df))
    # map_index <- str_which(string = map$`[record]: Record number`, pattern = ser)
    # values <- map$`[record]: Record number`[map_index+1] %>%
    #   substr(start = 11, stop = 11) %>%
    #   as.numeric()
    # start <- map_index+2
    # end <- start + values - 1
    # ser_map <- map %>% slice(start:end) %>% select(...2, ...3)
    # names(ser_map) <- c("Code", "Category")
    # names(df) <- c("Code")
    # ser_map$Code <- as.numeric(ser_map$Code)
    # df <- df %>%
    #   left_join(ser_map) %>%
    #   mutate(total = n()) %>%
    #   group_by(Code, Category) %>%
    #   mutate(count = n()) %>%
    #   mutate(Percentage = round(count * 100 / total, 1)) %>%
    #   unique() %>%
    #   arrange(Code) %>%
    #   select(-total, -count)
    # df <- as.data.frame(df)
    # write.xlsx(x = df,
    #            file = "CFFC_test.xlsx",
    #            sheetName = ser,
    #            col.names = TRUE,
    #            row.names = FALSE,
    #            append = TRUE)
    #
  #}
# }

cffc_unique(cffc = cffc, map = map, series = series_unique)
cffc_aggr(cffc = cffc, map = map, series = series_corona)
