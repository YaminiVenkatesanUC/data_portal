get_x_values <- function(dates) {
  return (ifelse(format(dates) == "double", as.character(format(dates, "%d-%m-%y")), as.character(dates)))
}

get_download_csv <- function() {
  downloadable_indicators <- names(indicator_definitions)[
    as.vector(sapply(indicator_definitions, function(x) (!is.null(x$download) && x$download)))
  ]

  output <- data.frame(
    class = c(),
    category = c(),
    indicator_name = c(),
    series_name = c(),
    sub_series_name = c(),
    parameter = c(),
    value = c(),
    units = c(),
    date_last_updated = c()
  )

  for (item_name in downloadable_indicators) {
    item <- indicator_definitions[[item_name]]
    
    sub_series <- data.frame(
      series_name = c(),
      sub_series_name = c(),
      value = c(),
      parameter = c(),
      units = c()
    )
    for (i in 1:length(item$groups)) {
      group <- item$groups[[i]]
      key <- paste(item$class, item$type, item$indicator_name, group$name, sep = "_")
      data_object <- DATA_STORE[[key]]
      sub_sub_series <- data_object$get_csv_content()

      sub_sub_series$series_name <- gsub(" \U2012 ", " - ", group$name)
      
      if (!is.null(group$units)) {
        sub_sub_series$units <- group$units
      } else if (!is.null(item$units)) {
        sub_sub_series$units <- item$units
      } else {
        sub_sub_series$units <- ""
      }
      sub_series <- rbind(sub_series, sub_sub_series)
    }

    sub_series$indicator_name <- gsub(" \U2012 ", " - ", item$indicator_name)
    sub_series$class <- item$class
    sub_series$category <- item$type
    sub_series$date_last_updated <- format(item$update_date, "%d-%m-%y")
    output <- rbind(sub_series, output)
  }

  output <- output %>% select(c("class", "category", "indicator_name", "series_name",  "sub_series_name", "parameter", "value", "units"))

  return (output)
}
