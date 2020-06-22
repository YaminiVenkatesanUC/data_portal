read_from_csv <- function(config, directory) {
  parameter_transform <- eval(parse(text = config$parameter_transform))
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  cols_to_read <- c(config$parameter_col, unlist(config$value_col))
  data <- as.data.frame(read.csv(
    paste0(directory, config$filename)
  ))
  names(data) <- paste0("col_", 1:ncol(data))
  
  data <- data %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter)) %>%
    select(c("Parameter", paste0("col_", config$value_col)))
  
  if (!is.null(config$input_units)) {
    data[,2:ncol(data)] <- mapply("*", data[,2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  return (data_frame_to_data_object_helper(
    directory,
    config,
    data %>% arrange(Parameter)
  ))
}

read_from_excel <- function(config, directory) {
  parameter_transform <- eval(parse(text = config$parameter_transform))
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  cols_to_read <- c(config$parameter_col, unlist(config$value_col))
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2 + skip, min(cols_to_read)), c(NA,max(cols_to_read)))
  )) %>%
   dplyr::rename(
    Parameter = paste0("col_", config$parameter_col)
    ) %>%
  mutate(Parameter = parameter_transform(Parameter))
  data <- data[,cols_to_read - min(cols_to_read) + 1]
  
  if (!is.null(config$input_units)) {
    data[,2:ncol(data)] <- mapply("*", data[,2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  return (data_frame_to_data_object_helper(
    directory,
    config,
    data %>% arrange(Parameter)
    ))
}

read_border_crossing_data <- function(config, directory, data_col = 4) {
  data<- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", 1:data_col),
    range = cell_limits(c(2, 1), c(NA, data_col)),
    skip = 1
  )) %>% rename(
    Date = paste0("col_", config$date_col)
  ) %>%
    mutate(Date = ymd(Date))

  data <- data[,c(1, data_col)] %>%
    mutate(year = year(Date)) %>%
    mutate(day = yday(Date)) %>%
    rename(col_ = paste0("col_", data_col))
  
  output <- data %>% filter(year == 2020)
  
  output <- left_join(output, data %>% filter(year == 2019) %>% select(c("day", "col_")), by = c("day"), suffix = c("1", "2"))
  output <- filter(output, !is.na(col_1) & !is.na(col_2))

  return (data_frame_to_data_object_helper(
    directory,
    config,
    output %>% select(-c("year", "day"))
  ))
}

read_border_crossing_data_daily <- function(config, directory) {
  return (read_border_crossing_data(config, directory, data_col = 3))
}

read_monetary_policy_file <- function(config, directory) {
  cols_to_read <- 1:2
  data<- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2, min(cols_to_read)), c(NA,max(cols_to_read)))
  ))

  return (data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}


read_traffic_data <- function(config, directory) {
  cols_to_read <- 1:5
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2, min(cols_to_read)), c(NA,max(cols_to_read)))
  ))

  if(!is.null(config$region)){
    data <- data %>% filter(col_3 == config$region) %>%
      select(Parameter = col_1, everything(),-col_4) %>%
      mutate(Parameter = as.Date(ymd(Parameter))) %>%
      group_by(Parameter, col_2, col_3) %>%
      summarise(col_5 = sum(col_5)) %>%
      spread(col_2, col_5)

    col_names =c("Parameter", paste0("col_", 2:ncol(data)))
    colnames(data) <- col_names
    data <- data %>%
      select(-c("col_2"))
  }else{
    stop("Please specify a region for traffic data")
  }
  return (data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

read_global_cases_file <- function(config, directory) {
  cols_to_read <- 1:5
  data<- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2, min(cols_to_read)), c(NA,max(cols_to_read)))
  ))

  if (!is.null(config$region)) {
    data<- data %>%
      filter(col_3==config$region) %>%
      select(Parameter = col_1, everything(), -col_5) %>%
      mutate(Parameter = as.Date(ymd(Parameter))) %>%
      group_by(Parameter, col_2, col_3) %>%
      summarise(col_4 = sum(col_4)) %>%
      spread(col_2, col_4)

    col_names =c("Parameter", paste0("col_", 2:ncol(data)))
    colnames(data) <- col_names
  } else {
    data <- data %>%
      dplyr::rename(country = col_2, Parameter = col_1) %>%
      filter(country == config$country_filter) %>%
      mutate(Parameter = as.Date(ymd(Parameter))) %>%
      select(-c("country"))
  }
  return (data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}


petrol_read_file <- function(config, directory) {
  filename <- config$filename
  data <- as.data.frame(read_excel(
    paste0(directory, filename),
    skip= 1,
    sheet = 1,
    col_names = TRUE,
    .name_repair = "minimal"
  ))

  data <-data[names(data) != ""]
  names(data)[1:2] <- c("Company", "Type")

  data <- data %>%
    filter(!is.na(Company) & !is.na(Type)) %>%
    pivot_longer(cols = 3:ncol(data), names_to = "date") %>%
    mutate(Parameter = as.Date(date, format = "%B %d, %y"))
  
  output <- data.frame(Parameter = unique(data$Parameter), stringsAsFactors = FALSE) %>% arrange()
  for (i in 1:length(config$fuel_type)) {
    output[[paste("col", i)]] <- (data %>%
      filter(Type == config$fuel_type[[i]] & Company == config$company_name[[i]]) %>%
      arrange(Parameter))$value
  }

  return (data_frame_to_data_object_helper(
    directory,
    config,
    output
  ))
}


read_trade_data <- function(config, directory) {
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    filter(
      Country %in% load_parameters$country &
        Commodity %in% load_parameters$commodity &
        Measure %in% load_parameters$measure &
        Direction %in% load_parameters$direction &
        Transport_Mode %in% load_parameters$transport_mode
    ) %>%
    mutate(
      Parameter = dmy(Current_Match)
    ) %>%
    select("Parameter", "Year", load_parameters$group_col, "Cumulative")
  
  names(data)[[3]] <- "group_col"
  
  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")
  
  for (group_name in unique(config$group_names)) {
    data_group <- data %>% filter(group_col == group_name) %>% select(-c("group_col"))
    output <- data.frame(Parameter = unique(data_group$Parameter), stringsAsFactors = FALSE) %>% arrange()
    # change this to use pivot_wider
    for (i in 1:length(unique(data_group$Year))) {
      output[[paste("col", i)]] <- (
        data_group %>%
          filter(Year == unique(data_group$Year)[[i]]) %>%
          arrange(Parameter))$Cumulative
    }
    values <- as.data.frame(output)
    output_group[[group_name]] <- TimeSeries$new(output, unique(data_group$Year), update_date)
  }

  return (output_group)
}

chorus_load_function <- function(config, directory) {
  input_files <- list.files(paste0(directory, config$filename))
  data <- foreach(i = 1:length(input_files), .combine=rbind) %do% {
    print(paste0(directory, config$filename, "/", input_files[[i]]))
    output <- as.data.frame(read_excel(
      paste0(directory, config$filename, "/", input_files[[i]]),
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
          date_parsed = ifelse(is.na(date), as.Date(as.numeric(date[[1]]), origin = "1899-12-30"), as.Date(as.numeric(date), origin = "1899-12-30"))
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

  return (data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

example_web_service_load_function <- function(data, indicator, group_name) {
  data_type <- get_indicator_parameter("data_type", indicator, group_name)

  data_object <- DATA_TYPES[[data_type]]$new(
    data %>%
      filter(series_name == group_name) %>%
      select(date, value) %>%
      mutate(date = as.Date(date)) %>%
      arrange(date) %>%
      rename(Parameter = date),
    group_name,
    as.Date(now())
  )

  return (data_object)
}

read_employment_data <- function(config, directory) {
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    filter(
      # Subject %in% load_parameters$Subject &
      #   Group %in% load_parameters$Group &
        Series_title_1 %in% load_parameters$Series_title_1 &
          substr(Series_reference, 4, 4) == "M"
        # Series_title_2 %in% load_parameters$Series_title_2 &
        # Series_title_3 %in% load_parameters$Series_title_3
    ) %>%
    mutate(
      Parameter = ymd(paste0(str_pad(as.character(Period), 7, side = "right", pad = "0"), ".01")),
      Data_value = Data_value * (10 ** Magnitude)
    ) %>%
    select("Parameter", "Series_title_2", "Series_title_3", "Data_value")

  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")

  for (series_title_2 in unique(data$Series_title_2)) {
    for (series_title_3 in unique(data$Series_title_3)) {
      data_group <- data %>%
        filter(
            Series_title_2 == series_title_2 &
            Series_title_3 == series_title_3
        ) %>%
        select(c("Parameter", "Data_value")) %>%
        arrange(Parameter)
      
      group_name <- paste0(series_title_2, " (", tolower(series_title_3), ")")
      output_group[[group_name]] <- TimeSeries$new(data_group, group_name, update_date)
    }
  }

  return (output_group)
}


read_filled_jobs_by_industry_or_region <- function(config, directory) {
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Parameter = ymd(paste0(str_pad(as.character(Period), 7, side = "right", pad = "0"), ".01")),
      Value = Value * (10 ** Magnitude)
    ) %>%
    select("Parameter", load_parameters$group_type, "Value")
  names(data)[[2]] <- "group_column"
  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")
  
  for (industry_group in unique(data$group_column)) {
    data_group <- data %>%
      filter(
        group_column == industry_group
      ) %>%
      select(c("Parameter", "Value"))
    group_name <- industry_group
    output_group[[group_name]] <- TimeSeries$new(data_group, group_name, update_date)
  }

  return (output_group)
}

read_employment_paid_jobs_data <- function(config, directory) {
  cols_to_read <- 1:4
  data <- as.data.frame(read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ))
  col_names = paste0("col_", min(cols_to_read):max(cols_to_read))
  colnames(data)<-col_names
  if(!is.null(config$filter_paid_jobs)){
    data <- data %>% filter(col_2 == config$filter_paid_jobs) %>%
      select(Parameter = col_1, everything()) %>%
      mutate(Parameter = as.Date(dmy(Parameter))) %>%
      spread(col_3, col_4)%>%
      select(Parameter, Total, everything(), -col_2)
    
    colnames(data)<-c("Parameter", paste0("col_", 2:ncol(data)))
    
  }else{
    stop("Please specify an indicator for employment paid jobs data")
  }
  return (data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

load_functions <- list(
  read_from_csv = read_from_csv,
  read_from_excel = read_from_excel,
  read_border_crossing_data = read_border_crossing_data,
  read_border_crossing_data_daily = read_border_crossing_data_daily,
  read_monetary_policy_file = read_monetary_policy_file,
  read_global_cases_file = read_global_cases_file,
  petrol_read_file = petrol_read_file,
  read_trade_data = read_trade_data,
  read_traffic_data = read_traffic_data,
  chorus_load_function = chorus_load_function,
  example_web_service_load_function = example_web_service_load_function,
  read_employment_data = read_employment_data,
  read_filled_jobs_by_industry_or_region = read_filled_jobs_by_industry_or_region,
  read_employment_paid_jobs_data=read_employment_paid_jobs_data
)
