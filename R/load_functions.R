read_from_csv <- function(config, directory) {

  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }

  data <- as.data.frame(read.csv(
    paste0(directory, config$filename),
    skip = skip
  ))
  names(data) <- paste0("col_", 1:ncol(data))

  data <- data %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter)) %>%
    select(c("Parameter", paste0("col_", config$value_col)))

  if (!is.null(config$input_units)) {
    data[, 2:ncol(data)] <- mapply("*", data[, 2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  if (is.null(config$order_parameter) || config$order_parameter) {
    data <- data %>% arrange(Parameter)
  }

  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

read_from_excel <- function(config, directory) {
  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  cols_to_read <- c(config$parameter_col, unlist(config$value_col))
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2 + skip, min(cols_to_read)), c(NA, max(cols_to_read)))
  )) %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter))
  data <- data[, cols_to_read - min(cols_to_read) + 1]

  if (!is.null(config$input_units)) {
    data[, 2:ncol(data)] <- mapply("*", data[, 2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  if (is.null(config$order_parameter) || config$order_parameter) {
    data <- data %>% arrange(Parameter)
  }

  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
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
    )
  data$Date <- dmy(data$Date)
  data <- data %>% arrange(Date)
  data <- data %>%
    mutate(
      Parameter = format(data$Date, "%d-%b")
    ) %>%
    select("Parameter", "Year", load_parameters$group_col, "Cumulative")

  names(data)[[3]] <- "group_col"

  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")

  for (group_name in unique(config$group_names)) {
    data_group <- data %>% filter(group_col == group_name) %>%
      select(-c("group_col"))

#adding extra row for non-leap years
    data_group <- data_group %>%
      tibble::add_row(Parameter = "29-Feb", Year = 2015, Cumulative = NA, .before = 60)

    output <- data_group %>%
      pivot_wider(names_from = Year, values_from = c("Cumulative"))

    data_type <- get_indicator_parameter("data_type", config)
    output_group[[group_name]] <- DATA_TYPES[[data_type]]$new(output, unique(data_group$Year), update_date)
  }

  return(output_group)
}

chorus_load_function <- function(config, directory) {
  input_files <- list.files(paste0(directory, config$filename))
  data <- foreach(i = 1:length(input_files), .combine = rbind) %do% {
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

  return(data_frame_to_data_object_helper(
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

  return(data_object)
}

read_employment_data <- function(config, directory) {
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    filter(Group == load_parameters$keyword) %>%
    filter(
      Series_title_1 %in% load_parameters$Series_title_1 &
        substr(Series_reference, 4, 4) == "M"
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

  return(output_group)
}

read_filled_jobs_by_gender <- function(config, directory) {
  print(paste0(directory, config$filename))
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Parameter = ymd(paste0(str_pad(as.character(Period), 7, side = "right", pad = "0"), ".01")),
      Value = as.numeric(Data_value)
    ) %>%
    filter(Group == load_parameters$keyword) %>%
    select("Parameter", load_parameters$group_type, "Value", "Series_title_2")
  names(data)[[2]] <- "group_column"
  names(data)[[4]] <- "Sex"
  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")

  for (industry_group in unique(data$group_column)) {
    data_group <- data %>%
      filter(
        group_column == industry_group
      ) %>%
      select(c("Parameter", "Value", "Sex")) %>%
      pivot_wider(names_from = Sex, values_from = c("Value"))

    group_name <- industry_group
    output_group[[group_name]] <- TimeSeries$new(data_group, names(data_group)[2:3], update_date)
  }

  return(output_group)
}


read_filled_jobs_by_age <- function(config, directory) {
  print(paste0(directory, config$filename))
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Parameter = ymd(paste0(str_pad(as.character(Period), 7, side = "right", pad = "0"), ".01")),
      Value = as.numeric(Data_value)
    ) %>%
    filter(Group == load_parameters$keyword) %>%
    select("Parameter", load_parameters$group_type, "Value", "Series_title_2")
  names(data)[[2]] <- "group_column"
  names(data)[[4]] <- "Age_group"
  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")

  for (industry_group in unique(data$group_column)) {
    data_group <- data %>%
      filter(
        group_column == industry_group
      ) %>%
      select(c("Parameter", "Value", "Age_group")) %>%
      pivot_wider(names_from = Age_group, values_from = c("Value"))

    group_name <- industry_group
    output_group[[group_name]] <- TimeSeries$new(
      data_group,
      names(data_group)[2:ncol(data_group)],
      update_date
    )
  }

  return(output_group)
}


read_filled_jobs_by_industry_or_region <- function(config, directory) {
  load_parameters <- config$load_parameters
  data <- read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Parameter = ymd(paste0(str_pad(as.character(Period), 7, side = "right", pad = "0"), ".01")),
      Value = Data_value * (10 ** Magnitude)
    ) %>%
    filter(Group == load_parameters$keyword) %>%
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

  return(output_group)
}

read_employment_paid_jobs_data <- function(config, directory) {
  cols_to_read <- 1:4
  data <- as.data.frame(read.csv(
    paste0(directory, config$filename),
    stringsAsFactors = FALSE
  ))
  col_names <- paste0("col_", min(cols_to_read):max(cols_to_read))
  colnames(data) <- col_names

  if (!is.null(config$filter_paid_jobs)) {
    data <- data %>%
      filter(col_2 == config$filter_paid_jobs) %>%
      select(Parameter = col_1, everything()) %>%
      mutate(Parameter = as.Date(ymd(Parameter))) %>%
      spread(col_3, col_4) %>%
      select(Parameter, Total, everything(), -col_2)

    colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))

  }else if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))

  }else{

    "please enter a filter for visa type or employment"
  }

  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}


read_from_csv_error <- function(config, directory) {
  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }

  data <- as.data.frame(read.csv(
    paste0(directory, config$filename),
    skip = skip
  ))
  names(data) <- paste0("col_", 1:ncol(data))

  data <- data %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter))


  if (!is.null(config$error_col)) {

    # don't judge this method too harshly, I am very sleepy
    stopifnot(length(config$value_col) == length(config$error_col))

    data_lower <- data %>% select(paste0("col_", config$value_col)) -
      data %>% select(paste0("col_", config$error_col))
    names(data_lower) <- paste0("col_", config$value_col, "_lower")


    data_upper <- data %>% select(paste0("col_", config$value_col)) +
      data %>% select(paste0("col_", config$error_col))
    names(data_upper) <- paste0("col_", config$value_col, "_upper")

    data <- cbind(data, data_lower, data_upper)

  } else if ((!is.null(config$lower_bound_col) && !is.null(config$upper_bound_col))) {

    stopifnot(length(config$value_col) == length(config$lower_bound_col),
              length(config$value_col) == length(config$upper_bound_col))

    data <- data %>%

      rename_with(~paste0("col_", config$value_col, "_lower"), unlist(config$lower_bound_col)) %>%
      rename_with(~paste0("col_", config$value_col, "_upper"), unlist(config$upper_bound_col))

  }

  data <- data %>% select("Parameter",
                          paste0("col_", config$value_col),
                          paste0("col_", config$value_col, "_lower"),
                          paste0("col_", config$value_col, "_upper"))


  if (!is.null(config$input_units)) {
    data[, 2:ncol(data)] <- mapply("*", data[, 2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  return(data_frame_to_data_object_helper_error(
    directory,
    config,
    data %>% arrange(Parameter)
  ))
}



read_from_excel_error <- function(config, directory) {
  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }


  if (!is.null(config$error_col)) {
    cols_to_read <-  c(config$parameter_col,
                       unlist(config$value_col),
                       unlist(config$error_col))
  } else {
    cols_to_read <-  c(config$parameter_col,
                       unlist(config$value_col),
                       unlist(config$lower_bound_col),
                       unlist(config$upper_bound_col))
  }


  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = paste0("col_", min(cols_to_read):max(cols_to_read)),
    range = cell_limits(c(2 + skip, min(cols_to_read)), c(NA, max(cols_to_read)))
  )) %>%
    dplyr::rename(
      Parameter = paste0("col_", config$parameter_col)
    ) %>%
    mutate(Parameter = parameter_transform(Parameter))



  if (!is.null(config$error_col)) {
    stopifnot(length(config$value_col) == length(config$error_col))

    data_lower <- data %>% select(paste0("col_", config$value_col)) -
      data %>% select(paste0("col_", config$error_col))
    names(data_lower) <- paste0("col_", config$value_col, "_lower")


    data_upper <- data %>% select(paste0("col_", config$value_col)) +
      data %>% select(paste0("col_", config$error_col))
    names(data_upper) <- paste0("col_", config$value_col, "_upper")

    data <- cbind(data, data_lower, data_upper)

  } else if ((!is.null(config$lower_bound_col) && !is.null(config$upper_bound_col))) {

    stopifnot(length(config$value_col) == length(config$lower_bound_col),
              length(config$value_col) == length(config$upper_bound_col))

    data <- data %>%
      rename_with(~paste0("col_", config$value_col, "_lower"), unlist(config$lower_bound_col)) %>%
      rename_with(~paste0("col_", config$value_col, "_upper"), unlist(config$upper_bound_col))

  }

  data <- data %>% select("Parameter",
                          paste0("col_", config$value_col),
                          paste0("col_", config$value_col, "_lower"),
                          paste0("col_", config$value_col, "_upper"))




  if (!is.null(config$input_units)) {
    data[, 2:ncol(data)] <- mapply("*", data[, 2:ncol(data)], config$input_units)
  }

  if (!is.null(config$date_filter)) {
    date_filter <- eval(parse(text = config$date_filter))
    data <- data %>% filter(date_filter(Parameter))
  }

  if (is.null(config$drop_na) || config$drop_na) {
    data <- drop_na(data)
  }

  return(data_frame_to_data_object_helper_error(
    directory,
    config,
    data %>% arrange(Parameter)
  ))
}



electricity_grid_by_region_data <- function(config, directory) {
  electricity_data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number
  ))

  region_codes <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = 2
  ))

  data <- left_join(electricity_data, region_codes, by = "Region ID") %>%
    mutate(`Period start` = ymd(`Period start`)) %>%
    group_by(`Regional Council`, `Period start`) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    spread(`Regional Council`, `Demand (GWh)`)

  colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))

  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))

}

read_chorus_regional_data <- function(config, directory) {
  dates <- ymd(c(
    "2020/04/29",
    "2020/05/14",
    "2020/07/05",
    "2020/07/06",
    "2020/07/21",
    "2020/07/19",
    "2020/07/20",
    "2020/07/26"
  ))
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number
  ))
  colnames(data)[1] <- "Date"
  data <- data %>%
    mutate(Date = ymd(Date)) %>%
    group_by(Date, `Region Name`) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(!Date %in% dates) %>%
    spread(`Region Name`, `Measure Values`)

  colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))

  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

gas_use_data <- function(config, directory) {
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    skip = config$skip
  )) %>%
    select(-`Source: First Gas`)

 if (config$gas_source == "Vector") {
    data <- data %>%
      select(Date = `...1`,  everything()) %>%
      mutate(
        Date = dmy(Date),
        `Ballance Agri- Nutrients` = `Ballance Agri-Nutrients...2` +
          `Ballance Agri-Nutrients...3`,
        Fonterra = `Subtotal Fonterra...6` +
          `Subtotal Fonterra...7` +
          `Subtotal Fonterra...8` +
          `Subtotal Fonterra...9` +
          `Subtotal Fonterra...10` +
          `Subtotal Fonterra...11` +
          `Subtotal Fonterra...12` +
          `Subtotal Fonterra...13` +
          `Subtotal Fonterra...14`
      ) %>%
      select(
        Date,
        Fonterra,
        `Ballance Agri- Nutrients`,
        `Glenbrook steel mill`,
        `Kinleith pulp and paper mill`,
        `Marsden Point oil refinery`
      )
  }

  if (config$gas_source == "Maui") {
    data <- data %>%
      mutate(Date = dmy(`...1`), Methanex = `...4` + `Methanex Motunui`) %>%
      select(Date, everything(), -`...1`, -`...4`, -`Methanex Motunui`)
  }

  colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))
  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

read_managed_isolotion_data <- function(config, directory) {
  data_object <- read_from_excel(config, directory)

  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    col_names = c("header_date", "occupancy"),
    range = cell_limits(c(1, 1), c(1, 2))
  ))
  value_name <- data$header_date
  value_name_transformed <- gsub(
    "Quarantine and managed isolation figures as at ",
    "Current - ",
    value_name
  )
  if(!is.null(config$occupancy_rate)){

    data_object[["Occupancy rate"]]$value_names <- value_name

  }else{
    data_object[["Occupancy"]]$value_names <- value_name
  }

  return(data_object)
}

read_hlfs_data <- function(config, directory) {
  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform, ))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  colNames <- c("Parameter", paste0("col_", config$value_col), paste0("col_", config$lower_bound_col, "_lower"), paste0("col_", config$upper_bound_col, "_upper"))
  values <- rep(NA, length(colNames))
  data_all <- data.frame(colNames = colNames, values = values) %>%
    spread(key = colNames, value = values)

  data_all <- data_all[,mixedsort(colNames)]

  if (config$group_names == "By region") {
    regions <- c("Northland",
                 "Auckland",
                 "Waikato",
                 "Bay of Plenty",
                 "Gisborne/Hawke's Bay",
                 "Taranaki",
                 "Manawatū-Whanganui",
                 "Wellington",
                 "Nelson/Tasman/Marlborough/West Coast",
                 "Canterbury",
                 "Otago",
                 "Southland")
    for (region in 2:length(regions)) {
      data_all[region,] <- NA
    }
    data_all <- data_all %>% relocate(Parameter)
    data_all$Parameter <- regions
  }


  for (i in 1:length(config$filename)) {
    data <- as.data.frame(read_excel(paste0(directory, config$filename[i]), sheet = config$sheet_number, col_names = TRUE, skip = skip, .name_repair = "unique"))

    data <- data %>% select(-2,-6)
    start_row <- grep(pattern = config$description, x = data[[1]])+1
    end_row <- start_row + config$n_rows - 1

    data <- data[c(start_row:end_row), ]
    data <- data %>% filter(grepl(pattern = config$keyword, x = data[[2]]))

    data <- data %>% select(-1, -2)


    for (col in 1:length(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }

    not_any_na <- function(x) all(!is.na(x))
    data <- data %>% select_if(not_any_na)

    quarter <- as.data.frame(read_excel(paste0(directory, config$filename[i]), sheet = config$sheet_number, range = "A4", col_names = FALSE))
    data$Parameter <- quarter[[1]]
    data$Parameter <- stringr::str_replace(data$Parameter, "quarter", replacement = "01")
    data$Parameter <- as.Date(data$Parameter, format = "%B %Y %d")
    data <- data %>% relocate(Parameter)

    stopifnot(all.equal(length(config$value_col), length(config$lower_bound_col), length(config$upper_bound_col)))


    if (config$group_names == "By region") {

      data <- data %>% select(-2, -3)

      data <- data %>% rename("date" = "Parameter")
      cols <- ((ncol(data)-1)/2)+1

      for (ind in 2:cols) {
        region <- ind
        error <- ind+1
        name_region <- names(data)[[ind]]
        new_name <- paste0(name_region, "_error")
        data <- unite(data = data, col = united, region:error, sep = "_", remove = TRUE) %>%
          dplyr::rename_with(.fn = ~paste0(name_region, "_error"), .cols = united)
      }

      data <- data %>%
        pivot_longer(cols = 2:ncol(data), names_to = "Parameter") %>%
        separate(col = value, into = c("value", "error"), sep = "_", convert = TRUE) %>%
        mutate(lower = value - error, upper = value + error)

      value_ind <- unlist(config$value_col[i])

      lower_ind <- unlist(config$lower_bound_col[i])

      upper_ind <- unlist(config$upper_bound_col[i])
      data_all[[value_ind]] <- data$value
      data_all[[lower_ind]] <- data$lower
      data_all[[upper_ind]] <- data$upper



    }  else if(config$group_names == "Total NZ Population"){

      data <- data %>% select(1,2,3)

      names(data) <- c("Parameter","Value","Error")

      value <- unlist(config$value_col)

      data <- data %>% mutate(lower = Value-Error, upper = Value + Error) %>%
        select(-c(value+1))

      data <- data %>%
        dplyr::rename_with(.fn = ~paste0("col_", value+1, "_lower"), .cols = lower) %>%
        dplyr::rename_with(.fn = ~paste0("col_", value+2, "_upper"), .cols = upper) %>%
        dplyr::rename_with(.fn = ~paste0("col_", value), .cols = value)

      data_all$Parameter <- as.Date(data_all$Parameter)
      data_all <- rbind(data_all, data)
      data_all <- drop_na(data_all)

    }
    else {
      data <- data %>% select(-2,-3)

      for (i in 1:length(config$value_col)) {
        value <- unlist(config$value_col[i])

        data <- data %>%
          tibble::add_column(lower = data[[value]] - data[[value+1]], .after = value+1) %>%
          tibble::add_column(upper = data[[value]] + data[[value+1]], .after = "lower") %>%
          select(-c(value+1))

        data <- data %>%
          dplyr::rename_with(.fn = ~paste0("col_", value+1, "_lower"), .cols = lower) %>%
          dplyr::rename_with(.fn = ~paste0("col_", value+2, "_upper"), .cols = upper) %>%
          dplyr::rename_with(.fn = ~paste0("col_", value), .cols = value)

        names(data_all) <- names(data)
      }
      data_all$Parameter <- as.Date(data_all$Parameter)
      data_all <- rbind(data_all, data)
      data_all <- drop_na(data_all)
    }
  }



  return(data_frame_to_data_object_helper_error(
    directory,
    config,
    data_all
  ))
}


petrol_read_file_month <- function(config, directory) {
  filename <- config$filename
  data <- as.data.frame(read_excel(
    paste0(directory, filename),
    sheet = config$sheet_number,
    skip = config$skip,
    col_names = TRUE,
    .name_repair = "minimal"
  ))

  data <- data[names(data) != ""]
  names(data)[1:2] <- c("Company", "Type")
  data$Type[data$Company == "Truckstops"] <- "Truckstops"

  data <- data %>%
    filter(!is.na(Company) & !is.na(Type)) %>%
    pivot_longer(cols = 3:ncol(data), names_to = "date") %>%
    mutate(Parameter = as.Date(as.numeric(date), origin = "1899-12-30"))

  output <- data.frame(Parameter = unique(data$Parameter), stringsAsFactors = FALSE) %>% arrange()
  for (i in 1:length(config$fuel_type)) {
    output[[paste("col", i)]] <- (
      data %>%
        filter(Type == config$fuel_type[[i]] & Company == config$company_name[[i]]) %>%
        arrange(Parameter)
    )$value
  }

  return(data_frame_to_data_object_helper(
    directory,
    config,
    output
  ))
}

read_MBIE_rental <- function(config, directory) {
  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  data <- as.data.frame(read_excel(paste0(directory, config$filename))) %>%
    select(`Time Frame`, Location, `Lodged Bonds`, `Active Bonds`, `Closed Bonds`, `Average Weekly Rent`) %>%
    filter(Location != "NA")

  data$`Time Frame` <- as.Date(data$`Time Frame`, "%b %d %Y")
  data$Location <- str_remove_all(data$Location, " Region")
  data <- data %>%
    filter(`Time Frame` > "2015-01-01") %>%
    select(`Time Frame`, Location, config$sub_series) %>%
    arrange(`Time Frame`) %>%
    pivot_wider(names_from = Location, values_from = config$sub_series)

  glimpse(data)
  # %>%
  #   select(`Time Frame`, ALL, everything())

  names(data) <- c("Parameter", paste0("col_", config$value_col))

  if (is.null(config$order_parameter) || config$order_parameter) {
    data <- data %>% arrange(Parameter)
  }

  glimpse(data)

  if (!is.null(config$input_units)) {
    data[, 2:ncol(data)] <- mapply("*", data[, 2:ncol(data)], config$input_units)
  }

  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}

read_hpa_drinking_data <- function(config,directory) {

  if (!is.null(config$parameter_transform)) {
    parameter_transform <- eval(parse(text = config$parameter_transform))
  }
  skip <- 0
  if (!is.null(config$skip)) {
    skip <- config$skip
  }
  data <- as.data.frame(read_excel(paste0(directory, config$filename))) %>%
    select(Parameter,config$series,"Wave 1","Wave 2")

  names(data)[[2]] <- "series"

  output_group <- list()
  update_date <- as.Date(file.info(paste0(directory, config$filename))$mtime, tz = "NZ")


  data$series  <- ifelse(str_detect(data$series ,"Total"),"Total",paste0("Age group ",gsub("-"," – ",data$series )))


  for (val in unique(data$series)) {
    data_group <- data %>%
      filter(
        series == val
      ) %>%
      select(-series)
    group_name <- val

    output_group[[group_name]] <- BarChart$new(data_group, names(data_group)[2:3], update_date)
  }

  return(output_group)


}


load_functions <- list(
  read_from_csv = read_from_csv,
  read_from_excel = read_from_excel,
  read_trade_data = read_trade_data,
  chorus_load_function = chorus_load_function,
  example_web_service_load_function = example_web_service_load_function,
  read_employment_data = read_employment_data,
  read_filled_jobs_by_industry_or_region = read_filled_jobs_by_industry_or_region,
  read_employment_paid_jobs_data = read_employment_paid_jobs_data,
  read_filled_jobs_by_gender = read_filled_jobs_by_gender,
  read_filled_jobs_by_age = read_filled_jobs_by_age,
  read_from_csv_error = read_from_csv_error,
  read_from_excel_error = read_from_excel_error,
  electricity_grid_by_region_data = electricity_grid_by_region_data,
  read_chorus_regional_data = read_chorus_regional_data,
  gas_use_data = gas_use_data,
  read_managed_isolotion_data = read_managed_isolotion_data,
  read_hlfs_data = read_hlfs_data,
  read_MBIE_rental = read_MBIE_rental,
  read_hpa_drinking_data = read_hpa_drinking_data
)
