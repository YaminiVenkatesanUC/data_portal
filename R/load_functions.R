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
  # value_name_transformed <- gsub(
  #   "Quarantine and managed isolation figures as at ",
  #   "Current - ",
  #   value_name
  # )
  if(!is.null(config$occupancy_rate)){

    data_object[["Occupancy rate"]]$value_names <- value_name

  }else{
    data_object[["Occupancy"]]$value_names <- value_name
  }

  return(data_object)
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

read_vaccination <- function(config, directory) {
  data_object <- read_from_excel(config, directory)
  group_name <- config$group_names[[1]]
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number
  ))

  value_names <- tail(colnames(data), 2)
  if (group_name != "Total") {
    data_object[[group_name]]$value_names <- as.list(value_names)
  }
  return(data_object)
}


load_functions <- list(
  read_from_csv = read_from_csv,
  read_from_excel = read_from_excel,
  read_employment_paid_jobs_data = read_employment_paid_jobs_data,
  read_from_csv_error = read_from_csv_error,
  read_from_excel_error = read_from_excel_error,
  gas_use_data = gas_use_data,
  read_managed_isolotion_data = read_managed_isolotion_data,
  read_MBIE_rental = read_MBIE_rental,
  read_hpa_drinking_data = read_hpa_drinking_data,
  read_vaccination = read_vaccination
)
