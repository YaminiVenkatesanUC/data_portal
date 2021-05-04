# Monthly filled jobs +++ Monthly earnings +++ Monthly filled jobs (by industry) +++ Monthly filled jobs (by region) +++ Monthly filled jobs (by gender) +++ Monthly filled jobs (by age)

load_parameters <- list(
  Monthly_earnings = list(
    Group = "High level industry by variable",
    Series_title_1 = "Total earnings",
    Series_title_2 = c("Primary industries", "Goods-producing industries", "Service industries", "All industries"),
    Series_title_3 = "Actual",
    Series_title_4 = ""
    ),
  Monthly_filled_jobs = list(
    Group = "High level industry by variable",
    Series_title_1 = "Filled jobs",
    Series_title_2 = c("Primary industries", "Goods-producing industries", "Service industries", "All industries"),
    Series_title_3 = c("Actual", "Seasonally adjusted"),
    Series_title_4 = ""
    ),
  Monthly_filled_jobs_by_industry = list(
    Group = "Industry by variable",
    Series_title_1 = "Filled jobs",
    Series_title_2 = c("Agriculture, Forestry and Fishing", "Mining", "Manufacturing", "Electricity, Gas, Water and Waste Services",
                       "Construction", "Wholesale Trade", "Retail Trade", "Accommodation and Food Services",
                       "Transport, Postal and Warehousing", "Information Media and Telecommunications",
                       "Financial and Insurance Services", "Rental, Hiring and Real Estate Services",
                       "Professional, Scientific and Technical Services", "Administrative and Support Services",
                       "Public Administration and Safety", "Education and Training", "Health Care and Social Assistance",
                       "Arts and Recreation Services", "Other Services"),
    Series_title_3 = "Actual",
    Series_title_4 = ""
  ),
  Monthly_filled_jobs_by_region = list(
    Group = "Region by variable",
    Series_title_1 = "Filled jobs",
    Series_title_2 = c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne", "Hawke's Bay", "Taranaki",
                       "Manawatu-Whanganui", "Wellington", "Tasman", "Nelson", "Marlborough", "West Coast", "Canterbury",
                       "Otago", "Southland"),
    Series_title_3 = "Actual",
    Series_title_4 = ""
  ),
  Monthly_filled_jobs_by_gender = list(
    Group = "Sex and Region by variable",
    Series_title_1 = "Filled jobs",
    Series_title_2 = c("Male", "Female"),
    Series_title_3 = c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne", "Hawke's Bay", "Taranaki",
                       "Manawatu-Whanganui", "Wellington", "Tasman", "Nelson", "Marlborough", "West Coast", "Canterbury",
                       "Otago", "Southland", "Total NZ"),
    Series_title_4 = "Actual"
  ),
  Monthly_filled_jobs_by_age = list(
    Group = "Age and Region by variable",
    Series_title_1 = "Filled jobs",
    Series_title_2 = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65 +"),
    Series_title_3 = c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne", "Hawke's Bay", "Taranaki",
                       "Manawatu-Whanganui", "Wellington", "Tasman", "Nelson", "Marlborough", "West Coast", "Canterbury",
                       "Otago", "Southland", "Total NZ"),
    Series_title_4 = "Actual"
  )
  )

config <- read_config_file()
update <- paste0(config$data_directory, "employment-indicators-monthly.csv")

data <- read.csv(update, stringsAsFactors = FALSE) %>%
  filter(Subject == "Employment indicators - MEI") %>%
  filter(substr(Series_reference, 4, 4) == "M")

col_names <- c("Series_reference", "Period", "Data_value", "Suppressed", "STATUS", "UNITS", "Magnitude", "Subject", "Group",
               "Series_title_1", "Series_title_2", "Series_title_3", "Series_title_4", "Series_title_5")
if (length(setdiff(names(data), col_names)) > 0) stop ("New column added: ", setdiff(names(data), col_names))


for (ind in 1:length(names(load_parameters))) {
  file.rename(from = paste0(paste0(config$data_directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx")),
              to = paste0(paste0(config$data_directory, "Previous/COVID-19 ", names(load_parameters)[ind], ".xlsx")))
}



for (ind in 1:length(load_parameters)) {
  param <- load_parameters[[ind]]

  df <- data %>%
    filter(Group == param$Group) %>%
    filter(Series_title_1 %in% param$Series_title_1) %>%
    filter(Series_title_2 %in% param$Series_title_2) %>%
    filter(Series_title_3 %in% param$Series_title_3) %>%
    filter(Series_title_4 %in% param$Series_title_4)

  if (!identical(unique(df$Series_title_1), param$Series_title_1)) stop ("Order in Series_title_1 changed")
  if (!identical(unique(df$Series_title_2), param$Series_title_2)) stop ("Order in Series_title_2 changed")
  if (!identical(unique(df$Series_title_3), param$Series_title_3)) stop ("Order in Series_title_3 changed")
  if (!identical(unique(df$Series_title_4), param$Series_title_4)) stop ("Order in Series_title_4 changed")

  df <- df %>%
    mutate(
      Parameter = ymd(paste0(str_pad(as.character(Period), 7, side = "right", pad = "0"), ".01")),
      Value = Data_value * (10 ** Magnitude)
    ) %>%
    select(Parameter, Value, Series_title_2, Series_title_3)

  if (ind %in% c(1, 2, 3, 4)) {
    for (group in unique(df$Series_title_2)) {
      output <- df %>%
        filter(Series_title_2 == group) %>%
        select(-Series_title_2) %>%
        pivot_wider(names_from = Series_title_3, values_from = Value) %>%
        as.data.frame()

      write.xlsx(x = output, paste0(config$data_directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx"),
                 sheetName = group,
                 append = TRUE,
                 row.names = FALSE)
    }

  } else {
    for (group in unique(df$Series_title_3)) {
      output <- df %>%
        filter(Series_title_3 == group) %>%
        select(-Series_title_3) %>%
        pivot_wider(names_from = Series_title_2, values_from = Value) %>%
        as.data.frame()

      write.xlsx(x = output, paste0(config$data_directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx"),
                 sheetName = group,
                 append = TRUE,
                 row.names = FALSE)
    }
  }
}


read_employment_data <- function(config, directory) {
  # load_parameters <- config$load_parameters
  # data <- read.csv(
  #   paste0(directory, config$filename),
  #   stringsAsFactors = FALSE
  # ) %>%
  #   filter(Group == load_parameters$keyword) %>%
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