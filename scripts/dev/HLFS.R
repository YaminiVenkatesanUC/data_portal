# Life worthwhile +++ Family wellbeing +++ Enough money for everyday needs +++ Help from welfare organisation +++
# Life satisfaction – HLFS quarterly +++ Trust for parliament +++ Trust for police +++ Trust for the media +++
# Trust in other people +++ Trust for health system +++ Loneliness – past 4 weeks +++ Experienced discrimination


config <- read_config_file()
path <- paste0(config$data_directory, "HLFS/")
files <- file.info(list.files(path, full.names = T, pattern = ".*\\.csv"))
update <- read.csv(rownames(files)[which.max(files$ctime)])

load_parameters <- list(
  Life_worthwhile = list(
    Total = list(
      description = "Life worthwhile",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number:"Sex"
    ),
    By_sex = list(
      description = "Life worthwhile",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number:"Sex"
    ),
    By_age = list(
      description = "Life worthwhile",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number = "Age"
    ),
    By_ethnicity = list(
      description = "Life worthwhile",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number = "Ethnic"
    ),
    By_region = list(
      description = "Life worthwhile",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number = "Region"
    ),
    By_parent_status = list(
      description = "Life worthwhile",
      keyword = "7 to 10",
      skip = 5,
      n_rows = 2,
      sheet_number = "Parent status"
    ),
    By_disability = list(
      description = "Life worthwhile",
      keyword = "7 to 10",
      skip = 5,
      n_rows = 2,
      sheet_number = "Disability"
    )
  ),
  Family_wellbeing = list(
    Total = list(
      description = "Family wellbeing",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number:"Sex"
    ),
    By_sex = list(
      description = "Family wellbeing",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number:"Sex"
    ),
    By_age = list(
      description = "Family wellbeing",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number = "Age"
    ),
    By_ethnicity = list(
      description = "Family wellbeing",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number = "Ethnic"
    ),
    By_region = list(
      description = "Family wellbeing",
      skip = 5,
      n_rows = 2,
      keyword = "7 to 10",
      sheet_number = "Region"
    ),
    By_parent_status = list(
      description = "Family wellbeing",
      keyword = "7 to 10",
      skip = 5,
      n_rows = 2,
      sheet_number = "Parent status"
    ),
    By_disability = list(
      description = "Family wellbeing",
      keyword = "7 to 10",
      skip = 5,
      n_rows = 2,
      sheet_number = "Disability"
    )
  ),
  Enough_money_for_everyday_needs = list(),
  Help_from_welfare_organisation = list(),
  Life_satisfaction_quarterly = list(),
  Trust_for_parliament = list(),
  Trust_for_police = list(),
  Trust_for_the_media = list(),
  Trust_in_other_people = list(),
  Trust_for_health_system = list(),
  Loneliness_past_4_weeks = list(),
  Experienced_discrimination = list()
)


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