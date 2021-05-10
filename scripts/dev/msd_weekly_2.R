# Amount of wage subsidy refunds received +++ Jobseeker support +++ Number of applications +++ Number of recipients of CIRP
# Number of wage subsidy refunds received +++ Recentage of population +++ Accommodation supplement +++ All special needs grants
# Special needs grants for food (weekly) +++ Temporary additional support and special benefit +++ Jobseeker support by region - weekly
# Special needs grants for food by region +++ Emergency housing grants

#library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)

# this script needs tidy up!!! the structure of raw files changes often
directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/_TEST/"
path <- paste0(directory, "MSD/weekly")
files <- file.info(list.files(path, full.names = T))
update <- rownames(files)[which.max(files$mtime)]

load_parameters <- list(
  Jobseeker_support = list(
    sheet = "1. Timeseries-MainBenefits-CIRP",
    rows = list(
      year2021 = c(1:5),
      year2020 = c(17:21)
    ),
    range = c("All main benefits", "Jobseeker Support", "Work Ready", "Health Condition and Disability")
  ),
  Percentage_of_population = list(
    sheet = "1. Timeseries-MainBenefits-CIRP",
    rows = list(
      year2021 = c(1, 15),
      year2020 = c(17,31)
    ),
    range = "Percentage of the estimated working-age population receiving Jobseeker Support"
  ),
  Number_of_applications = list(
    sheet = "3. Timeseries-Wage-Subsidy",
    rows = list(
      cumulative = c(25:28)
    ),
    range = c("NA", "Applications received", "Applications approved", "Applications declined")
  ),
  Number_of_wage_subsidy_refunds = list(
    sheet = "3. Timeseries-Wage-Subsidy",
    rows = list(
      cumulative = c(31, 32)
    ),
    range = c("NA", "Refunds received")
  ),
  Amount_of_wage_subsidy_refunds = list(
    sheet = "3. Timeseries-Wage-Subsidy",
    rows = list(
      cumulative = c(31, 33)
    ),
    range = c("NA", "Amount of refunds received (millions)")
  ),
  Jobseeker_support_by_MSD_region = list(
    sheet = "5. Work and Income regions",
    rows = list(
      year2021 = c(19:32),
      year2020 = c(34:47)
    ),
    range = c("Work and Income region", "Auckland Metro", "Bay of Plenty", "Canterbury", "Central", "East Coast", "Nelson", "Northland", "Southern", "Taranaki", "Waikato", "Wellington", "Other region", "Total")
  ),
  Accommodation_supplement = list(
    sheet = "2. Timeseries-Supplement-Hardsh",
    rows = list(
      year2021 = c(1,2),
      year2020 = c(10,11)
    ),
    range = c("Accommodation Supplement")
  ),
  Temporary_additional_support = list(
    sheet = "2. Timeseries-Supplement-Hardsh",
    rows = list(
      year2021 = c(1,3),
      year2020 = c(10,12)
    ),
    range = c("Temporary Additional Support and Special Benefit")
  ),
  All_special_needs_grants = list(
    sheet = "2. Timeseries-Supplement-Hardsh",
    rows = list(
      year2021 = c(4,5),
      year2020 = c(13,14)
    ),
    range = c("Total Special Needs Grants")
  ),
  Special_needs_grants_for_food_weekly = list(
    sheet = "2. Timeseries-Supplement-Hardsh",
    rows = list(
      year2021 = c(4,6),
      year2020 = c(13,15)
    ),
    range = c("Food grants")
  ),
  Emergency_housing_grants = list(
    sheet = "2. Timeseries-Supplement-Hardsh",
    rows = list(
      year2021 = c(4,7),
      year2020 = c(13,16)
    ),
    range = c("Emergency Housing grants")
  ),
  Jobseeker_support_by_region = list(
    sheet = "6. Regional Council",
    rows = list(
      year2021 = c(25:44),
      year2020 = c(46:65)
    ),
    range = c("Auckland region", "Bay of Plenty region", "Canterbury region", "Chatham Islands region", "Gisborne region", "Hawke's Bay region", "Manawatū-Whanganui region", "Marlborough region", "Nelson region", "Northland region", "Otago region", "Southland region", "Taranaki region", "Tasman region", "Waikato region", "Wellington region", "West Coast region", "Other/Region unknown", "Total")
  )
)

#for (ind in 1:length(load_parameters)) {
  ind = 12
  file.rename(from = paste0(directory, "COVID-19 MSD ", names(load_parameters)[[ind]], ".xlsx"),
              to = paste0(directory, "Previous/COVID-19 MSD ", names(load_parameters)[[ind]], ".xlsx"))

  param <- load_parameters[[ind]]
  df <- read.xlsx(xlsxFile = update,
                  sheet = param$sheet,
                  skipEmptyRows = TRUE,
                  skipEmptyCols = TRUE,
                  fillMergedCells = TRUE,
                  colNames = FALSE) %>%
    filter(!is.na(X4)) %>%
    select_if(~!(all(is.na(.x)))) %>%
    mutate_all(~trimws(.x))

  df_output <- as.data.frame(x = param$range)
  names(df_output) <- c("Series")
  for (i in 1:length(param$rows)) {
    print(i)
    df_param <- df %>%
      slice(param$rows[[i]]) %>%
      #select(param$column:ncol(df)) %>%
      select_if(~!(all(is.na(.x)))) %>%
      #select_if(~all(!is.na(.x))) %>%
      mutate_all(~as.character(.x))
    print(str(df_param))
    colnames(df_param) <- df_param[1,]
    df_param <- df_param[-1,]
    colnames(df_param)[[1]] <- "Series"
    print(str(df_param))
    df_output <- df_output %>%
      left_join(df_param, by = "Series")
  }

  if (length(setdiff(df_output$Series, param$range)) > 0 | length(setdiff(param$range, df_output$Series)) > 0)
    stop ("Row structure have changed for: ", names(load_parameters)[[ind]])

  df_output <- df_output %>%
    pivot_longer(cols = 2:(ncol(df_output)), names_to = "date", values_to = "value") %>%
    pivot_wider(names_from = "Series", values_from = "value")
  df_output$date <- as.numeric(df_output$date)
  df_output$date <- as.Date(df_output$date, origin = "1899-12-30")
  df_output <- select_if(df_output, ~!(all(is.na(.x))))
  df_output <- drop_na(df_output)

  for (col in 2:ncol(df_output)) {
    df_output[[col]] <- round(as.numeric(df_output[[col]]), 1)
  }

  View(df_output)
  # write_xlsx(x = df_output, path = paste0(directory, "COVID-19 MSD ", names(load_parameters)[[ind]], ".xlsx"), col_names = TRUE)
#}

