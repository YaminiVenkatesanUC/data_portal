# this script is NOT finalized!!!



library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)

raw_prev <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/MSD/monthly/data-file-monthly-benefits-update-november-2020.xlsx"
#raw_update <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/MSD/data-file-income-support-and-wage-subsidy-weekly-update-22-january-2021.xlsx"

read_msd <- function(raw_prev, raw_update, sheet, range_start, range_end, col_types, col_names, col_to_remove = 0) {
  df_prev <- read_excel(path = raw_prev,
                        sheet = sheet,
                        range = cell_limits(range_start, range_end),
                        col_names = col_names,
                        col_types = col_types) %>%
    select(1:(ncol(df_prev) - col_to_remove))

  df_update <- read_excel(path = raw_update,
                          sheet = sheet,
                          range = cell_limits(range_start, range_end),
                          col_names = col_names,
                          col_types = col_types) %>%
    select(1:(ncol(df_update) - col_to_remove))

  df_output <- cbind(df_prev, df_update)
  return(df_output)
}

write_msd <- function(df, series, filename_to_write, rows) {
  df <- df %>% slice(rows)
  df$series <- series
  df <- df %>%
    pivot_longer(cols = 1:(ncol(df)-1), names_to = "date", values_to = "value") %>%
    pivot_wider(names_from = "series", values_from = "value")
  df$date <- as.numeric(df$date)
  df$date <- as.Date(df$date, origin = "1899-12-30")
  write_xlsx(x = df, path = filename_to_write, col_names = TRUE)
}

#NUMBER_CIRP_BY AGE---------------------------
numCIRP_age <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 4,
                        range_start = c(46, 4), range_end = c(57, NA),
                        col_types = "numeric", col_names = TRUE, cols_to_remove = 2) %>%
  rbind(read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 4,
                 range_start = c(14, 4), range_end = c(14, NA),
                 col_types = "numeric", col_names = FALSE, cols_to_remove = 2))
numCIRP_age_series <- c("19 years and under",
                      "20–24 years",
                      "25–29 years",
                      "30–34 years",
                      "35–39 years",
                      "40–44 years",
                      "45–49 years",
                      "50–54 years",
                      "55–59 years",
                      "60–64 years",
                      "65+ years",
                      "Total")
numCIRP_rows <- c(1:length(numCIRP_age_series))

#NUMBER_CIRP_BY REGION----------------------------
numCIRP_region <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 4,
                  range_start = c(87, 4), range_end = c(104, NA),
                  col_types = "numeric", col_names = TRUE, cols_to_remove = 2) %>%
  rbind(read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 4,
                 range_start = c(14, 4), range_end = c(14, NA),
                 col_types = "numeric", col_names = FALSE, cols_to_remove = 2))
numCIRP_region_series <- c("Auckland",
                           "Bay of Plenty",
                           "Canterbury",
                           "Gisborne",
                           "Hawke's Bay",
                           "Manawatū-Whanganui",
                           "Marlborough",
                           "Nelson",
                           "Northland",
                           "Otago",
                           "Southland",
                           "Taranaki",
                           "Tasman",
                           "Waikato",
                           "Wellington",
                           "West Coast",
                           "Other/Unknown",
                           "Total")
numCIRP_region_rows <- c(1:length(numCIRP_region_series))

#NUMBER_CIRP_RECIPIENTS-------------------------
numCIRP_1<- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 2,
                     range_start = c(60, 4), range_end = c(64, NA), col_types = "numeric", col_names = TRUE)

numCIRP_2<- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 5,
                     range_start = c(45, 4), range_end = c(46, NA), col_names = FALSE, col_types = "numeric")
names(numCIRP_2) <- names(numCIRP_1)
numCIRP <- rbind(numCIRP_1, numCIRP_2)
numCIRP_series <- c("Total number of recipients of CIRP",
                    "Full-time recipients of CIRP",
                    "Part-time recipients of CIRP",
                    "CIRP recipients transferred from Jobseeker",
                    "CIRP grants",
                    "Total transfers from Jobseeker Support")
numCIRP_rows <- c(1:6)

#NUMBER_OF_WAGE_SUBSIDY_REFUNDS+++AMOUNT--------------------
wageSubsRefunds <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 4,
                            range_start = c(15, 4), range_end = c(17, NA), col_types = "text", col_names = TRUE)
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], str_remove, pattern = " million")
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], str_remove, pattern = "\\$")
wageSubsRefunds <- as.data.frame(apply(wageSubsRefunds, 2, as.numeric))
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], function(x) x * 1000000)

num_wageSubsRefunds_rows <- 1
num_wageSubsRefunds_series <- c("Number of wage subsidy refunds received - cumulative")

am_wageSubsRefunds_rows <- 2
am_wageSubsRefunds_series <- c("Amount of wage subsidy refunds received - cumulative")

#JOBSEEKER_SUPPORT_BY_MSD_REGION-------------------
jobseeker_MSD <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 6,
                          range_start = c(30, 3), range_end = c(43, NA), col_names = TRUE, col_types = "numeric")
jobseeker_MSD_rows <- c(1:13)
jobseeker_MSD_series <- c("Auckland Metro",
                          "Bay of Plenty",
                          "Canterbury",
                          "Central",
                          "East Coast",
                          "Nelson",
                          "Northland",
                          "Southern",
                          "Taranaki",
                          "Waikato",
                          "Wellington",
                          "Other region",
                          "Total")

#ACCOMMODATION_SUPPLEMENT+++TEMPORARY_ADDITIONAL_SUPPORT---------------------------
suppleSupport <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 3,
                          range_start = c(9, 4), range_end = c(11, NA), col_names = TRUE, col_types = "numeric")
suppleSupport_accom_rows <- 1
suppleSupport_accom_series <- "Accommodation Supplement"

suppleSupport_temp_rows <- 2
suppleSupport_temp_series <- "Temporary Additional Support and Special Benefit"

#ALL_SPECIAL_NEEDS+++SPECIAL_NEEDS_FOOD+++EMERGENCY_HOUSING------------------------
specialGrants <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 3,
                          range_start = c(14, 4), range_end = c(17, NA), col_names = TRUE, col_types = "numeric")
specialGrants_all_rows <- 1
specialGrants_all_series <- "All Special Needs Grants "
specialGrants_food_rows <- 2
specialGrants_food_series <- "Special Needs Grants for food"
specialGrants_housing_rows <- 3
specialGrants_housing_series <- "Emergency Housing grants"

#JOBSEEKER_SUPPORT_BY_REGION------------------------
jobseeker_region <- read_msd(raw_prev = raw_prev, raw_update = raw_update, sheet = 7,
                             range_start = c(36, 3), range_end = c(54, NA), col_names = TRUE, col_types = "numeric")
jobseeker_region_rows <- c(1:18)
jobseeker_region_series <- c("Auckland",
                             "Bay of Plenty",
                             "Canterbury",
                             "Gisborne",
                             "Hawke's Bay",
                             "Manawatū-Whanganui",
                             "Marlborough",
                             "Nelson",
                             "Northland",
                             "Otago",
                             "Southland",
                             "Taranaki",
                             "Tasman",
                             "Waikato",
                             "Wellington",
                             "West Coast",
                             "Other/Unknown",
                             "Total")

#CALLS---------------------------------------------
write_msd(df = jobseeker_percent, series = jobseeker_series, rows = jobseeker_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Jobseeker Support.xlsx")
write_msd(df = jobseeker_percent, series = percentPop_series, rows = percentPop_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Percentage of Population.xlsx")
write_msd(df = numAppls, series = numAppls_series, rows = numAppls_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Number of Applications.xlsx")
write_msd(df = numCIRP, series = numCIRP_series, rows = numCIRP_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Number of CIRP Recipients.xlsx")
write_msd(df = wageSubsRefunds, series = num_wageSubsRefunds_series, rows = num_wageSubsRefunds_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Number of Wage Subsidy Refunds.xlsx")
write_msd(df = wageSubsRefunds, series = am_wageSubsRefunds_series, rows = am_wageSubsRefunds_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Amount of Wage Subsidy Refunds.xlsx")
write_msd(df = jobseeker_MSD, series = jobseeker_MSD_series, rows = jobseeker_MSD_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Jobseeker Support by MSD Region.xlsx")
write_msd(df = suppleSupport, series = suppleSupport_accom_series, rows = suppleSupport_accom_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Accommodation Supplement.xlsx")
write_msd(df = suppleSupport, series = suppleSupport_temp_series, rows = suppleSupport_temp_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Temporary Additional Support and Special Benefit.xlsx")
write_msd(df = specialGrants, series = specialGrants_all_series, rows = specialGrants_all_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD All Special Needs Grants.xlsx")
write_msd(df = specialGrants, series = specialGrants_food_series, rows = specialGrants_food_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Special Needs Grants for Food.xlsx")
write_msd(df = specialGrants, series = specialGrants_housing_series, rows = specialGrants_housing_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Emergency Housing Grants.xlsx")
write_msd(df = jobseeker_region, series = jobseeker_region_series, rows = jobseeker_region_rows,
          filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Jobseeker Support By Region.xlsx")
