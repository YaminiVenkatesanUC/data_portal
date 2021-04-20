library(readxl)
library(writexl)

raw_files <- c("~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/data-file-income-support-and-wage-subsidy-weekly-update-11-december-2020.xlsx",
               "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/data-file-income-support-and-wage-subsidy-weekly-update-8-january-2021.xlsx")

msd_read <- function(raw_files, start, end, sheet, col_types) {
  df_total <- read_excel(path = raw_files[1],
                   sheet = sheet,
                   range = cell_limits(start, end),
                   col_names = TRUE,
                   col_types = col_types)
  for (i in 2:length(raw_files)) {
    df_i <- read_excel(path = raw_files[i],
                                sheet = sheet,
                                range = cell_limits(start, end),
                                col_names = TRUE)
  df_total <- cbind(df_total, df_i)
  }
  return(df_total)
}

msd_write <- function(df, series, filename_to_write, rows) {
  df <- df %>% slice(rows)
  df$series <- series
  df <- df %>%
    pivot_longer(cols = 1:(ncol(df)-1), names_to = "date", values_to = "value") %>%
    pivot_wider(names_from = "series", values_from = "value")
  df$date <- as.numeric(df$date)
  df$date <- as.Date(df$date, origin = "1899-12-30")
  write_xlsx(x = df, path = filename_to_write, col_names = TRUE)
}

#JOBSEEKER+++PERCENTAGE---------------------------
jobseeker_percent <- msd_read(raw_files = raw_files, start = c(9, 4), end = c(24, NA), sheet = 2, col_types = "numeric")
jobseeker_series <- c("All main benefits",
                      "Jobseeker Support - Total",
                      "Jobseeker Support - Work Ready",
                      "Jobseeker Support - Health Condition and Disability")
jobseeker_rows <- c(1:4)
percentPop_series <- c("Percentage of the estimated working-age population receiving Jobseeker Support")
percentPop_rows <- 15

#NUMBER_OF_APPLICATIONS----------------------------
numAppls <- msd_read(raw_files = raw_files, start = c(9, 4), end = c(13, NA), sheet = 4, col_types = "numeric")
numAppls_series <- c("Applications received", "Applications approved", "Applications closed", "Applications declined")
numAppls_rows <- c(1:4)

#NUMBER_OF_CIRP_RECIPIENTS-------------------------
numCIRP_number <- msd_read(raw_files = raw_files, start = c(60, 4), end = c(64, NA), sheet = 2, col_types = "numeric")
numCIRP_grants <- msd_read(raw_files = raw_files, start = c(43, 4), end = c(46, NA), sheet = 5, col_types = "numeric")
names(numCIRP_grants) <- names(numCIRP_number)
numCIRP <- rbind(numCIRP_number, numCIRP_grants)
numCIRP_series <- c("Total number of recipients of CIRP",
                    "Full-time recipients of CIRP",
                    "Part-time recipients of CIRP",
                    "CIRP recipients transferred from Jobseeker",
                    "CIRP grants",
                    "Total transfers from Jobseeker Support")
numCIRP_rows <- c(1, 2, 3, 4, 6, 7)

#NUMBER_OF_WAGE_SUBSIDY_REFUNDS+++AMOUNT--------------------
wageSubsRefunds <- msd_read(raw_files = raw_files, start = c(15, 4), end = c(17, NA), sheet = 4, col_types = NULL)
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], str_remove, pattern = " million")
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], str_remove, pattern = "\\$")
wageSubsRefunds <- as.data.frame(apply(wageSubsRefunds, 2, as.numeric))
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], function(x) x * 1000000)

num_wageSubsRefunds_rows <- 1
num_wageSubsRefunds_series <- c("Number of wage subsidy refunds received - cumulative")

am_wageSubsRefunds_rows <- 2
am_wageSubsRefunds_series <- c("Amount of wage subsidy refunds received - cumulative")

#JOBSEEKER_SUPPORT_BY_MSD_REGION-------------------
jobseeker_MSD <- msd_read(raw_files = raw_files, start = c(30, 3), end = c(43, NA), sheet = 6, col_types = "numeric")
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
suppleSupport <- msd_read(raw_files = raw_files, start = c(9, 4), end = c(11, NA), sheet = 3, col_types = "numeric")
suppleSupport_accom_rows <- 1
suppleSupport_accom_series <- "Accommodation Supplement"
suppleSupport_temp_rows <- 2
suppleSupport_temp_series <- "Temporary Additional Support and Special Benefit"

#ALL_SPECIAL_NEEDS+++SPECIAL_NEEDS_FOOD+++EMERGENCY_HOUSING------------------------
specialGrants <- msd_read(raw_files = raw_files, start = c(14, 4), end = c(17, NA), sheet = 3, col_types = "numeric")
specialGrants_all_rows <- 1
specialGrants_all_series <- "All Special Needs Grants "
specialGrants_food_rows <- 2
specialGrants_food_series <- "Special Needs Grants for food"
specialGrants_housing_rows <- 3
specialGrants_housing_series <- "Emergency Housing grants"

#JOBSEEKER_SUPPORT_BY_REGION------------------------
jobseeker_region <- msd_read(raw_files = raw_files, sheet = 7, start = c(36, 3), end = c(54, NA), col_types = "numeric")
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
msd_write(df = jobseeker_percent, series = jobseeker_series, rows = jobseeker_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Jobseeker Support.xlsx")
msd_write(df = jobseeker_percent, series = percentPop_series, rows = percentPop_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Percentage of Population.xlsx")
msd_write(df = numAppls, series = numAppls_series, rows = numAppls_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Number of Applications.xlsx")
msd_write(df = numCIRP, series = numCIRP_series, rows = numCIRP_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Number of CIRP Recipients.xlsx")
msd_write(df = wageSubsRefunds, series = num_wageSubsRefunds_series, rows = num_wageSubsRefunds_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Number of Wage Subsidy Refunds.xlsx")
msd_write(df = wageSubsRefunds, series = am_wageSubsRefunds_series, rows = am_wageSubsRefunds_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Amount of Wage Subsidy Refunds.xlsx")
msd_write(df = jobseeker_MSD, series = jobseeker_MSD_series, rows = jobseeker_MSD_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Jobseeker Support by MSD Region.xlsx")
msd_write(df = suppleSupport, series = suppleSupport_accom_series, rows = suppleSupport_accom_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Accommodation Supplement.xlsx")
msd_write(df = suppleSupport, series = suppleSupport_temp_series, rows = suppleSupport_temp_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Temporary Additional Support and Special Benefit.xlsx")
msd_write(df = specialGrants, series = specialGrants_all_series, rows = specialGrants_all_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD All Special Needs Grants.xlsx")
msd_write(df = specialGrants, series = specialGrants_food_series, rows = specialGrants_food_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Special Needs Grants for Food.xlsx")
msd_write(df = specialGrants, series = specialGrants_housing_series, rows = specialGrants_housing_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Emergency Housing Grants.xlsx")
msd_write(df = jobseeker_region, series = jobseeker_region_series, rows = jobseeker_region_rows,
    filename_to_write = "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/COVID-19 MSD Jobseeker Support By Region.xlsx")
