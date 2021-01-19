library(readxl)
library(writexl)

msd <- function(df, series, filename_to_write, rows) {
  df <- df %>% slice(rows)
  df$series <- series
  df <- df %>%
    pivot_longer(cols = 1:(ncol(df)-1), names_to = "date", values_to = "value") %>%
    pivot_wider(names_from = "series", values_from = "value")
  df$date <- as.numeric(df$date)
  df$date <- as.Date(df$date, origin = "1899-12-30")
  write_xlsx(x = df, path = filename_to_write, col_names = TRUE)
}


raw <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/data-file-income-support-and-wage-subsidy-weekly-update-11-december-2020.xlsx"

#JOBSEEKER+++PERCENTAGE---------------------------
jobseeker_percent <- read_excel(path = raw,
                               sheet = 2,
                               range = cell_limits(c(9, 4), c(24, NA)),
                               col_names = TRUE)

jobseeker_series <- c("All main benefits",
                      "Jobseeker Support - Total",
                      "Jobseeker Support - Work Ready",
                      "Jobseeker Support - Health Condition and Disability")
jobseeker_rows <- c(1:4)

percentPop_series <- c("Percentage of the estimated working-age population receiving Jobseeker Support")
percentPop_rows <- 15

#NUMBER_OF_APPLICATIONS----------------------------
numAppls <- read_excel(path = raw,
                          sheet = 4,
                          range = cell_limits(c(9, 4), c(13, NA)),
                          col_names = TRUE)
numAppls_series <- c("Applications received", "Applications approved", "Applications closed", "Applications declined")
numAppls_rows <- c(1:4)

#NUMBER_OF_CIRP_RECIPIENTS-------------------------
numCIRP_1<- read_excel(path = raw,
                       sheet = 2,
                       range = cell_limits(c(60, 27), c(64, NA)),
                       col_names = TRUE,
                       col_types = "numeric")
numCIRP_2<- read_excel(path = raw,
                     sheet = 5,
                     range = cell_limits(c(45, 27), c(46, NA)),
                     col_names = FALSE,
                     col_types = "numeric")
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
wageSubsRefunds <- read_excel(path = raw,
                              sheet = 4,
                              range = cell_limits(c(15, 14), c(17, NA)),
                              col_names = TRUE)
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], str_remove, pattern = " million")
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], str_remove, pattern = "\\$")
wageSubsRefunds <- as.data.frame(apply(wageSubsRefunds, 2, as.numeric))
wageSubsRefunds[2, ] <- lapply(wageSubsRefunds[2, ], function(x) x * 1000000)

num_wageSubsRefunds_rows <- 1
num_wageSubsRefunds_series <- c("Number of wage subsidy refunds received - cumulative")

am_wageSubsRefunds_rows <- 2
am_wageSubsRefunds_series <- c("Amount of wage subsidy refunds received - cumulative")

#JOBSEEKER_SUPPORT_BY_MSD_REGION-------------------
jobseeker_MSD <- read_excel(path = raw,
                            sheet = 6,
                            range = cell_limits(c(30, 3), c(43, NA)),
                            col_names = TRUE,
                            col_types = "numeric")
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
suppleSupport <- read_excel(path = raw,
                            sheet = 3,
                            range = cell_limits(c(9, 4), c(11, NA)),
                            col_names = TRUE,
                            col_types = "numeric")
suppleSupport_accom_rows <- 1
suppleSupport_accom_series <- "Accommodation Supplement"

suppleSupport_temp_rows <- 2
suppleSupport_temp_series <- "Temporary Additional Support and Special Benefit"

#ALL_SPECIAL_NEEDS+++SPECIAL_NEEDS_FOOD+++EMERGENCY_HOUSING------------------------
specialGrants <- read_excel(path = raw,
                            sheet = 3,
                            range = cell_limits(c(14, 4), c(17, NA)),
                            col_names = TRUE,
                            col_types = "numeric")
specialGrants_all_rows <- 1
specialGrants_all_series <- "All Special Needs Grants "
specialGrants_food_rows <- 2
specialGrants_food_series <- "Special Needs Grants for food"
specialGrants_housing_rows <- 3
specialGrants_housing_series <- "Emergency Housing grants"

#JOBSEEKER_SUPPORT_BY_REGION------------------------
jobseeker_region <- read_excel(path = raw,
                               sheet = 7,
                               range = cell_limits(c(36, 3), c(54, NA)),
                               col_names = TRUE,
                               col_types = "numeric")
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
msd(df = jobseeker_percent, series = jobseeker_series, rows = jobseeker_rows,
    filename_to_write = "COVID-19 MSD Jobseeker Support.xlsx")
msd(df = jobseeker_percent, series = percentPop_series, rows = percentPop_rows,
    filename_to_write = "COVID-19 MSD Percentage of Population.xlsx")
msd(df = numAppls, series = numAppls_series, rows = numAppls_rows,
    filename_to_write = "COVID-19 MSD Number of Applications.xlsx")
msd(df = numCIRP, series = numCIRP_series, rows = numCIRP_rows,
    filename_to_write = "COVID-19 MSD Number of CIRP Recipients.xlsx")
msd(df = wageSubsRefunds, series = num_wageSubsRefunds_series, rows = num_wageSubsRefunds_rows,
    filename_to_write = "COVID-19 MSD Number of Wage Subsidy Refunds.xlsx")
msd(df = wageSubsRefunds, series = am_wageSubsRefunds_series, rows = am_wageSubsRefunds_rows,
    filename_to_write = "COVID-19 MSD Amount of Wage Subsidy Refunds.xlsx")
msd(df = jobseeker_MSD, series = jobseeker_MSD_series, rows = jobseeker_MSD_rows,
    filename_to_write = "COVID-19 MSD Jobseeker Support by MSD Region.xlsx")
msd(df = suppleSupport, series = suppleSupport_accom_series, rows = suppleSupport_accom_rows,
    filename_to_write = "COVID-19 MSD Accommodation Supplement.xlsx")
msd(df = suppleSupport, series = suppleSupport_temp_series, rows = suppleSupport_temp_rows,
    filename_to_write = "COVID-19 MSD Temporary Additional Support and Special Benefit.xlsx")
msd(df = specialGrants, series = specialGrants_all_series, rows = specialGrants_all_rows,
    filename_to_write = "COVID-19 MSD All Special Needs Grants.xlsx")
msd(df = specialGrants, series = specialGrants_food_series, rows = specialGrants_food_rows,
    filename_to_write = "COVID-19 MSD Special Needs Grants for Food.xlsx")
msd(df = specialGrants, series = specialGrants_housing_series, rows = specialGrants_housing_rows,
    filename_to_write = "COVID-19 MSD Emergency Housing Grants.xlsx")
msd(df = jobseeker_region, series = jobseeker_region_series, rows = jobseeker_region_rows,
    filename_to_write = "COVID-19 MSD Jobseeker Support By Region.xlsx")
