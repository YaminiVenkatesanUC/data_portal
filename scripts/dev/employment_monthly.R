# Monthly filled jobs +++ Monthly earnings +++ Monthly filled jobs (by industry) +++
# Monthly filled jobs (by region) +++ Monthly filled jobs (by gender) +++ Monthly filled jobs (by age)

library(openxlsx)
library(dplyr)

load_parameters <- list(
  Monthly_earnings = list(
    Group = "High level industry by variable",
    Series_title_1 = "Earnings - cash",
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

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
update <- paste0(directory, "employment-indicators-monthly.csv")

data <- read.csv(update, stringsAsFactors = FALSE) %>%
  filter(Subject == "Employment indicators - MEI") %>%
  filter(substr(Series_reference, 4, 4) == "M")

col_names <- c("Series_reference", "Period", "Data_value", "Suppressed", "STATUS", "UNITS", "Magnitude", "Subject", "Group",
               "Series_title_1", "Series_title_2", "Series_title_3", "Series_title_4", "Series_title_5")
if (length(setdiff(names(data), col_names)) > 0) stop ("New column added: ", setdiff(names(data), col_names))


for (ind in 1:length(names(load_parameters))) {
  file.rename(from = paste0(paste0(directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx")),
              to = paste0(paste0(directory, "Previous/COVID-19 ", names(load_parameters)[ind], ".xlsx")))
}



for (ind in 1:length(load_parameters)) {
  param <- load_parameters[[ind]]
  print(names(load_parameters)[[ind]])

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

  OUT <- createWorkbook()
  if (ind %in% c(1, 2, 3, 4)) {
    for (group in unique(df$Series_title_2)) {
      output <- df %>%
        filter(Series_title_2 == group) %>%
        select(-Series_title_2) %>%
        pivot_wider(names_from = Series_title_3, values_from = Value) %>%
        as.data.frame()

      addWorksheet(OUT, str_trunc(group, 30, "right", ""))
      writeData(OUT, sheet = str_trunc(group, 30, "right", ""), x = output)
      # #append in this function is no longer behaving properly...
      # write.xlsx(x = output, paste0(directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx"),
      #            sheetName = group,
      #            append = TRUE,
      #            row.names = FALSE)
    }


  } else {
    for (group in unique(df$Series_title_3)) {
      output <- df %>%
        filter(Series_title_3 == group) %>%
        select(-Series_title_3) %>%
        pivot_wider(names_from = Series_title_2, values_from = Value) %>%
        as.data.frame()

      addWorksheet(OUT, str_trunc(group, 30, "right", ""))
      writeData(OUT, sheet = str_trunc(group, 30, "right", ""), x = output)
      # #append in this function is no longer behaving properly...
      # write.xlsx(x = output, paste0(directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx"),
      #            sheetName = group,
      #            append = TRUE,
      #            row.names = FALSE)
    }

  }
  saveWorkbook(OUT, paste0(directory, "COVID-19 ", names(load_parameters)[ind], ".xlsx"))
}
