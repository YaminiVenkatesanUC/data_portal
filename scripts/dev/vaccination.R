# COVID-19 vaccines administered – cumulative +++ daily total

library(readxl)
library(xlsx)
library(writexl)
library(stringr)


directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "MoH/Vaccination")
files <- file.info(list.files(path, full.names = T, pattern = ".*\\.xlsx"))

filename <- rownames(files)[which.max(files$ctime)]
date <- str_extract(string = filename, pattern = "\\d\\d_\\d\\d_\\d\\d\\d\\d") %>%
  dmy() %>%
  format("%d %B %Y")

sheet_sum <- "Summary"
sheets_cum <- c("DHBofResidence", "Ethnicity", "Gender", "Age")
sheet_daily <- "Date"

file.rename(from = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
            to = paste0(directory, "Previous/COVID-19 MoH Vaccination - Cumulative.xlsx"))

file.rename(from = paste0(directory, "COVID-19 MoH Vaccination - Daily.csv"),
            to = paste0(directory, "Previous/COVID-19 MoH Vaccination - Daily.csv"))

df_sum <- as.data.frame(read_excel(filename, sheet = sheet_sum)) %>%
  mutate(Date = date) %>%
  select(Date, everything())
write.xlsx(df_sum,
           file = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
           sheetName = sheet_sum,
           append = TRUE,
           showNA = FALSE,
           col.names = TRUE,
           row.names = FALSE)

for (sheet in sheets_cum) {
  df_cum <- as.data.frame(read_excel(filename, sheet = sheet))
  colnames(df_cum) <- paste0(colnames(df_cum), ", as at ", date)
  write.xlsx(df_cum,
             file = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
             sheetName = sheet,
             append = TRUE,
             showNA = FALSE,
             col.names = TRUE,
             row.names = FALSE)
}

df_daily <- as.data.frame(read_excel(filename, sheet = sheet_daily))
write.csv(df_daily, paste0(directory, "COVID-19 MoH Vaccination - Daily.csv"), row.names = FALSE)
