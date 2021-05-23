# COVID-19 vaccines administered – cumulative +++ daily total

library(readxl)
library(openxlsx)
#library(xlsx)
#library(writexl)
library(stringr)


directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "MoH/Vaccination")
files <- file.info(list.files(path, full.names = T, pattern = ".*\\.xlsx"))

filename <- rownames(files)[which.max(files$ctime)]
date <- str_extract(string = filename, pattern = "\\d\\d_\\d\\d_\\d\\d\\d\\d") %>%
  dmy() %>%
  format("%d %B %Y")

sheet_sum <- "Summary"
sheet_workforce <- "Workforce"
sheets_cum <- c("DHBofResidence", "Ethnicity", "Gender", "Age")
sheet_daily <- "Date"
sheet_plan <- "Plan"

file.rename(from = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
            to = paste0(directory, "Previous/COVID-19 MoH Vaccination - Cumulative.xlsx"))

file.rename(from = paste0(directory, "COVID-19 MoH Vaccination - Daily.xlsx"),
            to = paste0(directory, "Previous/COVID-19 MoH Vaccination - Daily.xlsx"))

file.rename(from = paste0(directory, "COVID-19 MoH Vaccination - Workforce.xlsx"),
            to = paste0(directory, "Previous/COVID-19 MoH Vaccination - Workforce.xlsx"))

OUT_daily <- createWorkbook()
for (sheet in sheet_daily) {
  df_daily <- as.data.frame(read_excel(filename, sheet = sheet))
  df_daily$Date <- ymd(df_daily[[1]])
  # xlsx::write.xlsx(df_daily,
  #            #paste0(directory, "COVID-19 MoH Vaccination - Daily.xlsx"),
  #            paste0("COVID-19 MoH Vaccination - Daily.xlsx"),
  #            sheetName = sheet,
  #            append = TRUE,
  #            showNA = FALSE,
  #            row.names = FALSE,
  #            col.names = TRUE)
  addWorksheet(OUT_daily, sheet)
  writeData(OUT_daily, sheet = sheet, x = df_daily)
}

saveWorkbook(OUT_daily, paste0(directory, "COVID-19 MoH Vaccination - Daily.xlsx"))


OUT_cum <- createWorkbook()
df_sum <- as.data.frame(read_excel(filename, sheet = sheet_sum)) %>%
  mutate(Date = date) %>%
  select(Date, everything())
addWorksheet(OUT_cum, sheet_sum)
writeData(OUT_cum, sheet = sheet_sum, x = df_sum)

# xlsx::write.xlsx(df_sum,
#            #file = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
#            file = paste0("COVID-19 MoH Vaccination - Cumulative.xlsx"),
#            sheetName = sheet_sum,
#            append = TRUE,
#            showNA = FALSE,
#            col.names = TRUE,
#            row.names = FALSE)

for (sheet in sheets_cum) {
  df_cum <- as.data.frame(read_excel(filename, sheet = sheet))
  colnames(df_cum) <- paste0(colnames(df_cum), ", as at ", date)
  # xlsx::write.xlsx(df_cum,
  #            #file = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
  #            file = paste0("COVID-19 MoH Vaccination - Cumulative.xlsx"),
  #            sheetName = sheet,
  #            append = TRUE,
  #            showNA = FALSE,
  #            col.names = TRUE,
  #            row.names = FALSE)
  addWorksheet(OUT_cum, sheet)
  writeData(OUT_cum, sheet = sheet, x = df_cum)
}

df_plan <- as.data.frame(read_excel(filename, sheet = sheet_plan))
df_plan$Date <- ymd(df_plan$Date)
addWorksheet(OUT_cum, sheet_plan)
writeData(OUT_cum, sheet = sheet_plan, x = df_plan)

# xlsx::write.xlsx(df_plan,
#            #file = paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"),
#            file = paste0("COVID-19 MoH Vaccination - Cumulative.xlsx"),
#            sheetName = sheet_plan,
#            append = TRUE,
#            showNA = FALSE,
#            col.names = TRUE,
#            row.names = FALSE)
saveWorkbook(OUT_cum, paste0(directory, "COVID-19 MoH Vaccination - Cumulative.xlsx"))

OUT_workforce <- createWorkbook()
df_workforce <- as.data.frame(read_excel(filename, sheet = sheet_workforce))
df_workforce$Date <- ymd(df_workforce$Date)
addWorksheet(OUT_workforce, sheet_workforce)
writeData(OUT_workforce, sheet = sheet_workforce, x = df_workforce)
# xlsx::write.xlsx(df_workforce,
#            #file = paste0(directory, "COVID-19 MoH Vaccination - Workforce.xlsx"),
#            file = paste0("COVID-19 MoH Vaccination - Workforce.xlsx"),
#            sheetName = sheet_workforce,
#            append = TRUE,
#            showNA = FALSE,
#            col.names = TRUE,
#            row.names = FALSE)
saveWorkbook(OUT_workforce, paste0(directory, "COVID-19 MoH Vaccination - Workforce.xlsx"))