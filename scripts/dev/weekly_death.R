# Weekly deaths: By region, By age and sex, By age

library(dplyr)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "Weekly deaths data DIA")
files <- file.info(list.files(path, full.names = T))
update <- rownames(files)[which.max(files$mtime)]
file.rename(from = paste0(directory, "COVID 19 - DIA Preliminary Death Data.xlsx"), to = paste0(directory, "Previous/COVID 19 - DIA Preliminary Death Data.xlsx"))
file.copy(from = update, to = paste0(directory, "COVID 19 - DIA Preliminary Death Data.xlsx"))
