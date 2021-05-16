# Food parcel distribution (Salvation Army)

library(dplyr)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
raw <- "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/Portal Data Supply/Salvation Army"

files <- file.info(list.files(raw, full.names = T))
update <- read_excel(path = rownames(files)[which.max(files$mtime)], col_names = TRUE, skip = 2) %>%
  filter(update[[1]] == "Total Food Parcels") %>%
  select(-1) %>%
  pivot_longer(everything(), names_to = "Week Ending", values_to = "Total Food Parcels") %>%
  drop_na()

update$`Week Ending` <- as.Date(as.numeric(update$`Week Ending`), origin = "1899-12-30")

file.rename(from = paste0(directory, "/COVID 19 - Food parcel distribution.xlsx"), to = paste0(directory, "/Previous/COVID 19 - Food parcel distribution.xlsx"))
writexl::write_xlsx(x = update, path = paste0(directory, "COVID 19 - Food parcel distribution.xlsx"))
