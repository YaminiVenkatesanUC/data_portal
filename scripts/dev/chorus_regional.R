directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "chorus regional/")
master <- read_excel(paste0(path, "COVID 19 - Chorus regional broadband master.xlsx"))
files <- file.info(list.files(path, full.names = T))
update <- read.csv(rownames(files)[which.max(files$mtime)])
update <- select(update, -weekday)
update <- update %>%
  dplyr::rename(Date = `Day.of.timecapturednzst5min`) %>%
  dplyr::rename(`Measure Names` = `Measure.Names`) %>%
  dplyr::rename(`Region Name` = `Region.Name`) %>%
  dplyr::rename(`Measure Values` = `Measure.Values`)
update$Date <- as.Date(update$Date, format = "%a, %B %d, %Y")

output <- rbind(master, update)

output <- output %>%
  group_by(Date, `Region Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  spread(`Region Name`, `Measure Values`)

file.rename(from = paste0(directory, "/COVID 19 - Chorus regional broadband.xlsx"), to = paste0(directory, "/Previous/COVID 19 - Chorus regional broadband.xlsx"))
writexl::write_xlsx(x = output, path = paste0(directory, "COVID 19 - Chorus regional broadband.xlsx"))
