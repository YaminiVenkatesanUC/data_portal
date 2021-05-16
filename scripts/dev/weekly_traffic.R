# Weekly traffic count

library(readxl)
library(dplyr)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard"
path <- paste0(directory, "/TMS")
files <- file.info(list.files(path, full.names = TRUE))
update <- read_excel(rownames(files)[which.max(files$mtime)], sheet = "Sumated_Data")
update$Day <- ymd(update$Day)

regions <- unique(update$Region)

for (region in regions) {
  data <- update %>%
    select(Day, Type, Region, TrafficCount, Exclude_Calculations) %>%
    filter(Region == region) %>%
    filter(Exclude_Calculations == 'N') %>%
    select(-Region) %>%
    pivot_wider(names_from = Type, values_from = TrafficCount) %>%
    select(Day, `Light Vehicles`, `Heavy Vehicles`)

  file.rename(from = paste0(directory, "/COVID-19 - Weekly traffic count - ", region, ".xlsx"),
              to = paste0(directory, "/Previous/COVID-19 - Weekly traffic count - ", region, ".xlsx"))
  writexl::write_xlsx(x = data, path = paste0(directory, "/COVID-19 - Weekly traffic count - ", region, ".xlsx"))

}

