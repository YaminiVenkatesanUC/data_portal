library(dplyr)
library(readxl)


## Drinking behaviour since lockdown
data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
  col_names = paste0("col_",2:5),
  range = cell_limits(c(13,2),c(18,5))
  ) %>% select(-col_3)


names(data) <- c("Parameter","Wave 1","Wave 2")

data <- data %>% na.omit()

write_file_excel("Overall drinking", data)

write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Overall drinking",append = TRUE)


## Drinking behaviour since lockdown by age


data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",1:10),
    range = cell_limits(c(29,2),c(38,11)),
    .name_repair = "minimal"
  )

names(data) <- c("Parameter","Total","18-24 W1","18-24 W2","25-49 W1","25-49 W2","50-65 W1","50-65 W2","65+ W1","65+ W2")
data <- data %>% na.omit()
data <- data %>% select(-Total) %>% filter(Parameter != "Total (Weighted)")

data_long <- data %>% pivot_longer(data,cols = 2:ncol(data), names_to = "Age")

data_long %>% separate(data_long, c("Age","Wave"), sep = " ")


#names(data) <- c("Parameter","Total","Wave 1","Wave 2")

data <- data %>% na.omit()

# not_all_na <- function(x) any(!is.na(x))
# data %>% select(where(not_all_na))
#
# data <- data[,colSums(is.na(data))<nrow(data)]


#write_file_excel("Drinking by age", data)

#openxlsx ::write.xlsx(data,"COVID 19 HPA data.xlsx", row.names = FALSE, sheetName = "Overall drinking")

data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",1:10),
    range = cell_limits(c(29,2),c(38,11)),
    .name_repair = "minimal"
  )
data <- as.data.frame(t(data))
data <- data[colSums(!is.na(data)) > 0]

data <- data[-2,]

names(data) <- c("Age","Wave","Total","Less than you usually did before lockdown","About the same as you usually did before lockdown","More than you usually did before lockdown")

data <- data[-1,] %>% select(-Total)

data <- as.data.frame(data %>% pivot_longer(cols = 3:ncol(data),names_to = "Parameter"),stringAsFactors = FALSE)

data <- as.data.frame(data %>% pivot_wider(names_from = Wave,values_from = value))

data$`Wave 1` <- as.numeric(as.character(data$`Wave 1`))

data$`Wave 2` <- as.numeric(as.character(data$`Wave 2`))



openxlsx::write.xlsx(data,"COVID 19 HPA drinking age data.xlsx",row.names= FALSE)
## worrying about own drinking

data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(51,2),c(55,5))
  ) %>% select(-col_3)


data <- data %>% na.omit()
names(data) <- c("Parameter","Wave 1","Wave 2")

write_file_excel("Worry about own drinking", data)

write.xlsx(data,"COVID 19 HPA data.xlsx", row.names = FALSE, sheetName = "Worry about own drinking",append = TRUE)


## worry about someone else's drinking



data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(66,2),c(70,5))
  ) %>% select(-col_3)


data <- data %>% na.omit()

#write_file_excel("Worry others drinking", data)
names(data) <- c("Parameter","Wave 1","Wave 2")

write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Worry about others drinking",append = TRUE)

## Expereinced harm due to own drinking


data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(77,2),c(121,5))
  ) %>% select(-col_3)


data <- data %>% filter(col_2 == "Experience harm (Net)" | col_2 == "No Harm (Net)")
names(data) <- c("Parameter","Wave 1","Wave 2")
#write_file_excel("Exp harm - own drinking", data)

write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Exp harm - own drinking",append = TRUE)

## Experienced harm due to someone else's drinking


data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(128,2),c(168,5))
  ) %>% select(-col_3)


data <- data %>% filter(col_2 == "Experience harm (Net)" | col_2 == "No Harm (Net)")
names(data) <- c("Parameter","Wave 1","Wave 2")
write_file_excel("Exp harm - others drinking", data)
write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Exp harm - others drinking",append = TRUE)

## Somking behaviour since lockdown

data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(178,2),c(186,5))
  ) %>% select(-col_3)


data <- data %>% na.omit()
names(data) <- c("Parameter","Wave 1","Wave 2")

write_file_excel("Smoking behaviour", data)
write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Smoking behaviour",append = TRUE)

## Worry about own smoking



data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(199,2),c(203,5))
  ) %>% select(-col_3)


data <- data %>% na.omit()
names(data) <- c("Parameter","Wave 1","Wave 2")

write_file_excel("Worry about own smoking", data)
write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Worry about own smoking",append = TRUE)
## Worry about someone else's smoking


data <-
  read_excel(
    "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx",
    sheet = 2,
    col_names = paste0("col_",2:5),
    range = cell_limits(c(215,2),c(219,5))
  ) %>% select(-col_3)


data <- data %>% na.omit()
names(data) <- c("Parameter","Wave 1","Wave 2")
write_file_excel("Worry about others smoking", data)
write.xlsx(data,"COVID 19 HPA data.xlsx", sheetName = "Worry about others smoking",append = TRUE)


## Write to excel

library(openxlsx)

write_file_excel <- function(sheetname, data){
  wb <- createWorkbook()

  ## Create the worksheets
  addWorksheet(wb, sheetName = sheetname)
  #addWorksheet(wb, sheetName = "Worry about others drinking")

  ## Write the data
  writeData(wb, sheetname, data)
  #writeData(wb, "Worry about others drinking", data_2)

  ## Save workbook to working directory
  saveWorkbook(wb, file = paste("COVID 19- HPA",".xlsx", sep=""), overwrite = TRUE)
}




