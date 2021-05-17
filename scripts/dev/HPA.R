# Drinking behaviours +++ Experienced harm due to drinking +++ Worry about drinking +++ Smoking behaviours +++ Worry about smoking

library(dplyr)
library(readxl)
library(writexl)

## Setting up config to read the fields from raw file
raw_file <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data/Development/210310 HPA request selected tables Impact of COVID-19.xlsx"

colnames <- c("Parameter","Total","Wave 1","Wave 2")

drinking_categories <- c("Less than you usually did before lockdown","About the same as you usually did before lockdown","More than you usually did before lockdown")
binary_categories <- c("Yes","No")
harm_categories <- c("Experience harm (Net)","No Harm (Net)")
smoking_categories <- categories <- c("Less than you usually did before lockdown","About the same as you usually did before lockdown","More than you usually did before lockdown")
age_categories <- c("18-24","25-49","50-64","65+")

output_file_path <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"

## Read HPA data

read_hpa_data <- function(file,sheet,range_start,range_end,categories,output_file){
  data <-
    as.data.frame(read_excel(file,
                             sheet = sheet,
                             range = cell_limits(range_start, range_end),
                             .name_repair = "minimal"))

  names(data) <- colnames

  data <- data %>% filter(Parameter %in% categories) %>% select(-Total)

  data[,2:ncol(data)] <- sapply(data[,2:ncol(data)],as.numeric)

  return(data)
}

read_hpa_drinking_by_age <-
  function(file,
           sheet,
           n_cols,
           range_start,
           range_end,
           categories) {

    data <-
      read_excel(file,
                 sheet = sheet,
                 col_names = paste0("col_",1:n_cols),
                 range = cell_limits(range_start, range_end),
                 .name_repair = "minimal")

    data <- as.data.frame(t(data),stringsAsFactors = FALSE)
    data <- data[colSums(!is.na(data)) > 0]

    names(data) <- c("Age","Wave","Total","Less than you usually did before lockdown","About the same as you usually did before lockdown","More than you usually did before lockdown")

    data <- data %>% filter( Age %in% categories) %>% select(-Total)

    data[3:ncol(data)] <- sapply(data[3:ncol(data)],as.numeric)


    data <- as.data.frame(data %>% pivot_longer(cols = 3:ncol(data),names_to = "Parameter"),stringAsFactors = FALSE)

    data <- as.data.frame(data %>% pivot_wider(names_from = Wave,values_from = value),stringAsFactors = FALSE)

    return(data)
  }



## Write to excel

write_hpa_data <- function(data,output_file){

  write_xlsx(x = data,path = output_file,col_names = TRUE)
}


## Drinking behaviour since lockdown

overall_drinking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(10, 2),
    range_end = c(18, 5),
    categories = categories
  )

overall_drinking$Parameter <- gsub(" you","",overall_drinking$Parameter)

#write_hpa_data(overall_drinking,output_file = paste0(output_file_path,"COVID 19 - HPA Overall Drinking.xlsx"))

## Drinking behaviour since lockdown by age

drinking_by_age <-
  read_hpa_drinking_by_age(
    file = raw_file,
    sheet = 2,
    n_cols = 10,
    range_start = c(29, 2),
    range_end = c(38, 11),
    categories = age_categories
  )

## Combine overall and drinking by age together

overall_drinking <- overall_drinking %>% mutate(Age = "Total")
drinking_data <- rbind(overall_drinking,drinking_by_age)

drinking_data$Parameter <- gsub(" you","",drinking_data$Parameter)


#write_hpa_data(drinking_by_age,output_file = paste0(output_file_path,"COVID-19 - HPA Drinking By Age.xlsx"))

write_hpa_data(drinking_data,output_file = paste0(output_file_path,"COVID-19 - HPA Drinking data.xlsx"))

## worrying about own drinking

worry_own_drinking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(49, 2),
    range_end = c(54, 5),
    categories = binary_categories,
    output_file = "COVID-19 - HPA_Worry_Drinking.xlsx"
  )

## worry about someone else's drinking


worry_others_drinking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(64, 2),
    range_end = c(70, 5),
    categories = binary_categories
  )




files_to_write <- list(Own_drinking = worry_own_drinking,Others_drinking = worry_others_drinking)

write_hpa_data(files_to_write,output_file = paste0(output_file_path,"COVID-19 - HPA Worry Drinking.xlsx"))



## Expereinced harm due to own drinking

harm_own_drinking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(79, 2),
    range_end = c(121, 5),
    categories = harm_categories
  )

harm_own_drinking$Parameter <- gsub(pattern = " \\(Net)","",gsub("Experience","Experienced",harm_own_drinking$Parameter))


## Experienced harm due to someone else's drinking


harm_others_drinking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(128, 2),
    range_end = c(168, 5),
    categories = harm_categories
  )

harm_others_drinking$Parameter <- gsub(pattern = " \\(Net)","",gsub("Experience","Experienced",harm_others_drinking$Parameter))

files_to_write <- list(harm_own_drinking = harm_own_drinking,harm_others_drinking = harm_others_drinking)

write_hpa_data(files_to_write,output_file = paste0(output_file_path,"COVID-19 - HPA Exp Harm By Drinking.xlsx"))

## Smoking behaviour since lockdown


smoking_data <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(176, 2),
    range_end = c(186, 5),
    categories = smoking_categories
  )

smoking_data$Parameter <- gsub(" you","",smoking_data$Parameter)

write_hpa_data(smoking_data,output_file = paste0(output_file_path,"COVID-19 - HPA Smoking.xlsx"))

## Worry about own smoking

worry_own_smoking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(197, 2),
    range_end = c(203, 5),
    categories = binary_categories
  )


## Worry about someone else's smoking


worry_others_smoking <-
  read_hpa_data(
    file = raw_file,
    sheet = 2,
    range_start = c(213, 2),
    range_end = c(219, 5),
    categories = binary_categories
  )

files_to_write <- list(worry_own_smoking = worry_own_smoking,worry_others_smoking = worry_others_smoking)

write_hpa_data(files_to_write,output_file = paste0(output_file_path,"COVID-19 - HPA Worry Smoking.xlsx"))





