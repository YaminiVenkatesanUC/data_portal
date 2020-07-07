library(jsonlite)
library(R6)
library(httr)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)


source("R/core/build_ui.R")
source("R/core/utils.R")
source("R/core/download_modal.R")
source("R/core/about_modal.R")
source("R/core/main_panel.R")
source("R/core/consts.R")
source("R/core/get_download_csv.R")
source("R/core/type_checks.R")
source("R/core/data_interface.R")
source("R/load_functions.R")
source("R/plot_functions.R")
source("R/data_types.R")
source("R/data_service_functions.R")

options(scipen = 999)

CONFIG <- read_config_file()

indicator_definitions_raw <- read_json(CONFIG$data_definitions)



list_ind<-"/home/STATSNZ/rbaltaza/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/supply_ Use/R/collab/data_portal/"
indicator_files<-"/home/STATSNZ/rbaltaza/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/supply_ Use/R/collab/data_portal/config/covid_19/"

indicator_list<-read.csv(paste0(list_ind, "indicator_register.csv"), stringsAsFactors = F)

by_class<-indicator_list%>%select(class, indicator_name, source)%>%mutate(Number_of_charts=1)%>%group_by(indicator_name, class, source)%>%
  summarise_if(is.numeric, sum, na.rm=T)%>%ungroup()%>%group_by(class, indicator_name, source)



# data_definitions<-fromJSON(file= paste0(indicator_files,"covid_19_data_definitions.json"))




indicator_list<-function(data_definitions){

for (i in 1: length(data_definitions)){
  
  if (i==1){
    
    df<-data.frame(unlist(data_definitions[i]))
    data<- t(df)
    
  }else{
    
    df2<-data.frame(unlist(data_definitions[i]))
    df2<- t(df2)
    data<-merge(data, df2, all=T)
  }
  
}

return(data)
}

indicator_data<-indicator_list(data_definitions)%>%select(class, type, indicator_name, filename) 

list_indicators<-merge(indicator_data, by_class, all = T)

write_xlsx(list_indicators, paste0(list_ind, "COVID - 19 data portal indicators.xlsx"))



