##note run load and shiny app before running this script
library(xlsx)
source("scripts/extract_indicator_list.R")

by_class<-output%>%select(class, indicator_name, source)%>%mutate(number_of_charts=1)%>%group_by(indicator_name, class, source)%>%
  summarise_if(is.numeric, sum, na.rm=T)%>%ungroup()%>%group_by(class, indicator_name, source)%>%as.data.frame()

data_definitions <- read_json(CONFIG$data_definitions)

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
list_indicators<-merge(indicator_data, by_class, all = T)%>%select(-number_of_charts)%>%as.data.frame()

run_date<-Sys.Date()
setwd("~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/IANZ v2/reports/COVID - 19")

write.xlsx(list_indicators, paste0("COVID - 19 data portal indicators ", run_date, ".xlsx"), sheetName = "source", row.names = FALSE)
write.xlsx(by_class, paste0("COVID - 19 data portal indicators ", run_date, ".xlsx"), sheetName = "chart_count", append = TRUE, row.names = FALSE)

setwd("~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/supply_ Use/R/collab/data_portal")




