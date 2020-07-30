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

write.xlsx(list_indicators, "COVID - 19 data portal indicators.xlsx", sheetName = "Sheet1", row.names = FALSE)
write.xlsx(by_class, "COVID - 19 data portal indicators.xlsx", sheetName = "Sheet2", append = TRUE, row.names = FALSE)




