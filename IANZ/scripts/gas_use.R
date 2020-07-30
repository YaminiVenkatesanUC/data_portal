library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(xlsx)
source("scripts/file_paths.R")


gas_use<-read_excel(paste0(path,"COVID 19 - Industrial Gas use.xlsx"), sheet = 1, skip = 1)%>%select(-`Source: First Gas`)
gas_use[is.na(gas_use)]<-0

maui_pipeline<-read_excel(paste0(path,"COVID 19 - Industrial Gas use.xlsx"), sheet = 2, skip = 1)%>%select(-`Source: First Gas`)


vector<-gas_use%>% select(Date=`...1`,  everything())%>%
  mutate(Date=dmy(Date), `Ballance Agri- Nutrients`=`Ballance Agri-Nutrients...2`+`Ballance Agri-Nutrients...3`, Fonterra=`Subtotal Fonterra...6`+`Subtotal Fonterra...7`+
           `Subtotal Fonterra...8`+`Subtotal Fonterra...9`+`Subtotal Fonterra...10`+`Subtotal Fonterra...11`+`Subtotal Fonterra...12`+`Subtotal Fonterra...13`+
           `Subtotal Fonterra...14`)%>%select(Date, Fonterra,`Ballance Agri- Nutrients`,`Glenbrook steel mill`, `Kinleith pulp and paper mill`, `Marsden Point oil refinery`)%>%
  as.data.frame()


maui<-maui_pipeline%>%mutate(Date=dmy(`...1`), Methanex=`...4`+`Methanex Motunui`)%>%select(Date, everything(), -`...1`, -`...4`, -`Methanex Motunui`)%>%
  as.data.frame()




setwd(dir)

write.xlsx2(maui,"COVID 19 - Industrial Gas use aggregated.xlsx", sheetName = "maui" , row.names = F)
write.xlsx2(vector,"COVID 19 - Industrial Gas use aggregated.xlsx", sheetName = "vector", append = T, row.names = F)

setwd(main)
