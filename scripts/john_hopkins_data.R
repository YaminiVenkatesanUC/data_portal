
library (RCurl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)
library(lubridate)


output<- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"

country<-c("Spain", "Italy","US",  "United Kingdom","Australia", "Canada", "Singapore" ,"China")

##get data from github
dth<-getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths_1 <- read.csv (text = dth)

cnf<-getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed_1<-read.csv(text=cnf)

rcv<-getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered_1<-read.csv(text=rcv)


JH_raw_data<-function(data, var){
data<-data
var=var

Country<-grep("Country", names(data))
Province<-grep("Province", names(data))
date<-grep("X", names(data))

colnames(data)[Country]<-"Country"
colnames(data)[Province]<-"Province"
new_data<-data[,c(Country,Province, date)]

new_data1<-new_data%>%gather(Date, var, -Country, -Province )%>%
  mutate(Date=as.Date(str_replace_all(substr(Date, 2, nchar(Date)), "[X.]", "/"), format= "%m/%d/%y"))

colnames(new_data1)[4]<-var

return(new_data1)
}

death_new<- JH_raw_data(deaths_1, "Deceased")
recovered_new<- JH_raw_data(recovered_1, "Recovered")
confirmed_new<- JH_raw_data(confirmed_1, "Active")


all_data<-Reduce(function(x, y) merge(x, y, all=TRUE), list(death_new, recovered_new, confirmed_new))

all_data[is.na(all_data)]<-0





df_cases_new<-all_data%>%select(-Province)%>% group_by(Date, Country )%>%
  dplyr::summarise(Active=sum(Active), Recovered=sum(Recovered),Deceased=sum(Deceased))%>%
  filter(Country %in% country)



df_cases_new_ROW<-all_data%>%
  filter(!Country %in% country)%>%select(-Country)%>%group_by(Date)%>%
  summarise_if(is.numeric, sum, na.rm=T)%>%
  mutate(Country="Rest of the world")%>%
  select(Country, everything())

# df_cases_all<-full_join(df_cases_new, df_cases_new_ROW)

aus_provinces<-all_data%>%filter(Country=='Australia')%>%mutate(Country= paste0(Country, " - ", Province))%>%
  select(-Province)


df_cases_all<-Reduce(function(x,y) merge(x = x, y = y, all=TRUE),
       list(df_cases_new, df_cases_new_ROW, aus_provinces))%>%mutate(Active=Active-Recovered-Deceased)

df_cases_all[is.na(df_cases_all)]<-0

##plot to check if data is ok

df_cases4<-df_cases_all

df_cases4<-df_cases4%>%mutate(Active=log(Active), Recovered=log(Recovered), Deceased=log(Deceased))

cases_policy<-df_cases4%>%
  gather(type, value, -Country, -Date)%>%
  filter(type=="Active")

p<-ggplot(cases_policy, aes(Date,value, fill=type ))+geom_bar(stat="identity")+facet_wrap(.~Country, ncol=3)+
  theme_minimal()+ theme(legend.position="none")+geom_bar(stat="identity")+
  scale_fill_manual(values = c("orange","black", "green"))+ggtitle("log of active COVID 19 cases")


ggplotly(p)




write_xlsx(df_cases_all,paste0(output,"COVID 19 - Global cases.xlsx"))



