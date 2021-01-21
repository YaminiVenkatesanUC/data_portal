library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
source("RB/file_paths.R")



  nzac_data<-read_excel(paste0(meta, "december_nzac.xlsx"), sheet = 1)
  bnz_pmi_data<-read_excel(paste0(dir, "COVID 19 - BNZ insights.xlsx"),  sheet=1)
  back_series_traffic<-read_excel(paste0(meta, "final data.xlsx"), sheet = 1)
  traffic<-read_excel(paste0(dir,"COVID 19 - ANZ insights.xlsx"), sheet = 3)
  anz_outlook<-read_excel(paste0(dir,"COVID 19 - ANZ insights.xlsx"), sheet = 1)
  grid<-read_excel(paste0(dir, "COVID 19 - EMA Grid demand.xlsx"), sheet = 3, skip = 1)
  jobs<-read_excel(paste0(dir,"COVID 19 - SEEK NZ job ads online index.xlsx"), sheet = 1)
  ECT_data<-read_excel(paste0(dir,"COVID 19 - Electronic Card Transaction data.xlsx"), sheet = 1)
##########################################

  colnames(nzac_data)[3]<-"nzac"
  NZAC<-nzac_data%>%
    select(Date=date, nzac)%>%
    mutate(Date=ymd(Date))

  max_date<-max(ymd(nzac_data$date))
  start_date<-'2003-09-01'
  ###anz outlook

  anz_activity_outlook<-anz_outlook%>%
    select(Date, anz_outlook=`Activity outlook - All sectors`)%>%
    mutate(Date=ymd(Date))%>%
    filter(Date<=max_date)
#bnz pmi
bnz_pmi<-bnz_pmi_data%>%select(Date,pmi=PMI)%>%  mutate(Date=ymd(Date))

  #traffic data
back_traffic<-back_series_traffic%>%select(Date=date, heavy_traffic, light_traffic)%>%filter(Date<"2015-04-01")
traffic_portal<-traffic%>%select(Date,heavy_traffic= `Heavy traffic index`, light_traffic= `Light traffic index`)


traffic_data<-rbind(back_traffic, traffic_portal)%>%
  mutate(Date=ymd(Date),
         lag_heavy = lag(heavy_traffic, 12),
         lag_light=lag(light_traffic,12),
         heavy_apc=(heavy_traffic/lag_heavy-1)*100,
         light_apc=(light_traffic/lag_light-1)*100)%>%
  filter(Date>=start_date)


##electricity generation (hydro+thermal) use
grid_electricity<-grid%>%
  select(Date=`Period start`,Demand= `Demand (GWh)`)%>%
  mutate(Date=ymd(Date),
         Demand_lag=lag(Demand,12),
         demand_apc= (Demand/ Demand_lag-1)*100)%>%
  filter(Date>= start_date)

##SEEk job listings
job_listing<-jobs%>%
  select(Date, SEEK_jobs=`NZ Job ad Index`)%>%
  mutate(Date=ymd(Date),
         Seek_jobs_lag=lag(SEEK_jobs, 12),
         seek_apc=(SEEK_jobs/Seek_jobs_lag-1)*100)%>%
  filter(Date>= start_date)

###ECT

ECT<-ECT_data%>%
  select(Date=...1,ECT_spend= `Total values - Electronic card transactions (Actual)`)%>%
  mutate(Date=ymd(Date),
         ECT_spend=ECT_spend*1000000,
         ECT_lag=lag(ECT_spend, 12),
         ect_apc= (ECT_spend/ ECT_lag-1)*100)%>%
  filter(Date>= start_date)




all_data_merge<-Reduce(function(x,y) merge(x, y, all =T),list(NZAC ,bnz_pmi,anz_activity_outlook,
                                                              grid_electricity, traffic_data, job_listing, ECT))

all_data<-all_data_merge%>%
  select(Date, nzac, anz_outlook, ect_apc, demand_apc, pmi, seek_apc, heavy_apc, light_apc, everything())%>%
  filter(Date>= start_date)


   write_xlsx(all_data, paste0(dir,"NZAC portal data.xlsx"))
 write_xlsx(all_data, paste0(meta,"NZAC portal data ", Sys.Date(), ".xlsx"))






