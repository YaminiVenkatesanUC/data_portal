library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

source("scripts/file_paths.R")


work<-read.csv(paste0(visa, "Population_by_Application_Substream work.csv"), stringsAsFactors = F)%>%mutate(Date=ymd(Date), Count=as.numeric(Count))
student<-read.csv(paste0(visa,"Population_by_Visa_Type_and_Application_Substream_Students.csv"), stringsAsFactors = F)%>%spread(Application_Substream, Count)

other_visa<-c("Business", "Crew of foreign fishing vessel", "Humanitarian/International", "Other" ,"Post Study Work" ,"Specific purposes","Student and trainee",
         "Work to residence" )

other<-work%>%filter(Application_Substream %in% other_visa )%>%
  group_by(Date)%>%summarise_if(is.numeric, sum, na.rm=T)%>%mutate(Application_Substream='Work other')

work_visa<-work%>%filter(!Application_Substream %in% other_visa)

work_visa_data<-rbind(work_visa, other)%>%spread(Application_Substream, Count)




write.csv(work_visa_data, paste0(dir, "Population_by_Visa_Type_and_Application_Substream_Work.csv"), row.names = F)
write.csv(student, paste0(dir, "Population_by_Visa_Type_and_Application_Substream_Students.csv"), row.names = F)
