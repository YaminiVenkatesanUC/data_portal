library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
source("scripts/file_paths.R")



library(zoo)
zd <- read.zoo(text = Lines, header = TRUE)
tt <- as.yearmon(seq(start(zd), end(zd), "month"))
zm <- na.spline(zd, as.yearmon, xout = tt)

qgdp<-read.csv(paste0(gdp,"gross-domestic-product-march-2020-quarter.csv"), stringsAsFactors = F)

gdp_monthly<-qgdp%>%select(Date=Period, Data_value, Series_title_1, Group)%>%filter(Series_title_1=='Gross Domestic Product - production measure',
                                                                            Group=="Series, GDP(P), Chain volume, Seasonally adjusted, Total, Percentage change" )%>%
  mutate(Date=ymd(paste0( str_replace( as.character(Date), "[.]", "-"), "-1")))%>%
  select(Date, Data_value)

# gdp_monthly[is.na(gdp_monthly)]

gdp_month_zoo<-read.zoo(gdp_monthly, format ="%Y-%m-%d")
monthy_gdp <- as.yearmon(seq(start(gdp_month_zoo), end(gdp_month_zoo), "month"))
monthy_gdp_int <- na.spline(gdp_month_zoo, as.yearmon, xout = monthy_gdp)

