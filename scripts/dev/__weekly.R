# MONDAY ----------------------------------------------------------
# Broadband usage by region
source("scripts/dev/chorus_regional.R")

# Weekly deaths: By region, By age and sex, By age
source("scripts/dev/weekly_death.R")


# WEDNESDAY -------------------------------------------------------
# COVID-19 vaccines administered – cumulative +++ daily total
source("scripts/dev/vaccination.R")

# Exports by country (values) +++ Imports by country (values) +++ Imports by commodity (values) +++ Exports by commodity (values)
source("scripts/dev/trade_data.R")

# Weekly traffic count
source("scripts/dev/weekly_traffic.R")

# Trade weighted index +++ Bank bill yields +++ Foreign exchange +++ Baltic dry index +++ Commodity prices
# Market volatility index +++ Global stock markets +++ New Zealand stock exchange +++ Global dairy trade
# Property sales - China +++ Transport congestion - China
# Property sales - China +++ Investor sentiment
source("scripts/dev/treasury.R")


# THURSDAY ---------------------------------------------------------
# Weekly filled jobs (34 days) +++ Weekly median earnings (34 days)
source("scripts/dev/employment_weekly.R")




# LOAD -----------------------------------------------------
source("scripts/run_load_process.R")
