# Trade weighted index +++ Bank bill yields +++ Foreign exchange +++ Baltic dry index +++ Commodity prices
# Market volatility index +++ Global stock markets +++ New Zealand stock exchange +++ Global dairy trade
# Property sales - China +++ Transport congestion - China
# Property sales - China +++ Investor sentiment

library(dplyr)
library(openxlsx)
library(readxl)

indicators <- list(
  twi = list(
    name = "Trade weighted index",
    file = "COVID-19 - Treasury - Trade weighted index.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 3),
    col_names = c("Date", "TWI")
  ),
  bank_bill = list(
    name = "Bank bill yields",
    file = "COVID-19 - Treasury - Bank bill yields.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 4),
    col_names = c("Date", "Bank.Bill.Yields")
  ),
  foreign_exchange = list(
    name = "Foreign exchange",
    file = "COVID-19 - Treasury - Foreign exchange.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 2),
    col_names = c("Date", "Foreign.Exchange")
  ),
  baltic_dry = list(
    name = "Baltic dry index",
    file = "COVID-19 - Treasury - Baltic dry index.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 20),
    col_names = c("Date", "Baltic.Dry.Index")
  ),
  commodity_prices = list(
    name = "Commodity prices",
    file = "COVID-19 - Treasury - Commodity prices.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 12, 13, 15),
    col_names = c("Date", "Crude.Oil", "Coal", "Copper")
  ),
  market_volatility = list(
    name = "Market volatility index",
    file = "COVID-19 - Treasury - Market volatility index.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 10),
    col_names = c("Date", "Market.Volatility")
  ),
  global_stock = list(
    name = "Global stock markets",
    file = "COVID-19 - Treasury - Global stock markets.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 7, 8, 9),
    col_names = c("Date", "Australia.All.Ordinaries", "China.Security.Index", "S.P.Global")
  ),
  nz_stock = list(
    name = "New Zealand stock exchange",
    file = "COVID-19 - Treasury - NZ stock exchange.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 5),
    col_names = c("Date", "NZ.Stock.Exchange")
  ),
  global_dairy = list(
    name = "Global dairy trade",
    file = "COVID-19 - Treasury - GLobal dairy trade.csv",
    sheet = "Daily",
    skip = 10,
    header = 2,
    cols = c(1, 17),
    col_names = c("Date", "Global.Dairy")
  ),
  property_sales = list(
    name = "Property sales - China",
    file = "COVID-19 - Treasury - Property sales - China.csv",
    sheet = "China daily_UBS",
    skip = 1,
    header = 1,
    cols = c(1, 8:12),
    col_names = c("Date", "2017", "2018", "2019", "2020", "2021")
  ),
  transport_congestion = list(
    name = "Transport congestion - China",
    file = "COVID-19 - Treasury - Transport congestion - China.csv",
    sheet = "China daily_UBS",
    skip = 1,
    header = 1,
    cols = c(1, 15:19)
  ),
  china_purchasing = list(
    name = "China purchasing managers index",
    file = "COVID-19 - Treasury - China purchasing managers index.csv",
    sheet = "Monthly",
    skip = 10,
    header = 2,
    cols = c(2, 7, 8)
  ),
  investor_sentiment = list(
    name = "Investor sentiment",
    file = "COVID-19 - Treasury - Investor sentiment.csv",
    sheet = "Monthly",
    skip = 10,
    header = 2,
    cols = c(2, 9)
  )
)

header_daily <- c(".DESC",
                  "New Zealand: Spot Exchange Rate: United States (US$/NZ$)",
                  "New Zealand: Trade-Weighted Exchange Rate Index 17(Oct-31-14=76.44)",
                  "New Zealand: Bank Bill Yields: 90-Days (%)...4",
                  "New Zealand: NZX All Total Return Index (Jun-30-86=1000)",
                  "Australia: Stock Price Index: All Ordinaries (Jan-1-80=500)...6",
                  "Australia: Stock Price Index: All Ordinaries (Jan-1-80=500)...7",
                  "China: China Security Index: Shanghai-Shenzhen-300 (Dec-31-04=1000)",
                  "S&P Global 1200 Index (Dec-31-97=1000)",
                  "CBOE Market Volatility Index, VIX (Index)",
                  "New Zealand: Bank Bill Yields: 90-Days (%)...11",
                  "Light Sweet Crude Oil Futures: 1st Expiring Contract Settlement (EOP, $/bbl)",
                  "Global Coal RB Index ($/Metric Ton)",
                  "LME Aluminum, 99.7% Purity: Closing Cash Price ($/Metric Tonne)",
                  "LME Copper, Grade A: Closing Cash Price ($/Metric Tonne)",
                  "LME Zinc: Closing Cash Price ($/Metric Tonne)",
                  "New Zealand: GDT Auction: Global Dairy Trade Price Index (Mar-2-10=1000)",
                  "China: Daily Air Quality Index [Unweighted Average] (0=No Pollution)",
                  "China: RMB Exchange Rate: United States (Yuan/100 US$)",
                  "Baltic Exchange Dry Index (Jan-04-85=1000)"
                  )

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
update_filepath <- paste0(directory, "Treasury/", "COVID 19-files requiring Treasury update.xlsx")

#for (ind in indicators) {
ind <- indicators[[1]]
  header <- read_excel(
    path = update_filepath,
    sheet = ind$sheet,
    range = cell_rows(ind$header)
  )
  if(!identical(names(header), header_daily)) stop ("Column order changed in sheet: ", ind$sheet)
  update <- read_excel(
    path = update_filepath,
    sheet = ind$sheet,
    skip = ind$skip,
    col_names = FALSE) %>%
    select(ind$cols)
  df <- read.csv(paste0(directory, ind$file), stringsAsFactors = FALSE)
  df_join <- df %>%
    right_join(update)
#}
