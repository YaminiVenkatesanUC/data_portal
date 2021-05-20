# Trade weighted index +++ Bank bill yields +++ Foreign exchange

library(dplyr)
library(openxlsx)
library(readxl)

indicators <- list(
  twi = list(
    name = "Trade weighted index",
    file = "COVID-19 - Treasury - Trade weighted index.csv",
    update = "hb1-daily.xlsx",
    sheet = "Data",
    skip = 5,
    header = 1,
    cols = c(1, 2),
    col_names = c("Date", "TWI")
  ),
  bank_bill = list(
    name = "Bank bill yields",
    file = "COVID-19 - Treasury - Bank bill yields.csv",
    update = "hb2-daily.xlsx",
    sheet = "Data",
    skip = 5,
    header = 1,
    cols = c(1, 6),
    col_names = c("Date", "Bank.Bill.Yields")
  ),
  foreign_exchange = list(
    name = "Foreign exchange",
    file = "COVID-19 - Treasury - Foreign exchange.csv",
    update = "hb1-daily.xlsx",
    sheet = "Data",
    skip = 5,
    header = 1,
    cols = c(1, 3),
    col_names = c("Date", "Foreign.Exchange")
  )
)

headers <- list(
  `hb1-daily.xlsx`= c("TWI",
                      "Exchange rates (quoted per NZ$)...2",
                      "Exchange rates (quoted per NZ$)...3",
                      "Exchange rates (quoted per NZ$)...4",
                      "Exchange rates (quoted per NZ$)...5",
                      "Exchange rates (quoted per NZ$)...6",
                      "Exchange rates (quoted per NZ$)...7",
                      "Exchange rates (quoted per NZ$)...8",
                      "Exchange rates (quoted per NZ$)...9",
                      "Exchange rates (quoted per NZ$)...10",
                      "Exchange rates (quoted per NZ$)...11",
                      "Exchange rates (quoted per NZ$)...12",
                      "Exchange rates (quoted per NZ$)...13",
                      "Exchange rates (quoted per NZ$)...14",
                      "Exchange rates (quoted per NZ$)...15",
                      "Exchange rates (quoted per NZ$)...16",
                      "Exchange rates (quoted per NZ$)...17",
                      "Exchange rates (quoted per NZ$)...18",
                      "Historical TWI"),
  `hb2-daily.xlsx` = c("Cash rate...1",
                       "Cash rate...2",
                       "Bank bill yields...3",
                       "Bank bill yields...4",
                       "Bank bill yields...5",
                       "Secondary market government bond yields...6",
                       "Secondary market government bond yields...7",
                       "Secondary market government bond yields...8",
                       "Secondary market government bond yields...9",
                       "Secondary market government bond yields...10",
                       "Secondary market government bond yields...11",
                       "Secondary market government bond yields...12",
                       "Secondary market government bond yields...13",
                       "Secondary market government bond yields...14",
                       "Secondary market government bond yields...15",
                       "Secondary market government bond yields...16",
                       "Secondary market government bond yields...17",
                       "Secondary market government bond yields...18",
                       "Secondary market government bond yields...19",
                       "Secondary market government bond yields...20",
                       "Secondary market government bond yields...21",
                       "Secondary market government bond yields...22",
                       "Secondary market government bond yields...23",
                       "Inflation indexed bond...24",
                       "Inflation indexed bond...25",
                       "Inflation indexed bond...26",
                       "Inflation indexed bond...27"))


directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"

for (ind in indicators) {
  header <- read_excel(
    path = paste0(directory, "daily financial/", ind$update),
    sheet = ind$sheet,
    range = cell_rows(ind$header)
  )
  if(!identical(names(header), headers[[ind$update]])) stop ("Column order changed in sheet: ", ind$sheet)
  update <- read_excel(
    path = paste0(directory, "daily financial/", ind$update),
    sheet = ind$sheet,
    skip = ind$skip,
    col_names = FALSE) %>%
    select(ind$cols)
  names(update) <- ind$col_names
  update$Date <- as.character(as.Date(update$Date), format = "%d/%b/%y")

  file.rename(from = paste0(directory, ind$file),
              to = paste0(directory, "Previous/", ind$file))
  write.csv(update, paste0(directory, ind$file), row.names = FALSE)

}
