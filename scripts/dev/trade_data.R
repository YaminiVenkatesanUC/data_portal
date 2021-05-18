# Exports by country (values) +++ Imports by country (values) +++ Imports by commodity (values) +++ Exports by commodity (values)


library(openxlsx)

#config <- read_config_file()
directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "Trade Data")
files <- file.info(list.files(path, full.names = T, pattern = ".*\\.csv"))
update <- read.csv(rownames(files)[which.max(files$ctime)])

col_names <- c("Direction", "Year", "Date", "Weekday", "Country", "Commodity", "Transport_Mode", "Measure", "Value", "Cumulative")
if (length(setdiff(names(update), col_names)) > 0) stop ("New column added: ", setdiff(names(update), col_names))

load_parameters <- list(
  exports_by_country = list(
    country = c("All" ,"China", "Australia", "United States", "Japan", "United Kingdom", "European Union (27)", "East Asia (excluding China)", "Total (excluding China)"),
    transport_mode = "All",
    commodity = "All",
    measure = "$",
    direction = "Exports",
    group_col ="Country"
  ),
  imports_by_country = list(
    country = c("All" ,"China", "Australia", "United Kingdom"),
    transport_mode = "All",
    commodity = "All",
    measure = "$",
    direction = "Imports",
    group_col = "Country"
  ),
  imports_by_commodity = list(
    country = "All",
    transport_mode = "All",
    commodity = c("All", "Non-food manufactured goods", "Mechanical machinery and equip", "Electrical machinery and equip"),
    measure = "$",
    direction = "Imports",
    group_col = "Commodity"
  ),
  exports_by_commodity = list(
    country = "All",
    transport_mode = "All",
    commodity = c("All", "Milk powder, butter, and cheese", "Meat and edible offal", "Fish, crustaceans, and molluscs", "Logs, wood, and wood articles", "Fruit", "Non-food manufactured goods"),
    measure = "$",
    direction = "Exports",
    group_col = "Commodity"
  )
)

for (ind in 1:length(names(load_parameters))) {parameter <- load_parameters[[ind]]
data <- update %>%
  filter(
    Country %in% parameter$country &
      Commodity %in% parameter$commodity &
      Measure %in% parameter$measure &
      Direction %in% parameter$direction &
      Transport_Mode %in% parameter$transport_mode
  )
data$Date <- dmy(data$Date)
data <- data %>% arrange(Date)
data <- data %>%
  mutate(
    Parameter = format(data$Date, "%d-%b")
  ) %>%
  select("Parameter", "Year", parameter$group_col, "Cumulative")

file.rename(from = paste0(path, "/COVID 19 - Trade Data - ", names(load_parameters)[[ind]], ".xlsx"),
            to = paste0(path, "/Previous/COVID 19 - Trade Data - ", names(load_parameters)[[ind]], ".xlsx"))

if (length(setdiff(unique(data[[3]]), parameter[[tolower(parameter$group_col)]])) > 0)
  stop ("New categories added: ", setdiff(unique(data[[3]]), parameter[[tolower(group_col)]]))
OUT <- createWorkbook()
for (group in unique(data[[3]])) {
  data_group <- data %>% filter(data[[3]] == group) %>%
    select(-3)

  data_group <- data_group %>%
    tibble::add_row(Parameter = "29-Feb", Year = 2015, Cumulative = NA, .before = 60)

  output <- data_group %>%
    pivot_wider(names_from = Year, values_from = c("Cumulative")) %>%
    as.data.frame()

  addWorksheet(OUT, group)
  writeData(OUT, sheet = group, x = output)

  # #append in this funciton is no longer behaving properly....
  # write.xlsx(x = output, paste0(path, "/COVID 19 - Trade Data - ", names(load_parameters)[[ind]], ".xlsx"),
  #            sheetName = group,
  #            append = TRUE,
  #            row.names = FALSE)
}
saveWorkbook(OUT, paste0(path, "/COVID 19 - Trade Data - ", names(load_parameters)[[ind]], ".xlsx"))}

