config <- read_config_file()
path <- paste0(config$data_directory, "EMA grid/")
#files <- file.info(list.files(path, full.names = T, pattern = "Grid_demand_trends.*"))
#update <- read.csv(rownames(files)[which.max(files$ctime)])


island <- read.csv(file = paste0(path, "Grid_demand_trends_Island.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE)
nz <- read.csv(file = paste0(path, "Grid_demand_trends_New Zealand.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE)
zone <- read.csv(file = paste0(path, "Grid_demand_trends_Zone.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE)
node <- as.data.frame(read.csv(file = paste0(path, "Grid_demand_trends_Node.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE))
codes <- read_excel(path = paste0(path, "codes.xlsx"), .name_repair = "universal")

# NZ / ISLAND / ZONE
# !!!! transform the master only once in master branch !!! --------------------------------------------------
master_zones <- read_excel(path = paste0(config$data_directory, "COVID 19 - EMA Grid demand.xlsx"), sheet = 1, .name_repair = "universal")

master_zones <- master_zones %>%
  left_join(codes, by = "Region.ID") %>%
  drop_na() %>%
  mutate(Period.start = ymd(Period.start)) %>%
  group_by(Regional.Council, Period.start) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  spread(Regional.Council, Demand..GWh.)


file.rename(from = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"),
            to = paste0(config$data_directory, "Previous/COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"))
write.csv(x = master_region, file = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), row.names = FALSE)

# appned update to master_region-----------------------------------------------------------------------
master_region <- read.csv(file = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), stringsAsFactors = FALSE)
master_region$Period.start <- ymd(master_region$Period.start)
master_region <- master_region %>%
  filter(!(Period.start %in% by_region$Period.start)) %>%
  rbind(by_region)

file.rename(from = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"),
            to = paste0(config$data_directory, "Previous/COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"))
write.csv(x = master_region, file = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), row.names = FALSE)



# BY REGION ---------------------------------------

by_region <- node %>%
  select(-Trading.period) %>%
  left_join(codes, by = "Region.ID") %>%
  drop_na() %>%
  mutate(Period.start = dmy(Period.start)) %>%
  group_by(Regional.Council, Period.start) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

value_names <- c("Auckland",
                 "Bay of Plenty",
                 "Canterbury",
                 "Gisborne",
                 "Hawke's Bay",
                 "Manawatū-Whanganui",
                 "Marlborough",
                 "Northland",
                 "Otago",
                 "Southland",
                 "Taranaki",
                 "Tasman/Nelson",
                 "Waikato",
                 "Wellington",
                 "West Coast")

if (!identical(unique(by_region$Regional.Council), value_names)) stop ("Order in regions changed")

by_region <- by_region %>%
  pivot_wider(names_from = Regional.Council, values_from = Demand..GWh., names_repair = "universal")


# !!!! transform the master only once in master branch !!! --------------------------------------------------
master_region <- read_excel(path = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"), sheet = 1, .name_repair = "universal")

master_region <- master_region %>%
  left_join(codes, by = "Region.ID") %>%
  drop_na() %>%
  mutate(Period.start = ymd(Period.start)) %>%
  group_by(Regional.Council, Period.start) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  spread(Regional.Council, Demand..GWh.)


file.rename(from = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"),
            to = paste0(config$data_directory, "Previous/COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"))
write.csv(x = master_region, file = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), row.names = FALSE)

# appned update to master_region-----------------------------------------------------------------------
master_region <- read.csv(file = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), stringsAsFactors = FALSE)
master_region$Period.start <- ymd(master_region$Period.start)
master_region <- master_region %>%
  filter(!(Period.start %in% by_region$Period.start)) %>%
  rbind(by_region)

file.rename(from = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"),
            to = paste0(config$data_directory, "Previous/COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"))
write.csv(x = master_region, file = paste0(config$data_directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), row.names = FALSE)
