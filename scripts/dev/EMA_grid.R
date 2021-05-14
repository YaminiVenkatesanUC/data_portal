#Electricity grid demand +++ Electricity grid demand by region

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "EMA grid/")

node <- as.data.frame(read.csv(file = paste0(path, "Grid_demand_trends_Node.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE))
codes <- read_excel(path = paste0(path, "codes.xlsx"), .name_repair = "universal")

# NZ / ISLAND / ZONE
# # !!!! transform the master only once in master branch !!! --------------------------------------------------
# old_zones <- read_excel(path = paste0(directory, "COVID 19 - EMA Grid demand.xlsx"),
#                            sheet = 2, .name_repair = "universal", skip = 1)
# names(old_zones) <- str_remove(names(old_zones), ".demand.*")
#
# file.rename(from = paste0(directory, "COVID 19 - EMA Grid demand.xlsx"),
#             to = paste0(directory, "Previous/COVID 19 - EMA Grid demand.xlsx"))
# write.csv(x = old_zones, file = paste0(directory, "COVID 19 - EMA Grid demand.csv"), row.names = FALSE)

# append update to master_region-----------------------------------------------------------------------

island <- read.csv(file = paste0(path, "Grid_demand_trends_Island.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE) %>%
  select(Period.start, Region, Demand..GWh.) %>%
  pivot_wider(names_from = Region, values_from = Demand..GWh., names_repair = "universal")
nz <- read.csv(file = paste0(path, "Grid_demand_trends_New Zealand.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE)%>%
  select(Period.start, Region, Demand..GWh.)%>%
  pivot_wider(names_from = Region, values_from = Demand..GWh., names_repair = "universal")
zone <- read.csv(file = paste0(path, "Grid_demand_trends_Zone.csv"), skip = 11, header = TRUE, stringsAsFactors = FALSE)%>%
  select(Period.start, Region, Demand..GWh.)%>%
  pivot_wider(names_from = Region, values_from = Demand..GWh., names_repair = "universal")

update_zones <- island %>%
  left_join(nz) %>%
  left_join(zone)
names(update_zones) <- str_replace(names(update_zones), "New.Zealand", "Total")
update_zones$Period.start <- dmy(update_zones$Period.start)

master_zones <- read.csv(file = paste0(directory, "COVID 19 - EMA Grid demand.csv"), stringsAsFactors = FALSE)
master_zones$Period.start <- ymd(master_zones$Period.start)
master_zones <- master_zones %>%
  filter(!(Period.start %in% update_zones$Period.start)) %>%
  rbind(update_zones)

file.rename(from = paste0(directory, "COVID 19 - EMA Grid demand.csv"),
            to = paste0(directory, "Previous/COVID 19 - EMA Grid demand.csv"))
write.csv(x = master_zones, file = paste0(directory, "COVID 19 - EMA Grid demand.csv"), row.names = FALSE)



# BY REGION ---------------------------------------

# # !!!! transform the master only once in master branch !!!
# old_region <- read_excel(path = paste0(directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"), sheet = 1, .name_repair = "universal")
#
# old_region <- old_region %>%
#   left_join(codes, by = "Region.ID") %>%
#   drop_na() %>%
#   mutate(Period.start = ymd(Period.start)) %>%
#   group_by(Regional.Council, Period.start) %>%
#   summarise_if(is.numeric, sum, na.rm = TRUE) %>%
#   spread(Regional.Council, Demand..GWh.)
#
#
# file.rename(from = paste0(directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"),
#             to = paste0(directory, "Previous/COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"))
# write.csv(x = old_region, file = paste0(directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), row.names = FALSE)

# appned update to master_region-----------------------------------------------------------------------
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

master_region <- read.csv(file = paste0(directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), stringsAsFactors = FALSE)
master_region$Period.start <- ymd(master_region$Period.start)
master_region <- master_region %>%
  filter(!(Period.start %in% by_region$Period.start)) %>%
  rbind(by_region)

file.rename(from = paste0(directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"),
            to = paste0(directory, "Previous/COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"))
write.csv(x = master_region, file = paste0(directory, "COVID 19 - EMA Grid demand by Region ID by Regional Council.csv"), row.names = FALSE)
