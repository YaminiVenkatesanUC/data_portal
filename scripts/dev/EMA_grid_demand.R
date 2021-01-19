
fpath <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/Raw data"
grid_demand_all_new <- lapply(list.files(fpath, pattern = "Grid_demand_trends", full.names = TRUE), read.csv,  skip = 11) %>% bind_rows()
grid_demand <- read_xlsx(paste0(fpath,"/Development/COVID 19 - EMA Grid demand.xlsx"),
                         skip=1,
                         sheet="Grid demand NI SI")
grid_demand_by_node  <- read_xlsx(paste0(fpath,"/Development/COVID 19 - EMA Grid demand by Region ID by Regional Council.xlsx"),
                                  sheet="Grid demand from 010215")

EMA_grid_demand <- function(grid_demand_all_new, grid_demand){
  grid_demand_new <- grid_demand_all_new %>%
                    select("Period.start", "Region", "Demand..GWh.") %>%
                    filter(Region %in%
                             c("New Zealand",
                               "North Island",
                                "South Island",
                                "Upper North Island",
                                "Central North Island",
                                "Lower North Island",
                                "Upper South Island",
                                "Lower South Island")) %>%
                    spread(key = unique(Region), value = Demand..GWh.) %>%
                    rename("Period start" = "Period.start",
                           "North Island demand (GWh)" = "North Island",
                           "South Island demand (GWh)" = "South Island",
                           "Total demand (GWh)" = "New Zealand")

  grid_demand_new$`Period start` <- as.Date(grid_demand_new$`Period start`)

  updates <- anti_join(x=grid_demand_new,y=grid_demand,by=c("Period start","Total demand (GWh)",
                                                          "North Island demand (GWh)",
                                                          "South Island demand (GWh)",
                                                          "Central North Island",
                                                          "Lower North Island",
                                                          "Lower South Island",
                                                          "Upper North Island",
                                                          "Upper South Island"))
  no_updates <- anti_join(x=grid_demand, y=updates, by="Period start")
  grid_demand_updated <- full_join(x=updates,y=no_updates, by = intersect(names(updates), names(no_updates)))

}

EMA_grid_demand_nodes <- function(grid_demand_all_new, grid_demand_by_node){
  grid_demand_new <- grid_demand_all_new %>%
    select("Period.start", "Region", "Region.ID", "Demand..GWh.") %>%
    filter( !(Region %in%
             c("New Zealand",
               "North Island",
               "South Island",
               "Upper North Island",
               "Central North Island",
               "Lower North Island",
               "Upper South Island",
               "Lower South Island")))  %>%
    rename("Period start" = "Period.start",
           "Region ID" = "Region.ID",
           "Demand (GWh)" = "Demand..GWh.")

  grid_demand_new$`Period start` <- as.Date(grid_demand_new$`Period start`)

  updates <- anti_join(x=grid_demand_new,y=grid_demand_by_node,by=c("Period start",
                                                                    "Region",
                                                                    "Region ID",
                                                                    "Demand (GWh)"))
  no_updates <- anti_join(x=grid_demand_by_node, y=updates, by=c("Period start","Region ID"))
  grid_demand_updated <- full_join(x=updates,y=no_updates, by = intersect(names(updates), names(no_updates)))

}
