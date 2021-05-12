# Gas consumption by largest users +++ Gas consumption by selected major users

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/_TEST/"
path <- paste0(directory, "First Gas/")
#files <- file.info(list.files(path, full.names = T, pattern = "DDR[0-9].*"))
master_path <- file.info(list.files(path, full.names = T, pattern = "master.*"))



load_parameters <- list(
  by_selected_major_users = list(
    sheet = "Ex Vector pipe by key user gate",
    gas_source = "Vector",
    categories = list(
       Fonterra = list(
          points = list(
             edgecumbe_df = list(
                id = "EGC30701",
                meters = c("30701", "30703")
             ),
             kauri_df = list(
                id = "KUR33601",
                meters = c("33601")
             ),
             lichfield_df = list(
                id = "LCF20010",
                meters = c("20001")
             ),
             lichfield_2 = list(
                id = "LCF20011",
                meters = c("20021", "20022")
             ),
             morrinsville_df = list(
                id = "MRV16301",
                meters = c("16301")
             ),
             maungaturoto_df = list(
                id = "MUT19001",
                meters = c("19001")
             ),
             pahiatua_df = list(
                id = "PHT04902",
                meters = c("4921")
             ),
             te_awamutu_df = list(
                id = "TAC31001",
                meters = c("9931001")
             ),
             tirau_df = list(
                id = "TIR33501",
                meters = c("33501")
             )
         )
       ),
       Ballance = list(
          points = list(
             ballance_8201 = list(
                id = "BAL08201",
                meters = c("8201")
             ),
             ballance_9626 = list(
                id = "BAL09626",
                meters = c("9626"))
       )),
       Glenbrook = list(
          points = list(
             glebrook = list(
                id = "GLB03401",
                meters = c("3401", "3402"))
       ),
       Kinleith = list(
          points = list(
             kinleith_chh = list(
                id = "KIN04310",
                meters = c("4301", "4302"))
       ),
       Marsden = list(
          points = list(
             "MSD01801")
       )
    )
  ),
  by_largest_users = list(
    sheet = "Maui pipeline key user gates",
    gas_source = "Maui"
  )
)


#for (ind in 1:length(load_parameters)) {
 ind <- 1
 param <- load_parameters[[ind]]

 if (param$gas_source == "Vector") {

   master_df <- read_excel(rownames(master_path), sheet = param$sheet, skip = 1) %>%
     select(-`Source: First Gas`)

   write_xlsx(x = master_df, path = paste0(path, "lala.xlsx"))

   updates <- file.info(list.files(path, full.names = T, pattern = "DDR[0-9].*"))


   data <- data %>%
     select(Date = `...1`,  everything()) %>%
     mutate(
       Date = dmy(Date),
       `Ballance Agri- Nutrients` = `Ballance Agri-Nutrients...2` +
         `Ballance Agri-Nutrients...3`,
       Fonterra = `Subtotal Fonterra...6` +
         `Subtotal Fonterra...7` +
         `Subtotal Fonterra...8` +
         `Subtotal Fonterra...9` +
         `Subtotal Fonterra...10` +
         `Subtotal Fonterra...11` +
         `Subtotal Fonterra...12` +
         `Subtotal Fonterra...13` +
         `Subtotal Fonterra...14`
     ) %>%
     select(
       Date,
       Fonterra,
       `Ballance Agri- Nutrients`,
       `Glenbrook steel mill`,
       `Kinleith pulp and paper mill`,
       `Marsden Point oil refinery`
     )
 }

 if (config$gas_source == "Maui") {
   data <- data %>%
     mutate(Date = dmy(`...1`), Methanex = `...4` + `Methanex Motunui`) %>%
     select(Date, everything(), -`...1`, -`...4`, -`Methanex Motunui`)
 }


#}





 gas_use_data <- function(config, directory) {
   data <- as.data.frame(read_excel(
     paste0(directory, config$filename),
     sheet = config$sheet_number,
     skip = config$skip
   )) %>%
     select(-`Source: First Gas`)

   if (config$gas_source == "Vector") {
     data <- data %>%
       select(Date = `...1`,  everything()) %>%
       mutate(
         Date = dmy(Date),
         `Ballance Agri- Nutrients` = `Ballance Agri-Nutrients...2` +
           `Ballance Agri-Nutrients...3`,
         Fonterra = `Subtotal Fonterra...6` +
           `Subtotal Fonterra...7` +
           `Subtotal Fonterra...8` +
           `Subtotal Fonterra...9` +
           `Subtotal Fonterra...10` +
           `Subtotal Fonterra...11` +
           `Subtotal Fonterra...12` +
           `Subtotal Fonterra...13` +
           `Subtotal Fonterra...14`
       ) %>%
       select(
         Date,
         Fonterra,
         `Ballance Agri- Nutrients`,
         `Glenbrook steel mill`,
         `Kinleith pulp and paper mill`,
         `Marsden Point oil refinery`
       )
   }

   if (config$gas_source == "Maui") {
     data <- data %>%
       mutate(Date = dmy(`...1`), Methanex = `...4` + `Methanex Motunui`) %>%
       select(Date, everything(), -`...1`, -`...4`, -`Methanex Motunui`)
   }

   colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))
   return(data_frame_to_data_object_helper(
     directory,
     config,
     data
   ))
 }
