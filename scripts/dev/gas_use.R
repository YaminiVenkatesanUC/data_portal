# Gas consumption by largest users +++ Gas consumption by selected major users

library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

#COVID-19 First Gas by_selected_major_users.xlsx

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/_TEST/"
path <- paste0(directory, "First Gas/")
files <- file.info(list.files(path, full.names = T, pattern = "DDR[0-9].*"))


#master_path <- rownames(file.info(list.files(path, full.names = T, pattern = "master.*")))



load_parameters <- list(
  by_selected_major_users = list(
    #sheet = "Ex Vector pipe by key user gate",
    #gas_source = "Vector",
    categories = list(
       Fonterra = list(
          source = "First Gas",
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
                meters = c("9931003")
             ),
             tirau_df = list(
                id = "TIR33501",
                meters = c("33501"))
         )),
       Ballance = list(
          source = "First Gas",
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
          source = "First Gas",
          points = list(
             glebrook = list(
                id = "GLB03401",
                meters = c("3401", "3402"))
       )),
       Kinleith = list(
          source = "First Gas",
          points = list(
             kinleith_chh = list(
                id = "KIN04310",
                meters = c("4301", "4302"))
       )),
       Marsden = list(
          source = "First Gas",
          points = list(
             marsden_1 = list(
                id = "MSD01801",
                meters = c("1801", "1803"))
       )
    )
  )),
  by_largest_users = list(
    sheet = "Maui pipeline key user gates",
    gas_source = "Maui",
    categories = list(
       Methanex_Waitara = list(
          source = "Maui",
          points = list(
             bertrand = list(
                id = "4000564",
                meters = c()
             ))
       ),
       Methanex_Motunui = list(
          source = "Maui",
          points = list(
             faull = list(
                id = "4000653",
                meters = c()
             ),
             ngatimaru = list(
                id = "4000669",
                meters = c())
          )
       ),
       Huntly = list(
          source = "Maui",
          points = list(
             huntly = list(
                id = "4002993",
                meters = c()
             )
          )
       ),
       Stratford = list(
          source = "First Gas",
          points = list(
             stratford_2 = list(
                id = "STR00521",
                meters = c("521", "522"))
          )),
       Taranaki = list(
          source = "First Gas",
          points = list(
             tcc = list(
                id = "TCC00201",
                meters = c("201", "202"))
         )),
       Te_Rapa = list(
          source = "First Gas",
          points = list(
             te_rapa_cogen = list(
                id = "TRC02003",
                meters = c("2003", "2004"))
          )
       )
    )
  )
)




for (ind in 1:length(load_parameters)) {
#ind <- 1
indicator <- load_parameters[[ind]]
OUT <- createWorkbook()
for (cat in 1:length(indicator$categories)) {
#for (cat in 1) {
   category <- indicator$categories[[cat]]
   master_path <- paste0(directory, "COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx")
   master <- read_excel(master_path, sheet = names(indicator$categories)[[cat]])
   master$Energy <- round(master$Energy, 2)

   for (p in 1:length(category$points)) {
   #for (p in 1) {
      point <- category$points[[p]]

      if (length(point$meters) > 0) {
         df_total <- data.frame(matrix(nrow = 0, ncol = 2))
         names(df_total) <- c("Date", "Energy")
         for (m in 1:length(point$meters)) {
            meter <- point$meters[[m]]
            filepath <- rownames(files)[str_detect(rownames(files), meter)]
            df <- read.csv(filepath, skip = 9, header = FALSE)
            df <- df %>% select(1, ncol(df))
            names(df) <- c("Date", "Energy")
            df <- df %>%
               filter(Date != "Totals")
            df$Date <- dmy(df$Date)
            df$Energy <- round(as.numeric(df$Energy), 2)

            df_total <- rbind(df_total, df)
         }
         df_total <- df_total %>%
            group_by(Date) %>%
            summarise(Energy = sum(Energy))
         master <- rbind(master, df_total)
      } else {
         filepath_maui <- rownames(file.info(list.files(path, full.names = T, pattern = "Maui.*")))
         df <- read.xlsx(xlsxFile = filepath_maui, startRow = 7, fillMergedCells = TRUE)
      }

   }
   master <- master %>% unique()
   addWorksheet(wb = OUT, sheetName = names(indicator$categories)[[cat]])
   writeData(OUT, sheet = names(indicator$categories)[[cat]], x = master)
}

file.rename(from = paste0(directory, "COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx"),
            to = paste0(directory, "Previous/COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx"))
saveWorkbook(wb = OUT, file = paste0(directory, "COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx"))



}





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

   colnames(data) <- c("indicatoreter", paste0("col_", 2:ncol(data)))
   return(data_frame_to_data_object_helper(
     directory,
     config,
     data
   ))
 }
