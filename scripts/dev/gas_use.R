# Gas consumption by largest users +++ Gas consumption by selected major users

library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/_TEST/"
path <- paste0(directory, "First Gas/")
files <- file.info(list.files(path, full.names = T, pattern = "DDR[0-9].*"))

load_parameters <- list(
  by_selected_major_users = list(
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
    categories = list(
       Methanex_Waitara = list(
          source = "Maui",
          points = list(
             bertrand = list(
                id = "4000564",
                cols = c(1, 12)
             ))
       ),
       Methanex_Motunui = list(
          source = "Maui",
          points = list(
             faull = list(
                id = "4000653",
                cols = c(1, 14)
             ),
             ngatimaru = list(
                id = "4000669",
                cols = c(1, 22))
          )
       ),
       Huntly = list(
          source = "Maui",
          points = list(
             huntly = list(
                id = "4002993",
                cols = c(1, 40)
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


filepath_maui <- rownames(file.info(list.files(path, full.names = T, pattern = "Maui.*")))
header <- read.xlsx(xlsxFile = filepath_maui,
                    rows = c(7),
                    skipEmptyCols = TRUE,
                    skipEmptyRows = TRUE,
                    fillMergedCells = TRUE)
header_check <- c("Oaonui.4000000", "Frankley.Road.4000439", "Mangorei.4000485", "New.Plymouth.Power.Station.4000530", "Bertrand.Road.(Waitara.Valley).4000564",
                  "Faull.Road.4000653", "Kowhai.Mixing.Station.4000666", "Tikorangi.#2.4000667", "Tikorangi.4000668", "Ngatimaru.Rd.(Delivery).4000669",
                  "Ngatimaru.Rd.(Receipt).4000670", "Tikorangi.#3.(Receipt).4000702", "Tikorangi.#3.(Delivery).4000703", "Turangi.Mixing.Station.4000710",
                  "Mokau.Compressor.Station.4001143", "Pokuru.4002308", "Pirongia.4002374", "Rotowaro.4002906", "Huntly.Power.Station.4002993",
                  "NZX.(Delivery).THD", "NZX.(Receipt).THR", "TRS.(Delivery).TRSD", "TRS.(Receipt).TRSR", "Balancing.Gas.(Receipt).BGR")

if (!identical(names(header), header_check)) stop ("Columns in Maui spreadsheet changed.")

for (ind in 1:length(load_parameters)) {
indicator <- load_parameters[[ind]]
OUT <- createWorkbook()
for (cat in 1:length(indicator$categories)) {
   category <- indicator$categories[[cat]]
   master_path <- paste0(directory, "COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx")
   master <- read_excel(master_path, sheet = names(indicator$categories)[[cat]])
   master$Energy <- round(master$Energy, 2)

   for (p in 1:length(category$points)) {
      point <- category$points[[p]]

      if (category$source == "First Gas") {
         df_total <- data.frame(matrix(nrow = 0, ncol = 2))
         names(df_total) <- c("Date", "Energy")
         for (m in 1:length(point$meters)) {
            meter <- point$meters[[m]]
            filepath <- rownames(files)[str_detect(rownames(files), paste0("DDR", meter))]
            df <- read.csv(filepath, skip = 9, header = FALSE)
            df <- df %>% select(1, ncol(df))
            names(df) <- c("Date", "Energy")
            df <- df %>%
               filter(Date != "Totals")
            df$Date <- dmy(df$Date)
            df$Energy <- round(as.numeric(df$Energy), 2)

            df_total <- rbind(df_total, df)
         }

      } else {
         df_total <- read.xlsx(xlsxFile = filepath_maui,
                         startRow = 10,
                         skipEmptyRows = TRUE,
                         skipEmptyCols = TRUE,
                         fillMergedCells = TRUE)
         df_total <- df_total %>% select(point$cols)
         names(df_total) <- c("Date", "Energy")
         df_total$Date <- dmy(df_total$Date)
         df_total$Energy <- round(as.numeric(df_total$Energy), 2)
         df_total <- drop_na(df_total)
      }

      master <- rbind(master, df_total) %>%
         group_by(Date) %>%
         summarise(Energy = sum(Energy))
   }
   master <- master %>%
      arrange(Date) %>%
      unique()
   addWorksheet(wb = OUT, sheetName = names(indicator$categories)[[cat]])
   writeData(OUT, sheet = names(indicator$categories)[[cat]], x = master)
}

file.rename(from = paste0(directory, "COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx"),
            to = paste0(directory, "Previous/COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx"))
saveWorkbook(wb = OUT, file = paste0(directory, "COVID-19 First Gas ", names(load_parameters)[[ind]], ".xlsx"))
}
