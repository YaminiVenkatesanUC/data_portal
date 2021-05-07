# Staying connected with others +++ Safety travelling to and from essential services +++ COVID-19 related scams

library(tidyverse)
library(openxlsx)

wb <- loadWorkbook("example_data/MOJ pulse survey data - new.xlsx")
sheets <- names(getSheets(wb))


MOJ <- lapply(sheets,
             function(sheet) {
               df <- read.xlsx("example_data/MOJ pulse survey data - new.xlsx",
                               sheetName = sheet,
                               stringsAsFactors = F,
                               colIndex = 1:4)
             })

names(MOJ) <- sheets


for(i in 1:length(sheets)) {
  colnames(MOJ[[i]]) <- MOJ[[i]][1,]
  MOJ[[i]] <- MOJ[[i]] %>% slice(-1)

  MOJ[[i]] <- MOJ[[i]] %>%
    mutate(Percentage = as.numeric(Percentage)) %>%
    pivot_wider(values_from = Percentage, names_from = Response) %>%
    mutate(Date = gsub(" - ", "-", Date)) %>%
    mutate(Date = gsub("-", " - ", Date))
}



safety_vars <- c("safety_home", "safety_neighbourhood", "safety_travelling")
    for(i in safety_vars) {
     MOJ[[i]] <- MOJ[[i]] %>%
       mutate(Measure = .data$`2. Safe` + .data$`1. Very safe`) %>%
       mutate(Measure = round(Measure, 1)) %>%
       select(Date, Measure)

     colnames(MOJ[[i]])[2] <- i
    }


# MOJ$safety_contact <- MOJ$safety_contact %>%
#     mutate(Measure = .data$`1.\tVery easy` + .data$`2.\tEasy`) %>%
#     mutate(Measure = round(Measure, 1)) %>%
#   select(Date, safety_contact = Measure)
#

  MOJ$communication <- MOJ$communication %>%
    mutate(Measure = .data$`Hard` + .data$`Very hard`) %>%
    mutate(Measure = round(Measure, 1) ) %>%
    select(Date, communication = Measure)

  MOJ$crime <- MOJ$crime %>%
    select(Date, `COVID-19 related scam`, Cybercrime) %>%
    mutate(across(where(is.numeric), ~round(.x, 1)))


# MOJ$safety <- left_join(MOJ$safety_home, MOJ$safety_neighbourhood, by = "Date")
# MOJ$safety <-  left_join(MOJ$safety, MOJ$safety_travelling, by = "Date")



MOJ_output <- list(MOJ$communication,
                   # MOJ$safety,
                   # MOJ$safety_home,
                   # MOJ$safety_neighbourhood,
                   MOJ$safety_travelling,
                   #  MOJ$safety_contact,
                   MOJ$crime
                   )
names(MOJ_output) <- c("communication",
                       # "safety",
                       # "safety_home",
                       # "safety_neighbourhood",
                       "safety_travelling",
                       # "safety_contact",
                       "crime")

if (file.exists("example_data/MOJ_pulse_survey_output.xlsx")) file.remove("example_data/MOJ_pulse_survey_output.xlsx")
OUT <- createWorkbook()

  for (i in 1:length(MOJ_output)) {
    addWorksheet(OUT, names(MOJ_output)[i])
    writeData(OUT, sheet = names(MOJ_output)[i], x = MOJ_output[[i]])

    #this function is misbehaving... (append not working properly)
    # write.xlsx(MOJ_output[[i]],
    #            file = "example_data/MOJ_pulse_survey_output.xlsx",
    #            sheetName = names(MOJ_output)[i],
    #            append = T,
    #            showNA = F)
  }
saveWorkbook(OUT, "example_data/MOJ_pulse_survey_output.xlsx")
