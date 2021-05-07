library(tidyverse)
library(openxlsx)

# read the data

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"

moh_data <- # read.xlsx("example_data/Results to week 18.xlsx", sheetIndex = 1, stringsAsFactors = F)
            read.xlsx("~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/Portal Data Supply/MoH/Results to week 23.xlsx", sheetIndex = 1, stringsAsFactors = F)

# reformat date
# reformat as percentage
# combine variable label & name into one
# pivot so each series (including error series) is its own column

vars <- c("q22_mod",
         "q53_mod",
         "Q76_mod",
         "GAD2_OR_PHQ2",
         "q21_mod",
         "Q82_mod",
         "Int_Calm01",
         "Int_Nervous01",
         "Int_STLHome01",
         "Int_WFH01")
labels <- c("Health status",
           "Sleep quantity",
           "Life satisfaction – MoH",
           "Depression and Anxiety",
           "Loneliness – past 7 days",
           "Overall wellbeing",
           "Calmness",
           "Nervousness",
           "Stress about leaving home",
           "Worry about family health")

moh_data <-
  moh_data %>%
  mutate(Week.ending = gsub("Week ending", "", Week.ending)) %>% # variable name changed between supplies...
  mutate(Week.ending = paste0(Week.ending, " 2020")) %>%
  mutate(Week.ending = parse_date(Week.ending, "%d %B %Y"))%>%
  #mutate(Var = paste(VarLabel, VarName, sep = "_")) %>%
  mutate(across(where(is.numeric), ~(.x * 100))) %>%
  mutate(across(where(is.numeric), ~round(.x, 1))) %>% # didn't work when combined in earlier mutate call
  select(Week_ending = Week.ending,
         VarName,
         Mean,
         LowerCLMean,
         UpperCLMean
         )

for (i in 1:length(vars)) {
  df <- moh_data %>%
    filter(VarName == vars[i]) %>%
    select(-VarName)

  file.rename(from = paste0(directory, "COVID-19 MoH - ", labels[i], ".xlsx"),
              to = paste0(directory, "Previous/COVID-19 MoH - ", labels[i], ".xlsx"))
  write.xlsx(df, paste0(directory, "COVID-19 MoH - ", labels[i], ".xlsx"), row.names = F)
}

