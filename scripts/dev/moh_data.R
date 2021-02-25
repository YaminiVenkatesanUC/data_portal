library(tidyverse)
library(xlsx)

# read the data

moh_data <- # read.xlsx("example_data/Results to week 18.xlsx", sheetIndex = 1, stringsAsFactors = F)
            read.xlsx("example_data/Weighted data rounded - for Stats.xlsx", sheetIndex = 1, stringsAsFactors = F)

# reformat date
# reformat as percentage
# combine variable label & name into one 
# pivot so each series (including error series) is its own column


moh_data <-
  moh_data %>%
  mutate(Week.ending = gsub("Week ending", "", Date)) %>% # variable name changed between supplies...
  mutate(Week.ending = paste0(Week.ending, " 2020")) %>%
  mutate(Week.ending = parse_date(Week.ending, "%d %B %Y")) %>%
  mutate(Var = paste(VarLabel, VarName, sep = "_")) %>%
  mutate(across(where(is.numeric), ~(.x * 100))) %>%
  mutate(across(where(is.numeric), ~round(.x, 1))) %>% # didn't work when combined in earlier mutate call 
  select(Week_ending = Week.ending,
         Var,
         Mean,
         Lower = LowerCLMean,
         Upper = UpperCLMean
         ) 

# save the variable names we'll need to select the columns in the right order

names <- unique(moh_data$Var)
names <- rep(names, each = 3)
names <- paste0(c("Mean", "Lower", "Upper"), "_", names)

# pivot and rearrange so value columns are next to error columns

moh_data_wide <-
  moh_data %>%
  pivot_wider(names_from = Var, values_from = c(Mean, Lower, Upper)) %>%
  select(Week_ending,
         any_of(names)) 


moh_data_wide <- as.data.frame(moh_data_wide) # I don't know why I need to do this

write.xlsx(moh_data_wide, "example_data/COVID 19 - MoH Health and Wellbeing Survey.xlsx", row.names = F) 

