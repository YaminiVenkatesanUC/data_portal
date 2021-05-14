# Student visa +++ Work visa

library(dplyr)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
folder <- paste0(directory, "MBIE/Visa/")

app_student <- read.csv(list.files(folder, pattern = "*.Student.*", full.names = TRUE))
app_work <- read.csv(list.files(folder, pattern = "*.Work.*", full.names = TRUE))
visa <- read.csv(list.files(folder, pattern = "*.Visa.*", full.names = TRUE))

student <- app_student %>%
  pivot_wider(names_from = Application_Substream, values_from = Count) %>%
  mutate(Total = visa$Count[visa$Visa_Type == 'Student'])

work <- app_work %>%
  pivot_wider(names_from = Application_Substream, values_from = Count) %>%
  mutate(Total = visa$Count[visa$Visa_Type == 'Work']) %>%
  mutate(Work_other = `Business` +
           `Crew of foreign fishing vessel` +
           `Humanitarian/International` +
           `Other` +
           `Post Study Work` +
           `Specific purposes` +
           `Student and trainee` +
           `Work to residence`) %>%
  select(Date,
         `Essential Skills`,
         `Family`,
         `Horticulture and Viticulture seasonal work`,
         `Work_other`,
         `Working Holiday Scheme`,
         `Total`)

file.rename(from = paste0(directory, "/COVID-19 MBIE Student visa.csv"),
            to = paste0(directory, "/Previous/COVID-19 MBIE Student visa.csv"))
file.rename(from = paste0(directory, "/COVID-19 MBIE Work visa.csv"),
            to = paste0(directory, "/Previous/COVID-19 MBIE Work visa.csv"))
write.csv(x = student, file = paste0(directory, "COVID-19 MBIE Student visa.csv"), row.names = FALSE)
write.csv(x = work, file = paste0(directory, "COVID-19 MBIE Work visa.csv"), row.names = FALSE)
