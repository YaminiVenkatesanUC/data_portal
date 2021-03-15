library(jsonlite)
library(writexl)

indicators <- fromJSON(txt = "config/covid_19/covid_19_indicators.json")
definitions <- fromJSON(txt = "config/covid_19/covid_19_data_definitions.json")

definitions <- definitions %>% select(class, type, indicator_name, group_names)

#'By ethnicity' break down?--------------------------------

filter_ethnicity <- c("ethnic", "European", "Pacific", "Māori")

for (ethnicity in filter_ethnicity) {
  definitions[[ethnicity]] <- str_detect(string = definitions$group_names, pattern = ethnicity) | str_detect(string = definitions$indicator_name, pattern = ethnicity)
}

definitions$filter <- definitions$ethnic | definitions$European | definitions$Pacific | definitions$Māori

definitions <- definitions %>%
  select(class, type, indicator_name, filter) %>%
  arrange(filter, class, type, indicator_name) %>%
  unique()


#Write output-------------------------------------------
write_xlsx(x = definitions, path = "example_data/COVID-19 report.xlsx")
