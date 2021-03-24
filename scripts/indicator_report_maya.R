library(jsonlite)
library(writexl)

#indicators <- fromJSON(txt = "config/covid_19/covid_19_indicators.json")
definitions <- fromJSON(txt = "config/covid_19/covid_19_data_definitions.json")

definitions <- definitions %>% select(class, type, indicator_name, group_names)
#indicators <- indicators %>% select(class, type, indicator_name, groups)

# By ethnicity break down?-------------------------------------------

filter_ethnicity <- c("ethnic", "European", "Pacific", "Māori")

for (ethnicity in filter_ethnicity) {
  definitions[[ethnicity]] <- str_detect(string = definitions$group_names, pattern = ethnicity) | str_detect(string = definitions$indicator_name, pattern = ethnicity)
}

definitions$filter <- definitions$ethnic | definitions$European | definitions$Pacific | definitions$Māori

definitions <- definitions %>%
  select(class, type, indicator_name, filter) %>%
  arrange(filter, class, type, indicator_name) %>%
  unique()

# Unnested group names-------------------------------------
definitions <- unnest(data = definitions, cols = group_names)

definitions <- definitions %>%
  select(class, type, indicator_name, group_names, filter) %>%
  arrange(filter, class, type, indicator_name, group_names) %>%
  unique()

# Without group names---------------------------------------
definitions <- definitions %>%
  select(class, type, indicator_name) %>%
  arrange(class, type, indicator_name) %>%
  unique()

#Write output-------------------------------------------
previous <- as.data.frame(read_excel(path = "example_data/COVID-19 report.xlsx"))
compare <- full_join(definitions, previous)

if (sum(is.na(compare)) > 0) {
  print("Indicator list changed...")
  print(compare[which(is.na(compare)), ])
  write_xlsx(x = definitions, path = paste0("example_data/COVID-19 report.xlsx"))
}

