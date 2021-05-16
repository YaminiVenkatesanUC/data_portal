# Life worthwhile +++ Family wellbeing +++ Enough money for everyday needs +++ Help from welfare organisation +++
# Life satisfaction – HLFS quarterly +++ Trust for parliament +++ Trust for police +++ Trust for the media +++
# Trust in other people +++ Trust for health system +++ Loneliness – past 4 weeks +++ Experienced discrimination

library(openxlsx)
library(dplyr)

directory <- "~/Network-Shares/U-Drive-SAS-03BAU/MEES/National Accounts/COVID-19 data_Secure/COVID-19_dashboard/"
path <- paste0(directory, "HLFS/")
files <- file.info(list.files(path, full.names = T))

series_list <- list(
  TotalSex = list(
    sheet = "Sex",
    header = c("Male", "Female"),
    col_names = c("Quarter",
                  "Total", "Total_lower", "Total_upper",
                  "Male", "Male_lower", "Male_upper",
                  "Female", "Female_lower", "Female_upper")
  ),
  By_age = list(
    sheet = "Age",
    header = c("18–24", "25–34", "35–44", "45–54", "55–64", "65–74", "75+"),
    col_names = c("Quarter",
                  "Total", "Total_lower", "Total_upper",
                  "18–24", "18–24_lower", "18–24_upper",
                  "25–34", "25–34_lower", "25–34_upper",
                  "35–44", "35–44_lower", "35–44_upper",
                  "45–54", "45–54_lower", "45–54_upper",
                  "55–64", "55–64_lower", "55–64_upper",
                  "65–74", "65–74_lower", "65–74_upper",
                  "75+", "75+_lower", "75+_upper")
    ),
  By_ethnicity = list(
    sheet = "Ethnic",
    header = c("European", "Māori", "Pacific peoples", "Asian"),
    col_names = c("Quarter",
                  "Total", "Total_lower", "Total_upper",
                  "European", "European_lower", "European_upper",
                  "Māori", "Māori_lower", "Māori_upper",
                  "Pacific peoples", "Pacific peoples_lower", "Pacific peoples_upper",
                  "Asian", "Asian_lower", "Asian_upper")
),
  By_region = list(
    sheet = "Region",
    header = c("Northland",
               "Auckland",
               "Waikato",
               "Bay of Plenty",
               "Gisborne/Hawke's Bay",
               "Taranaki",
               "Manawatu-Whanganui",
               "Wellington",
               "Nelson/Tasman/Marlborough/West Coast",
               "Canterbury",
               "Otago",
               "Southland"),
    col_names = c("Quarter",
                  "Total", "Total_lower", "Total_upper",
                  "Northland", "Northland_lower", "Northland_upper",
                  "Auckland", "Auckland_lower", "Auckland_upper",
                  "Waikato", "Waikato_lower", "Waikato_upper",
                  "Bay of Plenty", "Bay of Plenty_lower", "Bay of Plenty_upper",
                  "Gisborne/Hawke's Bay", "Gisborne/Hawke's Bay_lower", "Gisborne/Hawke's Bay_upper",
                  "Taranaki", "Taranaki_lower", "Taranaki_upper",
                  "Manawatu-Whanganui", "Manawatu-Whanganui_lower", "Manawatu-Whanganui_upper",
                  "Wellington", "Wellington_lower", "Wellington_upper",
                  "Nelson/Tasman/Marlborough/West Coast", "Nelson/Tasman/Marlborough/West Coast_lower", "Nelson/Tasman/Marlborough/West Coast_upper",
                  "Canterbury", "Canterbury_lower", "Canterbury_upper",
                  "Otago", "Otago_lower", "Otago_upper",
                  "Southland", "Southland_lower", "Southland_upper")
    ),
  By_parent_status = list(
    sheet = "Parent status",
    header = c("Sole parent",
               "Mother in two-parent family",
               "Father in two-parent family",
               "Not parent of dependent child, female",
               "Not parent of dependent child, male"),
    col_names = c("Quarter",
                  "Total", "Total_lower", "Total_upper",
                  "Sole parent", "Sole parent_lower", "Sole parent_upper",
                  "Mother in two-parent family", "Mother in two-parent family_lower", "Mother in two-parent family_upper",
                  "Father in two-parent family", "Father in two-parent family_lower", "Father in two-parent family_upper",
                  "Not parent of dependent child, female", "Not parent of dependent child, female_lower", "Not parent of dependent child, female_upper",
                  "Not parent of dependent child, male", "Not parent of dependent child, male_lower", "Not parent of dependent child, male_upper")
  ),
  By_disability = list(
    sheet = "Disability",
    header = c("Disabled",
               "Non–disabled",
               "Disabled",
               "Non–disabled"),
    col_names = c("Quarter",
                  "Total", "Total_lower", "Total_upper",
                  "Disabled_18_over", "Disabled_18_over_lower", "Disabled_18_over_upper",
                  "Non–disabled_18_over", "Non–disabled_18_over_lower", "Non–disabled_18_over_upper",
                  "Disabled_18-64", "Disabled_18-64_lower", "Disabled_18-64_upper",
                  "Non–disabled_18-64", "Non–disabled_18-64_lower", "Non–disabled_18-64_upper")
  )
)

load_parameters <- list(
  Life_worthwhile = list(
    description = "Life worthwhile",
    rows = c(15:17),
    keyword = "7 to 10"
  ),
  Family_wellbeing = list(
    description = "Family wellbeing",
    rows = c(18:20),
    keyword = "7 to 10"
  ),
  Enough_money_for_everyday_needs = list(
    description = "Adequacy of income to meet everyday needs",
    keyword = "Enough money / more than enough money",
    rows = c(22:25)
  ),
  Help_from_welfare_organisation = list(
    description = "Received help from organisation such as a church or foodbank in last 12 months",
    keyword = "At least once",
    rows = c(26:28)
  ),
  Life_satisfaction_quarterly = list(
    description = "Overall life satisfaction",
    keyword = "7 to 10",
    rows = c(12:14)
  ),
  Trust_for_parliament = list(
    description = "Trust held for parliament",
    keyword = "7 to 10",
    rows = c(44:46)
  ),
  Trust_for_police = list(
    description = "Trust held for police",
    keyword = "7 to 10",
    rows = c(47:49)
  ),
  Trust_for_the_media = list(
    description = "Trust held for media",
    keyword = "7 to 10",
    rows = c(50:52)
  ),
  Trust_in_other_people = list(
    description = "Trust held for people in New Zealand",
    keyword = "7 to 10",
    rows = c(37:39)
  ),
  Trust_for_health_system = list(
    description = "Trust held for health system",
    rows = c(41:43),
    keyword = "7 to 10"
  ),
  Loneliness_past_4_weeks = list(
    description = "Felt lonely in last four weeks",
    keyword = "Some / most / all of the time",
    rows = c(32:35)
  ),
  Experienced_discrimination = list(
    description = "Discrimination",
    rows = c(29:30),
    keyword = "Experienced discrimination in last 12 months"
  )
)


for (ind in 1:length(names(load_parameters))) {
  file.rename(from = paste0(directory, "/COVID 19 - HLFS - ", names(load_parameters)[[ind]], ".xlsx"),
              to = paste0(directory, "Previous/COVID 19 - HLFS - ", names(load_parameters)[[ind]], ".xlsx"))
  OUT <- createWorkbook()
  param <- load_parameters[[ind]]
  for (ser in 1:length(series_list)) {
    series <- series_list[[ser]]
    data_all <- data.frame(matrix(ncol = length(series$col_names), nrow = 0))
    colnames(data_all) <- series$col_names
    for (file in rownames(files)) {
      if (series$sheet %in% excel_sheets(file)) {
        quarter <- as.data.frame(read_excel(file, series$sheet, range = "A4", col_names = FALSE))[[1]] %>%
          stringr::str_replace("quarter", replacement = "01") %>%
          as.Date(format = "%B %Y %d")

        df <- read.xlsx(xlsxFile = file,
                        sheet = series$sheet,
                        rows = param$rows,
                        colNames = FALSE,
                        rowNames = FALSE,
                        skipEmptyRows = TRUE,
                        skipEmptyCols = TRUE,
                        check.names = FALSE,
                        fillMergedCells = FALSE,
                        na.strings = c("*", "**"))

        header <- read.xlsx(xlsxFile = file,
                            sheet = series$sheet,
                            rows = c(6),
                            colNames = TRUE,
                            rowNames = FALSE,
                            skipEmptyRows = TRUE,
                            skipEmptyCols = TRUE,
                            check.names = FALSE,
                            fillMergedCells = TRUE,
                            sep.names = " ")

        if (!str_detect(df$X1[1], param$description)) stop ("Row structure have changed in sheet: ", series$sheet)

        df <- df %>%
          filter(X2 == param$keyword) %>%
          select(-X1, -X2)

        # drop columns with NA!!!
        df <- df %>% select(where(~!all(is.na(.x))))

        if (!identical(names(header), series$header)) stop ("Column structure have changed in sheet: ", series$sheet)

        value_cols <- seq(from = 1, to = length(colnames(df)), by = 2)
        error_cols <- seq(from = 2, to = length(colnames(df)), by = 2)

        for (col in value_cols) {
          df <- df %>%
            mutate(lower = df[[col]] - df[[col+1]]) %>%
            mutate(upper = df[[col]] + df[[col+1]]) %>%
            dplyr::rename_with(~ paste0(colnames(df)[[col]], "_lower"), .cols = lower) %>%
            dplyr::rename_with(~ paste0(colnames(df)[[col]], "_upper"), .cols = upper)
        }

        df <- df %>%
          mutate(Date = quarter) %>%
          select(-error_cols)

        df <- df %>%
          select(order(colnames(df)))

        colnames(df) <- series$col_names
        data_all <- rbind(data_all, df)

        data_all <- data_all %>%
          arrange(Quarter)
      } else {
        warning("Sheet has been dropped from raw file: ", series$sheet)
      }

    }
addWorksheet(OUT, names(series_list)[[ser]])
writeData(OUT, sheet = names(series_list)[[ser]], x = data_all)
  }
  saveWorkbook(OUT, paste0(directory, "/COVID 19 - HLFS - ", names(load_parameters)[[ind]], ".xlsx"))
}
