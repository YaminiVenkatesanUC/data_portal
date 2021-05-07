### This code processes the CFFC first-wave unit record data


### NOTE!!!!!
### This will need reformatting when the second wave comes
### to be a time series, i.e., one column per series
### and one column for the date

### this will also require adjustment in the config
### and possibly some date presentation options
### so that we can show e.g., "April 2020"





library(tidyverse)
library(openxlsx)
library(haven)
library(sjlabelled)

data <-  c("~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/Portal Data Supply/CFFC/Wave 1/NZ_Covid 19 Wellbeing 2 with components with serials.sav",
           "~/Network-Shares/J-Drive-WLG-Shared/Indicators_aotearoa/Covid-19/Portal Data Supply/CFFC/Wave 1/ORD-551132-J1V4_Covid19 Wellbeing W2_MAIN_Final Data_SPSS_v1.sav")

# data <- c("example_data/NZ_Covid 19 Wellbeing 2 with components with serials.sav",
#           "example_data/ORD-551132-J1V4_Covid19 Wellbeing W2_MAIN_Final Data_SPSS_v1.sav")

output_list <- list()

for (i in 1:length(data)) {
  # read the SPSS file
  cffc <- read_sav(data[i])

  # get labels instead of codes for every variable
  cffc <-
    cffc %>%
    mutate(across(everything(), as_label))


  # variables at the household level
  # a1 is the filter for respondents who could answer for their household

  names(cffc) <- tolower(names(cffc))

  cffc_h <-
    cffc %>%
    filter(a1 == "Yes") %>%
    select("a3_wb",
           "b18_wb_1",
           "c101",
           "c17",
           paste0("b19_wb", 11:19))


  # variables at the individual level
  # removed corona_3c

  cffc_i <-
    cffc %>%
    select(
      "anxiety",
      paste0("corona_",
             c("1c1",
               "1c2",
               "1c3",
               "1c4",
               "2c1",
               "2c2",
               "2c3",
               "2c4",
               "3c1",
               "3c2",
               "3c3",
               "3c4")
      )
    ) %>%

    ## Recodes to 'yes' if this has happened
    ## to respondent OR the partner they live with, 'no' if neither

    mutate(across(starts_with("corona"), ~(.x=="Selected")))  %>%
    mutate(corona_1c = case_when(
      corona_1c4 ~ "Don't know",
      corona_1c3 ~ "No",
      corona_1c1 | corona_1c2 ~ "Yes",
      TRUE ~ "No"
    )
    ) %>%
    mutate(corona_2c = case_when(
      corona_2c4 ~ "Don't know",
      corona_2c3 ~ "No",
      corona_2c1 | corona_2c2 ~ "Yes",
      TRUE ~ "No"
    )
    ) %>%
    mutate(corona_3c = case_when(
      corona_3c4 ~ "Don't know",
      corona_3c3 ~ "No",
      corona_3c1 | corona_3c2 ~ "Yes",
      TRUE ~ "No"
    )
    ) %>%
    select(anxiety, corona_1c, corona_2c, corona_3c)


  # create one dataframe per indicator in a list object
  # filter out Don't Know answers to exclude from denominator
  # then calculate relative frequencies

  cffc_output <- list(lapply(colnames(cffc_h),
                                            function(x) {
                                              df <- cffc_h %>%
                                                filter(.data[[x]] != "Don't know") %>%
                                                group_by(.data[[x]]) %>%
                                                summarise (n = n()) %>%
                                                mutate(Percentage = n / sum(n)) %>%
                                                mutate(Percentage = round(100 * Percentage, 1)) %>%
                                                select(-n)
                                            }
  ),
  lapply(colnames(cffc_i),
         function(x) {
           df <- cffc_i %>%
             filter(.data[[x]] != "Don't know") %>%
             group_by(.data[[x]]) %>%
             summarise (n = n()) %>%
             mutate(Percentage = n / sum(n)) %>%
             mutate(Percentage = round(100 * Percentage, 1)) %>%
             select(-n)
         }
  )
  )





  # un-nest the list
  # no one wants a list of lists

  cffc_output <- unlist(cffc_output, recursive = FALSE)
  names(cffc_output) <- c(colnames(cffc_h), colnames(cffc_i))



  # all I can say about these next few lines is 'sorry'

  # we need one dataframe for all of the b19_wb variables
  # this  code:
  # creates an empty dataframe
  # cycles through and adds the appropriate values
  # gets the question labels instead of e.g., b19_wb11

  cffc_output$b19_wb <- as.data.frame(
    list("Question" = vector(),
         "Percentage" = vector())
  )

  vars <- paste0("b19_wb", 11:19)

  for (var in vars) {

    colnames(cffc_output[[var]]) <- c ("Question", "Percentage")
    cffc_output$b19_wb <- rbind(
      cffc_output$b19_wb,
      as.data.frame(
        list(
          "Question" = as.character(colnames(label_to_colnames(cffc))[colnames(cffc) == var]),
          "Percentage" = cffc_output[[var]][2, 2])
      )
    )

  }


  # we don't need the full labels - extract the meaningful portion
  # using regex (a bit unnecessary)
  # gsub seemed easier than substring but either works

  cffc_output$b19_wb <-
    cffc_output$b19_wb %>%
    mutate(Question = str_extract_all(Question,
                                      "\\:.+?\\-")) %>%
    mutate(Question = gsub(": ", "", Question)) %>%
    mutate(Question = gsub(" -", "", Question))




  cffc_output$corona <- as.data.frame(
    list(
      Question = c("Temporarily laid off work and not receiving salary",
                   "Lost job* and now unemployed",
                   "Earnings fallen substantially"
      ),
      Percentage = unlist(c(
        cffc_output$corona_3c[2, 2],
        cffc_output$corona_1c[2, 2],
        cffc_output$corona_2c[2, 2]))
    )
  )



  #


  # keep the dataframes we want
  # controlling the order just makes config easier

  df_order <- c("a3_wb",
                "b18_wb_1",
                "c101",
                "c17",
                "anxiety",
                "b19_wb",
                "corona")


  cffc_output <- cffc_output[df_order]

  output_list[[i]] <- cffc_output

}

if (file.exists("example_data/CFFC_output.xlsx")) file.remove("example_data/CFFC_output.xlsx")
OUT <- createWorkbook()

for (i in 1:length(output_list)) {
  for (j in 1:length(output_list[[i]])) {
    if (i <= length(output_list)-1) {
      df_i <- as.data.frame(output_list[[i]][j])
      df_ii <- as.data.frame(output_list[[i+1]][j])
      df_i[[1]] <- as.factor(df_i[[1]])
      df_ii[[1]] <- as.factor(df_ii[[1]])
      if (all(levels(df_i[[1]]) != levels(df_ii[[1]]))) {
        df_ii <-df_ii[order(df_ii[[1]], decreasing = TRUE),]
        names(df_i) <- c(df_order[j], "wave1")
        names(df_ii) <- c(df_order[j], "wave2")
        df <- cbind(df_i, df_ii) %>% select(3, 2, 4)

      } else{
        df <- merge(df_i, df_ii, by.x = 1, by.y = 1, all.x = T, all.y = F, sort = FALSE)
        names(df) <- c(df_order[j], "wave1", "wave2")
      }

      addWorksheet(OUT, df_order[j])
      writeData(OUT, sheet = df_order[j], x = df)

      # this function is no longer behaving... :(
      # write.xlsx(df,
      #            file = "example_data/CFFC_output.xlsx",
      #            sheetName = df_order[j],
      #            append = T,
      #            showNA = F)
    }
    saveWorkbook(OUT, "example_data/CFFC_output.xlsx")
  }
}
