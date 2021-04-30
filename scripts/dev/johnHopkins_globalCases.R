# Number of cases (other countries)


countries <- c("Australia",
               "Australia - Australian Capital Territory",
               "Australia - New South Wales",
               "Australia - Northern Territory",
               "Australia - Queensland",
               "Australia - South Australia",
               "Australia - Tasmania",
               "Australia - Victoria",
               "Australia - Western Australia",
               "China",
               "Canada",
               "Singapore",
               "Italy",
               "Spain",
               "United Kingdom",
               "US",
               "Rest of the world")

config <- read_config_file()
file_path <- config$data_directory
files <- list.files(
    path = file_path,
    pattern = "COVID 19 - Global cases.xlsx",
    full.names = TRUE,
    recursive = TRUE
  )

  details <- (file.info(files)$ctime)
  current_date <- as.POSIXct(Sys.Date())
  github_url <- paste0(
    "https://raw.githubusercontent.com/CSSEGISandData/",
    "COVID-19/master/csse_covid_19_data/"
  )

  # if (details < current_date) {

    country <- c(
      "Spain",
      "Italy",
      "US",
      "United Kingdom",
      "Australia",
      "Canada",
      "Singapore",
      "China"
    )

    dth <- RCurl::getURL(paste0(
      github_url,
      "csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
    )
    deaths <- read.csv(text = dth)
    cnf <- RCurl::getURL(paste0(
      github_url,
      "csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
    )
    confirmed <- read.csv(text = cnf)

    rcv <- RCurl::getURL(paste0(
      github_url,
      "csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
    )
    recovered <- read.csv(text = rcv)

    jh_raw_data <- function(data, var) {
      Country <- grep("Country", names(data))
      Province <- grep("Province", names(data))
      date <- grep("X", names(data))

      colnames(data)[Country] <- "Country"
      colnames(data)[Province] <- "Province"
      new_data <- data[, c(Country, Province, date)]

      output <- new_data %>%
        gather(Date, var, -Country, -Province ) %>%
        mutate(Date = as.Date(
          str_replace_all(substr(Date, 2, nchar(Date)), "[X.]", "/"),
          format = "%m/%d/%y"
        )
        )

      colnames(output)[4] <- var

      return(output)
    }

    death_new <- jh_raw_data(deaths, "Deceased")
    recovered_new <- jh_raw_data(recovered, "Recovered")
    confirmed_new <- jh_raw_data(confirmed, "Active")

    all_data <- Reduce(
      function(x, y) merge(x, y, all = TRUE),
      list(death_new, recovered_new, confirmed_new)
    )

    all_data[is.na(all_data)] <- 0


    cases_selected_countries <- all_data %>%
      group_by(Date, Country ) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      filter(Country %in% country)


    cases_rest_of_world <- all_data %>%
      filter(!Country %in% country) %>%
      select(-Country) %>%
      group_by(Date) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      mutate(Country = "Rest of the world") %>%
      select(Country, everything())

    aus_provinces <- all_data %>%
      filter(Country == 'Australia') %>%
      mutate(Country = paste0(Country, " - ", Province)) %>%
      select(-Province)

    df_cases_all <- Reduce(
      function(x, y) merge(x = x, y = y, all = TRUE),
      list(cases_selected_countries, cases_rest_of_world, aus_provinces)
    ) %>%
      mutate(Active = Active - Recovered - Deceased) %>%
      arrange(Country) %>%
      replace(is.na(.), 0)

    check_for_negative <- function(data) {
      negative_active_cases <- which(data["Active"] < 0)
      if (!is.null(negative_active_cases)) {
        print(paste0("Negative Active cases present"))
        data[negative_active_cases, ] <- data[negative_active_cases - 1, ]
      }
      return(data)
    }

    data <- check_for_negative(df_cases_all)

    file.rename(from = paste0(file_path, "COVID 19 - Global cases.xlsx"),
                to = paste0(file_path, "/Previous/COVID 19 - Global cases.xlsx"))
    for (country in countries) {
      df <- data %>%
        filter(Country == country) %>%
        mutate(Date = as.Date(ymd(Date))) %>%
        select(-Country)
      print(str(df))

      write.xlsx(x = df, file = paste0(file_path, "COVID 19 - Global cases.xlsx"), sheetName = country,
                 append = TRUE,
                 row.names = FALSE)
    }

