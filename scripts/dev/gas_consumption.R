# Gas consumption by largest users +++ Gas consumption by selected major users






gas_use_data <- function(config, directory) {
  data <- as.data.frame(read_excel(
    paste0(directory, config$filename),
    sheet = config$sheet_number,
    skip = config$skip
  )) %>%
    select(-`Source: First Gas`)

  if (config$gas_source == "Vector") {
    data <- data %>%
      select(Date = `...1`,  everything()) %>%
      mutate(
        Date = dmy(Date),
        `Ballance Agri- Nutrients` = `Ballance Agri-Nutrients...2` +
          `Ballance Agri-Nutrients...3`,
        Fonterra = `Subtotal Fonterra...6` +
          `Subtotal Fonterra...7` +
          `Subtotal Fonterra...8` +
          `Subtotal Fonterra...9` +
          `Subtotal Fonterra...10` +
          `Subtotal Fonterra...11` +
          `Subtotal Fonterra...12` +
          `Subtotal Fonterra...13` +
          `Subtotal Fonterra...14`
      ) %>%
      select(
        Date,
        Fonterra,
        `Ballance Agri- Nutrients`,
        `Glenbrook steel mill`,
        `Kinleith pulp and paper mill`,
        `Marsden Point oil refinery`
      )
  }

  if (config$gas_source == "Maui") {
    data <- data %>%
      mutate(Date = dmy(`...1`), Methanex = `...4` + `Methanex Motunui`) %>%
      select(Date, everything(), -`...1`, -`...4`, -`Methanex Motunui`)
  }

  colnames(data) <- c("Parameter", paste0("col_", 2:ncol(data)))
  return(data_frame_to_data_object_helper(
    directory,
    config,
    data
  ))
}