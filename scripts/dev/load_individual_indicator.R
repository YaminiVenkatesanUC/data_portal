source("R/core/utils_API.R")
source("R/load_functions.R")

CONFIG <- read_config_file()
indicator_name <- "Source of cases"
directory <- CONFIG$data_directory
odata_definitions <- fromJSON(CONFIG$odata_definitions)
data_definitions <- fromJSON(CONFIG$data_definitions)
config <- data_definitions[data_definitions$indicator_name == indicator_name,] %>% as.list()

func <- eval(parse(text=config$load_function))
data <- func(indicator_definition, directory, odata_definitions)

