source("R/core/utils_API.R")
source("R/load_functions.R")

CONFIG <- read_config_file()
indicator_name <- "Confidence in household's financial situation"
directory <- CONFIG$data_directory
odata_definitions <- fromJSON(CONFIG$odata_definitions)
config <- get_config(CONFIG$data_definitions, indicator_name)

data <- load_functions[[config$load_function]](config, directory, odata_definitions)

get_config <- function(config, indicator_name){
  data_definitions <- read_json(config)
  for(data_definition in data_definitions){
    if(data_definition$indicator_name == indicator_name){return(data_definition)}
  }
}