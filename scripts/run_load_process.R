source("scripts/load_data.R")
CONFIG <- read_config_file()
load_data(CONFIG, odata_load_flag=TRUE)
