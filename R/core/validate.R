
validate_indicator_match <- function(indicator_definition, data_object, group_definition){
  data <- load_from_store(indicator_definition, group_definition$name)
  if (!is.null(data) ){ # condition is being hit when page loads?
    NULL
  } else {
    print("Data store and indicator definitions do not match")
  }
}

validate_indicator_data <- function(data_object){
  # add check for time series plot
  if(!is.null(data_object)){
    if (nrow(data_object[['values']]) == length(data_object[['dates']])) {
      NULL
    } else {
      print("Missing data entry")
    }
  }
}

validate_indicator_groups <- function(indicator_definition, data_object, group_definition){
  key <- paste(indicator_definition$class, indicator_definition$type, indicator_definition$indicator_name, ".*", sep = "_")
  indicator_match <- str_match(names(DATA_STORE), key)
  indicator_match <- indicator_match[!is.na(indicator_match)]
  print(indicator_definition$groups[indicator_match])
  print(indicator_match)
  print(" ")
  NULL
}

validate_date_format <- function(indicator_definition, data_object) {
  NULL
}