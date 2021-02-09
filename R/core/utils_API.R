
data_frame_to_json_helper <- function(directory, config, data){
  resource <- to_resource(config)
  observations <- to_observations(config, data)
}

to_observations <- functions(config, data){
  data <- reshape2::melt(data,  id.vars = "Parameter") %>% toJSON(na ="null")
  return(data)
}

to_resource <- function(config){
  Resource <- tibble("ResourceID" = config$api_resource_id,
                       "Subject" = config$type,
                       "Title" = config$indicator_name,
                       "Description" = NA,
                       "Notes" = NA,
                       "Caveats" = NA,
                       "Source" = NA,
                       "SourceURL" = NA,
                       "Modified" = NA,
                       "Frequency" = check_null(config$frequency),
                       "Var1" = check_null(config$var1),
                       "Var2" = check_null(config$var2),
                       "Var3" = check_null(config$var3)) %>% toJSON(na ="null")
}

check_null <- function(value){
  if (is.null(value)){
    return(NA)
  }
  return(value)
}