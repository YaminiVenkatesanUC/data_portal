
data_frame_to_json_helper <- function(directory, config, data){
  print(config)
  print(head(data))
  resource <- to_resource(config)
  observations <- to_observations(config, data)
}

to_observations <- functions(config, data){


}

to_resource <- function(config){
  Frequency <- check_null(config$frequency)
  Var1 <- check_null(config$var1)
  Var2 <- check_null(config$var2)
  Var3 <- check_null(config$var3)
  Resource <- tibble("ResourceID" = config$api_resource_id,
                       "Subject" = config$type,
                       "Title" = config$indicator_name,
                       "Description" = NA,
                       "Notes" = NA,
                       "Caveats" = NA,
                       "Source" = NA,
                       "SourceURL" = NA,
                       "Modified" = NA,
                       "Frequency" = Frequency,
                       "Var1" = Var1,
                       "Var2" = Var2,
                       "Var3" = Var3) %>% toJSON(na ="null")
}

check_null <- function(value){
  if (is.null(value)){
    return(NA)
  }
  return(value)
}