
data_frame_to_json_helper <- function(directory, config, odata_definitions, data){
  metadata <- odata_definitions[which(odata_definitions$indicator_name == config$indicator_name),]
  resource <- to_resource(config, metadata)
  print(resource)
  observations <- to_observations(config, data)
  print(observations)
}

to_observations <- function(config, data){
  data <- reshape2::melt(data,  id.vars = "Parameter") #%>% toJSON(na ="null")
  return(data)
}

to_resource <- function(config, metadata){
  Resource <- tibble("ResourceID" = metadata$ResourceID,
                       "Subject" = metadata$Subject,
                       "Title" = metadata$Title,
                       "Description" = NA,
                       "Notes" = NA,
                       "Caveats" = NA,
                       "Source" = NA,
                       "SourceURL" = NA,
                       "Modified" = NA,
                       "Frequency" = check_null(metadata$Frequency),
                       "Var1" = check_null(metadata$Var1),
                       "Var2" = check_null(metadata$Var2),
                       "Var3" = check_null(metadata$Var3)) #%>% toJSON(na ="null")
  return(Resource)
}

check_null <- function(value){
  if (is.null(value)){
    return(NA)
  }
  return(value)
}