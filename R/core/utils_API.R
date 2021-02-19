
data_frame_to_json_helper <- function(directory, config, odata_definitions, data){
  #error when there is not a match or indicator removed
  metadata <- odata_definitions[which(odata_definitions$ResourceID == config$api_resource_id),]
  resource <- to_resource(config, metadata)
  print(resource)
  observations <- to_observations(config, metadata, data)
}

to_observations <- function(config, metadata, data){
  names(data) <- c("parameter", config$value_names)
  data <- reshape2::melt(data,  id.vars = "parameter") #%>% toJSON(na ="null")
  print(rep(metadata$Duration, nrow(data)))
  Observations <- tibble("ResourceID" = rep(metadata$ResourceID, nrow(data)),
                        "Geo" = "",
                        "GeoUnit" = "",
                        "Duration" = rep(metadata$Duration, nrow(data)),
                        "Peroid" = data$parameter,
                        "Label1" = "",
                        "Label2" = "",
                        "Label3" = "",
                        "Label4" = "",
                        "Label5" = "",
                        "Label6" = "",
                        "Value" = data$value,
                        "Unit" = rep(metadata$Unit, nrow(data)),
                        "Mesasure" = rep(metadata$Measure, nrow(data)),
                        "NullReason" = "",
                        "Multiplier" = rep(metadata$Multiplier, nrow(data)),
                        "Status" = "")
  print(Observations)
  return(Observations)
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
                     "Var3" = check_null(metadata$Var3),
                     "Var4" = check_null(metadata$Var4),
                     "Var5" = check_null(metadata$Var5),
                     "Var6" = check_null(metadata$Var6)) #%>% toJSON(na ="null")
  return(Resource)
}

check_null <- function(value){
  if (is.null(value)){
    return(NA)
  }
  return(value)
}