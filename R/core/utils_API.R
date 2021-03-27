
data_frame_to_api_helper <- function(directory, config, metadata, data){
  #error when there is not a match or indicator removed
  resource <- to_resource(config, metadata)
  write.table(resource, "dump.txt", append = TRUE)
  observations <- to_observations(config, metadata, data)
  write.table(observations, "dump.txt", append = TRUE)

  #writeDatastore(observations,location = list(collection = "PDS", instance = "Covid-19", table = "Observation_test"), version = 9, server = "uat")
}

to_observations <- function(config, metadata, data){
  names(data) <- c("parameter", config$value_names)
  data <- reshape2::melt(data,  id.vars = "parameter")
  Observations <- tibble("ResourceID" = rep(metadata$ResourceID, nrow(data)),
                        "Geo" = get_label(data, check_null(metadata$Geo), nrow(data)),
                        "GeoUnit" = rep(check_null(metadata$GeoUnit), nrow(data)),
                        "Duration" = rep(check_null(metadata$Duration), nrow(data)),
                        "Period" = data$parameter,
                        "Label1" = get_label(data, check_null(metadata$Label1), nrow(data)),
                        "Label2" = get_label(data, check_null(metadata$Label2), nrow(data)),
                        "Label3" = get_label(data, check_null(metadata$Label3), nrow(data)),
                        "Label4" = get_label(data, check_null(metadata$Label4), nrow(data)),
                        "Label5" = get_label(data, check_null(metadata$Label5), nrow(data)),
                        "Label6" = get_label(data, check_null(metadata$Label6), nrow(data)),
                        "Value" = data$value,
                        "Unit" = rep(check_null(metadata$Unit), nrow(data)),
                        "Measure" = rep(check_null(metadata$Measure), nrow(data)),
                        "NullReason" = NA,
                        "Multiplier" = rep(check_null(metadata$Multiplier), nrow(data)),
                        "Status" = NA) #%>% toJSON(na ="null")
  return(Observations)
}

to_resource <- function(config, metadata){
  Resource <- tibble("LatestDataTable" = "",
                     "ResourceID" = metadata$ResourceID,
                     "Subject" = metadata$Subject,
                     "Title" = metadata$Title,
                     "Description" = check_null(metadata$Description),
                     "Notes" = check_null(metadata$Notes),
                     "Caveats" = check_null(metadata$Caveats),
                     "Source" = check_null(metadata$Source),
                     "SourceURL" = check_null(metadata$SourceURL),
                     #"Modified" = check_null(metadata$Modified),
                     "Frequency" = check_null(metadata$Frequency),
                     "Var1" = check_null(metadata$Var1),
                     "Var2" = check_null(metadata$Var2),
                     "Var3" = check_null(metadata$Var3),
                     "Var4" = check_null(metadata$Var4),
                     "Var5" = check_null(metadata$Var5),
                     "Var6" = check_null(metadata$Var6)) #%>% toJSON(na ="null")
  return(Resource)
}

get_label <- function(data, label, len){
  if (label == "variable" & !is.na(label) ){
    return(data$variable)
  }
  return(rep(label,len))
}

check_null <- function(value){
  if (is.null(value)){
    return(NA)
  }
  return(value)
}