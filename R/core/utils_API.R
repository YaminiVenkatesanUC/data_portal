
data_frame_to_api_helper <- function(directory, config, metadata, data){
  #error when there is not a match or indicator removed

  resource <- to_resource(config, metadata)
  write.table(resource, "dump.txt", append = TRUE)
  observations <- to_observations(config, metadata, data)
  observations <- observations[!is.na(observations$Value),]
  write.table(observations, "dump.txt", append = TRUE)

  #print(paste(resource$ResourceID, resource$Subject, resource$Title))
  version_obs <- getLatestVersion(location= list(collection = "PDS", instance = "Covid-19", table = "Observation_test"), server = "uat")

  writeDatastore(observations,location = list(collection = "PDS", instance = "Covid-19", table = "Observation_test"), version = version_obs, server = "uat")

}

create_odata_version <- function(){
  observation_dummy <- tibble("ResourceID" = "xxxx",
                         "Period" = "1800-01-01",
                         "Value" = 0,
                         "Unit" = "xxxx",
                         "Measure" = "xxxx",
                         "Multiplier" = 0)
  writeDatastore(observation_dummy,location = list(collection = "PDS", instance = "Covid-19", table = "Observation_test"), server = "uat")
}

to_observations <- function(config, metadata, data){
  names(data) <- c("parameter", config$value_names)
  data <- reshape2::melt(data,  id.vars = "parameter")
  Observations <- tibble("ResourceID" = rep(metadata$ResourceID, nrow(data)),
                        "Geo" = rep(check_null(metadata$Geo), nrow(data)),
                        "GeoUnit" = get_label(data, check_null(metadata$GeoUnit), nrow(data)),
                        "Duration" = rep(check_null(metadata$Duration), nrow(data)),
                        "Period" = get_peroid(data, config, nrow(data)),
                        "Label1" = get_label(data, check_null(metadata$Label1), nrow(data)),
                        "Label2" = get_label(data, check_null(metadata$Label2), nrow(data)),
                        "Label3" = get_label(data, check_null(metadata$Label3), nrow(data)),
                        "Label4" = get_label(data, check_null(metadata$Label4), nrow(data)),
                        "Label5" = get_label(data, check_null(metadata$Label5), nrow(data)),
                        "Label6" = get_label(data, check_null(metadata$Label6), nrow(data)),
                        "Value" = as.numeric(as.character(data$value)),
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

# If the indicator datatype is a bar chart there is no peroid
get_peroid <- function(data, config, len){
  data_type <- check_null(config$data_type)
  if(data_type == "BarChart" & !is.na(data_type) ){
    return(rep(NA,len))
  }
  return(data$parameter)
}

# If the indicator config specifies parameter then time sereis parameter is added as the label
# If the indicator config specifies variable then variable dimension is added to the label
# Else return the config label for all observations
get_label <- function(data, label, len){
  if (label == "parameter" & !is.na(label) ){
    return(data$parameter)
  }
  else if (label == "variable" & !is.na(label) ){
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

writeDatastore <- function(data, location, version = NULL, server = "uat") {
  if (server == "uat"){
    baseURL <- "https://epl-uat/statsnz-epl-data/api/v1/collections/"
  }
  else{
    baseURL <- "https://epl-prd/statsnz-epl-data/api/v1/collections/"
  }
  #POST /api/v1/collections/{collectionCode}/{collectionInstanceCode}/datasets/{tableName}
  if(is.null(version)){
    theUrl <- paste0( baseURL,
                      location$collection,
                      "/",
                      location$instance,
                      "/datasets/",
                      location$table)
  }else{
    # add versions.
    theUrl <- paste0( baseURL,
                      location$collection,
                      "/",
                      location$instance,
                      "/datasets/",
                      location$table,
                      "/versions/",
                      version)
  }
  result <- httr::POST( url = theUrl, httr::use_proxy(""), httr::config(http_version = 2L), httr::config(ssl_verifypeer = 0L),
                        httr::authenticate("","", type ="gssnegotiate"),
                        httr::content_type_json(),
                        body = jsonlite::toJSON(data), encode = "raw" )
  # boolean success code
  if(http_error(result))
  {
    errorMessage <- httr::content(result, "text", encoding = "UTF-8")
    message(errorMessage)
  }else{
    errorMessage <- "success"
  }
  errorMessage
}


#' @title Get latest version
#' @description Gets the latest version of a datastore table and adds one
#'
#' @param location   The datastore location, a list with collecton, instance and table
#' @param server     prd or uat
#'
#' @return version   version number
#'
#' @export
getLatestVersion <- function(location, server = "uat") {
  # set-up version to write to
  # GET statsnz-epl-metadata/api/v1/collections/{collection}/{collectionInstance}/tables/{tableName}/versions
  if (server == "uat"){
    baseURL <- "https://epl-uat/statsnz-epl-metadata/api/v1/collections/"
  }
  else{
    baseURL <- "https://epl-prd/statsnz-epl-metadata/api/v1/collections/"
  }
  theUrl <- paste0( baseURL,
                    location$collection,
                    "/",
                    location$instance,
                    "/tables/",
                    location$table,
                    "/versions")
  # get current version
  result <- httr::GET(url = theUrl, httr::use_proxy(""), httr::config(http_version = 2L), httr::config(ssl_verifypeer = 0L),
                      httr::authenticate("","", type ="gssnegotiate"))

  version <- jsonlite::fromJSON(httr::content(result, "text", encoding = "UTF-8"))
  if (length(version) > 0){
    version <- version$VersionNumber[1]
  } else{
    version <- 0
  }
  version
}