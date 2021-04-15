' @title Write to datastore
#' #@description This does a simple write to datastore, only good if data is small...
#'
#' @param data       The data to be written
#' @param location   The datastore location, a list with collecton, instance and table
#' @param version    The version to write to, if this is included it appends to the table.
#'
#' @return success   TRUE / FALSE if http POST successful
#'
#' @export

observation <- tibble("ResourceID" = "CPWAG1",
                   "Geo" = "",
                   "GeoUnit" = "",
                   "Duration" = "P7D",
                   "Period" = "2020-03-20",
                   "Label1" = "",
                   "Label2" = "",
                   "Label3" = "",
                   "Label4" = "",
                   "Label5" = "",
                   "Label6" = "",
                   "Value" = 3,
                   "Unit" = "NZD",
                   "Measure" = "Wage subsidy paid out (cumulative)",
                   "NullReason" = "",
                   "Multiplier" = 0,
                   "Status" = "") #%>% toJSON(na ="null")

writeDatastore(observation,location = list(collection = "PDS", instance = "Covid-19", table = "Observation_test"), version = 20, server = "uat")


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


#' @title Write whole dataset to datastore
#' @description This chunks up a large table to write it out to datastore
#'
#' @param data       The data to be written
#' @param location   The datastore location, a list with collecton, instance and table
#'
#' @return success   TRUE / FALSE if http POST successful
#'
#' @export

