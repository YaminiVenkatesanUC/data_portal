
#' @title Gets data from datastore
#' @description This function calls the datastore API to retrieve data from a single dataset in the datastore.
#'
#' @param location   The datastore location, a list with collecton, instance and table
#' @param instance   This is the instance that the dataset is located in within the datastore eg. "2013"
#' @param variables  This is a vector list of variables that you want from the dataset eg. vars = ["age_code", "sex_code"]
#' @param filters    This is if you want to filter the data that you get eg filter = "[sex_code]=2"
#' @param other      This is extra parameters for getting the data such as limits eg. other = "&limit=1000"
#' @param server     Which datastore server uat or prd to use
#'
#' @return A dataset from the datastore.


library (jsonlite)
library (httr)


observation <- getDatastore(location = list(collection = "PDS", instance = "Covid-19", table = "Observation"), variables = "", filters = "", other ="", server = "uat")


getDatastore <- function(location, variables = "", filters = "", other = "", server = "uat")
{

  if(grepl("[1-9a-zA-Z]",location$instance) == FALSE | grepl("[1-9a-zA-Z]",location$table) == FALSE | grepl("[1-9a-zA-Z]", location$collection) == FALSE) {
    stop("At least one input argument is missing (make sure collection, instance and table have been specified in location list)")
  }

  if(is.character(location$instance) == FALSE | is.character(location$table) == FALSE | is.character(location$collection) == FALSE){
    stop("At least one input argument is not in character format. Check that specified parameters are in quotation marks")
  }

  # If there is a filter we need to put an &q in front to put in call, if a list need to flatten with appropriate syntax
  # paste will work on a single item
  if (filters[1] != "") {
    filterString <- paste(filters, collapse = ";")
    filterString <- paste0("&q=", filterString)
  }
  else{filterString <- ""
  }

  if (server == "uat"){
    baseUrl <- "https://epl-uat/statsnz-epl-data/api/v1/collections/"
  }
  else{
    baseUrl <- "https://epl-prd/statsnz-epl-data/api/v1/collections/"
  }

  # combine all to make the call
  if(length(variables) > 0){
    # Create the string for the call from the list of variables
    # This is fine for a single variable or already done string as it will leave it the same
    varString <- paste(variables, collapse = ",")
    theUrl <- paste0(baseUrl,
                     location$collection,
                     "/",
                     location$instance,
                     "/datasets/",
                     location$table,
                     "?columnList=",
                     varString,
                     filterString,
                     other)
  } else{
    theUrl <- paste0(baseUrl,
                     location$collection,
                     "/",
                     location$instance,
                     "/datasets/",
                     location$table,
                     filterString,
                     other)
  }
  # Write out to log and create some timing
  message(theUrl)
  start <- proc.time()

  #Error checking while loop - retries call
  attempt <- 0
  result <- NULL
  while(is.null(result) && attempt < 2 ) {
    attempt <- attempt + 1
    try (result <- httr::GET(url = theUrl, httr::use_proxy(""), httr::config(http_version = 2L), httr::config(ssl_verifypeer = 0L),
                             httr::authenticate("","", type ="gssnegotiate")))
  }

  # Check for error status - this should return the error code
  if(http_error(result))
  {
    errorMessage <- httr::content(result, "text", encoding = "UTF-8")
    message(errorMessage)
    #httr::stop_for_status(result, task = paste0("do DataStore API call: ", errorMessage))
    return(errorMessage)
  }
  httpend <- proc.time()
  httpdur <- httpend - start
  message("Total time for http call: ", sprintf("%g", httpdur["elapsed"]), "s")

  # Process the JSON - this seems to be the slow bit, so time separately.

  table <- jsonlite::fromJSON(httr::content(result, "text", encoding = "UTF-8"), bigint_as_char = TRUE)
  jsondur <- proc.time() - httpend

  # Log to output and then return value.

  message("Total time for JSON parse: ", sprintf("%g", jsondur["elapsed"]), "s")
  numRows <- nrow(table)
  numCols <- ncol(table)
  message("Table returned with ", numRows, " rows and ", numCols, " columns")

  table
}
