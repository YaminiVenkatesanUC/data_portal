library(dplyr)
#' Fetch data from TSM
#'
#' Fetches data for a single TSM series
#'
#' @param conn connection to TSM. Will be created if not provided
#' @param pattern a single TSM pattern
#' @param timeref time reference period to extract
#' @param ready ready STATUS of cells to extract
#' @param suppress suppress = "y" will return all data
#' @param round roud = "y" will return rounded data
#' @param believeNRows the number of rows cannot be trusted
#' @param details set to TRUE to keep extra metadata details
#'
#'
#' @return Data frame of TSM data
#'
#' @examples
#' tsm_conn <- get_connection_to_tsm()
#'
#' tsm_fetch_single(conn = tsm_conn, pattern = "SNEQ.SG01RSC00B01")
tsm_fetch_single <- function (conn,
                              pattern,
                              timeref = "A",
                              ready = "n",
                              suppress = "n",
                              round = "n",
                              believeNRows = FALSE,
                              details = FALSE){
  if (!require(RODBC)) stop("Please install RODBC package and try again.")
  if (!inherits(conn, "RODBC")) stop(sprintf("%s is not a database connection.", deparse(substitute(conn))))
  if (!RODBC:::odbcValidChannel(conn)) stop(sprintf("%s is not an open database connection.", deparse(substitute(conn))))
  odbcQuery(conn, "use tsm_prod")
  query <- paste("exec fetch_cells_x @series_selection_text='",
                 pattern, "',@time_selection_text=", timeref, ",@only_ready_ind=",
                 ready, ",@suppressed_ind=", suppress, ",@rounded_ind=",
                 round, sep = "")
  res <- sqlQuery(conn, query, as.is = TRUE, believeNRows = believeNRows)

  if(!details) res <- res %>% select(series_ref_text, time_ref, data_val_nbr, status_code, suppression_ind)

  return(res)
}


#' Fetch data from TSM
#'
#' Fetches data for a list of patterns from TSM
#'
#' @param conn connection to TSM. Will be created if not provided
#' @param pattern a list or vector of TSM patterns
#' @param timeref time reference period to extract
#' @param ready ready STATUS of cells to extract
#' @param suppress suppress = "y" will return all data
#' @param round roud = "y" will return rounded data
#' @param believeNRows the number of rows cannot be trusted
#' @param details set to TRUE to keep extra metadata details
#'
#'
#' @return Data frame of TSM data
#'
#' @examples
# tsm_conn <- get_connection_to_tsm()
#'



tsm_fetch <- function(conn = get_connection_to_tsm(),
                      pattern,
                      timeref = "A",
                      ready = "n",
                      suppress = "n",
                      round = "n",
                      believeNRows = FALSE,
                      details = FALSE){

  for (p in pattern) {
    df <- tsm_fetch_single(conn = conn,
                                   pattern = p,
                                   timeref = timeref,
                                   ready = ready,
                                   suppress = suppress,
                                   round = round,
                                   believeNRows = believeNRows,
                                   details = details)

    if (exists("tsm_all")) {
      tsm_all <- rbind(tsm_all, df)
    } else {
      tsm_all <- df
    }
  }

  return(tsm_all)

}

#' Initialise relevant packages and create a connection to the TSM
#'
#' Loads relevant R packages (RODBC) and creates a connection variable to TSM
#'
#' @param name usename to use for login. Defaults to session username
#' @param server server to connect to. Defaults to Sybase03
#' @param port port to connect on. Defaults to 5000
#' @param db database to connect to within servr. Defaults to tsm_prod
#'
#' @return Connection to TSM object
#'
#' @examples
#' tsm_conn <- get_connection_to_tsm()
#'
#' df <- tsm_fetch(conn = tsm_conn, pattern = "SNEQ.SG01RSC00B01")
#'
#' @export
get_connection_to_tsm <- function (name = Sys.info()["user"], server = "wprdsyb03.stats.govt.nz",
                     port = 5000, db = "tsm_prod")
{

  if(!("RODBC" %in% installed.packages())) {
    install.packages("RODBC")
  }

  library(RODBC)

  channel <- RODBC::odbcDriverConnect(sprintf("Driver=Sybase;Server=%s;Port=%s;Database=%s;UID=%s;PWD=%s",
                                              server, port, db, name, rstudioapi::askForPassword(paste0(server,
                                                                                                        " password:"))), believeNRows = FALSE)
  if (channel == -1) {
    print("Invalid connection")
  }
  else {
    cat("Valid connection\n")
    cat("Connected to", substr(server, 5, 9), db, "\n")
    return(channel)
  }
}
