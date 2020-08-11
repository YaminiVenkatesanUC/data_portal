TimeSeries <- R6Class("TimeSeries", list(
  dates = NULL,
  values = NULL,
  value_names = NULL,
  update_date = NULL,
  initialize = function(...) {
    arguments <- list(...)
    if (nargs() == 3) {
      self$initialize_from_args(...)
    }
  },
  initialize_from_args = function(data, value_names, update_date) {
    self$dates = data$Parameter
    self$values <- as.data.frame((data[colnames(data) != "Parameter"]))
    self$value_names <- value_names
    self$update_date <- update_date
  },
  get_csv_content = function() {
    output <- cbind(self$dates, self$values)
    names(output) <- c("parameter", self$value_names)
    output <- pivot_longer(output, cols = 2:ncol(output), names_to = "sub_series_name") %>%
      mutate(
        parameter = format(parameter, "%d-%m-%y")
      )
    return (output)
  }
))

TimeSeriesError <- R6Class("TimeSeriesError", list(
  dates = NULL,
  values = NULL,
  upper = NULL,
  lower = NULL,
  value_names = NULL,
  update_date = NULL,
  initialize = function(...) {
    arguments <- list(...)
    self$initialize_from_args(...)
    
  },
  initialize_from_args = function(data, value_names, update_date) {
    self$dates = data$Parameter
    self$values <- as.data.frame(
      (data[colnames(data) != "Parameter" &
              !endsWith(colnames(data), "_lower") &
              !endsWith(colnames(data), "_upper")]))
    self$lower = as.data.frame(data[endsWith(colnames(data), "_lower")]) 
    self$upper = as.data.frame(data[endsWith(colnames(data), "_upper")])  
    self$value_names <- value_names
    self$update_date <- update_date
  },
  get_csv_content = function() {
    output <- cbind(self$dates, self$values, self$lower, self$upper)
    names(output) <- c("parameter", self$value_names, paste0(self$value_names, "_lower"), paste0(self$value_names, "_upper"))
    output <- pivot_longer(output, cols = 2:ncol(output), names_to = "sub_series_name")  %>%
      mutate(
        parameter = format(parameter, "%d-%m-%y"))
    
    return (output)
  }
))

BarChart <- R6Class("BarChart", list(
  categories = NULL,
  values = NULL,
  value_names = NULL,
  update_date = NULL,
  initialize = function(...) {
    arguments <- list(...)
    if (nargs() == 3) {
      self$initialize_from_args(...)
    }
  },
  initialize_from_args = function(data, value_names, update_date) {
    self$categories = data$Parameter
    self$values <- as.data.frame((data[colnames(data) != "Parameter"]))
    self$value_names <- value_names
    self$update_date <- update_date
  },
  get_csv_content = function() {
    output <- cbind(self$categories, self$values)
    names(output) <- c("parameter", self$value_names)
    output <- pivot_longer(output, cols = 2:ncol(output), names_to = "sub_series_name")
    return (output)
  }
))

`+.TimeSeries` = function(x, y) {
  if (length(setdiff(x$date, y$date)) > 0) {
    stop("Can't combine time series data with different dates")
  }
  result <- TimeSeries$new()
  result$dates <- x$date
  result$values <- cbind(x$values, y$values)
  result$value_names <- c(x$value_name, y$value_names)
  result$update_date <- x$update_date
  
  return (result) 
}

`+.BarChart` = function(x, y) {
  if (length(setdiff(x$categories, y$categories)) > 0) {
    stop("Can't combine bar charts with different categories")
  }
  result <- BarChart$new()
  result$categories <- x$categories
  result$values <- cbind(x$values, y$values)
  result$value_names <- c(x$value_name, y$value_names)
  result$update_date <- x$update_date
  return (result) 
}

DATA_TYPES <- list(
  "TimeSeries" = TimeSeries,
  "TimeSeriesError" = TimeSeriesError,
  "BarChart" = BarChart
)