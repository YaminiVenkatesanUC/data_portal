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
    self$values <- as.data.table((data[colnames(data) != "Parameter"]))
    self$value_names <- value_names
    self$update_date <- update_date
  },
  get_csv_content = function(date_range = NULL) {
    output <- cbind(self$dates, self$values)
    names(output) <- c("parameter", self$value_names)
    if (!is.null(date_range)) {
      output <- output[parameter >= date_range[[1]] & parameter <= date_range[[2]]]
    }
    result <- melt(output, measure = 2:ncol(output), value.name = c("value"), variable.name = "sub_series_name")
    return (result)
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
  "BarChart" = BarChart
)