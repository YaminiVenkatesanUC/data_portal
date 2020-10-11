check_data_definition <- function(x) {
  req_fields <- DATA_DEFINITION_FIELDS[!(DATA_DEFINITION_FIELDS %in% names(x))]
  if (length(req_fields) > 0) {
    stop(
      paste(
        "Load failed - data definition is missing fields:",
        paste(req_fields, collapse = " , "),
        "\n",
        "Details provided were:",
        paste(x$class, collapse = " , "),
        paste(x$type, collapse = " , "),
        paste(x$indicator_name, collapse = " , ")
      )
    )
  }
}

check_indicator_definition <- function(x) {
  req_fields <- INDICATOR_DEFINITION_FIELDS[!(INDICATOR_DEFINITION_FIELDS %in% names(x))]
  if (length(req_fields) > 0) {
    stop(
      paste(
        "Load failed - indicator is missing fields:",
        paste(req_fields, collapse = " , "),
        "\n",
        "Details provided were:",
        paste(x$class, collapse = " , "),
        paste(x$type, collapse = " , "),
        paste(x$indicator_name, collapse = " , ")
      )
    )
  }
}

check_config_file <- function(x) {
  req_fields <- CONFIG_FIELDS[!(CONFIG_FIELDS %in% names(x))]
  if (length(req_fields) > 0) {
    stop(
      paste(
        "Config file is missing the following fields:",
        paste(req_fields, collapse = " , "),
        "\n",
        "Please fill these in first - see documentation for details."
      )
    )
  }
}

check_api_parameters <- function(x) {
  req_fields <- DATA_API_FIELDS[!(DATA_API_FIELDS %in% names(x))]
  if (length(req_fields) > 0) {
    warning(
      paste(
        "Data request is missing the following fields:",
        paste(req_fields, collapse = " , "),
        "\n",
        "Please fill these in first - see documentation for details."
      )
    )
    return(FALSE)
  }
  return(TRUE)
}
