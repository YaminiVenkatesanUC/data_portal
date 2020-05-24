old_indicators <- read_json("indicators.json")

remove_endash <- function(x) {
  y <- gsub(" \U2012 ", " - ", x)
  return (y)
}

add_field <- function(field, new_item, old_item, transform = function(x) x) {
  if (length(old_item[[field]]) > 0) {
    new_item[[field]] <- transform(old_item[[field]])
  }
  return (new_item)
}

new_indicators <- list()
new_data_definitions <- list()

for (i in 1:length(old_indicators)) {
  item <- old_indicators[[i]]
  new_indicator_definition <- list()
  new_indicator_definition <- add_field("class", new_indicator_definition, item, transform = remove_endash)
  new_indicator_definition <- add_field("indicator_name", new_indicator_definition, item, transform = remove_endash)
  new_indicator_definition <- add_field("type", new_indicator_definition, item, transform = remove_endash)
  new_indicator_definition <- add_field("source", new_indicator_definition, item, transform = remove_endash)
  new_indicator_definition <- add_field("plot_function", new_indicator_definition, item)
  new_indicator_definition <- add_field("international", new_indicator_definition, item)
  new_indicator_definition <- add_field("include_date_slider", new_indicator_definition, item)
  new_indicator_definition <- add_field("source_url", new_indicator_definition, item)
  new_indicator_definition <- add_field("download", new_indicator_definition, item)
  new_indicator_definition <- add_field("description", new_indicator_definition, item, transform = remove_endash)
  new_indicator_definition <- add_field("caveat", new_indicator_definition, item, transform = remove_endash)

  new_data_definition <- list()
  new_data_definition <- add_field("class", new_data_definition, item, transform = remove_endash)
  new_data_definition <- add_field("indicator_name", new_data_definition, item, transform = remove_endash)
  new_data_definition <- add_field("type", new_data_definition, item, transform = remove_endash)
  
  new_data_definition <- add_field("parameter_col", new_data_definition, item)
  new_data_definition <- add_field("parameter_transform", new_data_definition, item)
  new_data_definition <- add_field("date_filter", new_data_definition, item)
  new_data_definition <- add_field("sheet_number", new_data_definition, item)
  new_data_definition <- add_field("value_col", new_data_definition, item)
  new_data_definition <- add_field("value_names", new_data_definition, item)
  new_data_definition <- add_field("drop_na", new_data_definition, item)
  new_data_definition <- add_field("filename", new_data_definition, item)
  new_data_definition <- add_field("load_function", new_data_definition, item)
  new_data_definition <- add_field("skip", new_data_definition, item)
  new_data_definition <- add_field("country_filter", new_data_definition, item)
  new_data_definition <- add_field("region", new_data_definition, item)
  
  new_data_definition$data_type <- "TimeSeries"
  
  
  if (length(item$title) < length(item$units)) {
    item$title <- rep(item$title, length(item$units))
  }
  

  if (length(item$units) == length(item$value_names)) {
    groups <- list()
    for (j in 1:length(item$units)) {
      group <- list(
        name = item$value_names[[j]],
        title = item$title[[j]],
        units = item$units[[j]]
      )
      groups[[j]] <- group
    }
    new_data_definition$group_names <- item$value_names
  } else if (length(units) == 1) {
    groups <- list()
    groups[[1]] <- list(
      name = "undefined_name",
      title = item$title[[1]],
      units = item$unit[[1]]
    )
    new_data_definition$group_names <- "undefined_name"
  }

  new_indicator_definition$groups <- I(groups)
  new_indicators[[i]] <- new_indicator_definition
  new_data_definitions[[i]] <- new_data_definition
  
  
  
  
  
  
}

write(toJSON(new_indicators, pretty = TRUE, auto_unbox = TRUE), "converted_indicators.json")
write(toJSON(new_data_definitions, pretty = TRUE, auto_unbox = TRUE), "converted_data_definitions.json")

