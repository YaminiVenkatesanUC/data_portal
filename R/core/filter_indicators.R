filter_indicator <- function(indicator, regions) {
  for (region in regions) {
    len_indicators <- length(
      grep(paste0("\\<", region, "\\>"), indicator$indicator_name, ignore.case = TRUE)
    )
    if (len_indicators > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

filter_group <- function(group, regions) {
  for (region in regions) {
    len_indicators <- length(grep(
      paste0("\\<", region, "\\>"), paste(group$title, group$name, collapse = " "),
      ignore.case = TRUE
      )
    )
    if (len_indicators > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

filter_indicators_by_region <- function(indicator_definitions, regions) {
  indicator_definitions

  output <- list()
  for (region in regions) {
    regions <- c(regions, as.character(FILTER_DICTIONARY[[region]]$synonyms))
  }

  for (key in names(indicator_definitions)) {
    if (filter_indicator(indicator_definitions[[key]], regions)) {
      output[[key]] <- indicator_definitions[[key]]
    } else {
      groups <- indicator_definitions[[key]]$groups

      selection <- as.vector(sapply(
        indicator_definitions[[key]]$groups,
        function(x) filter_group(x, regions)
      ))
      if (TRUE %in% selection) {
        output[[key]] <- indicator_definitions[[key]]
        output[[key]]$groups <- groups[selection]
      }
    }
  }

  return(output)
}
