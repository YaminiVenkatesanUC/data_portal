get_tab_panel <- function(indicator_classes, index) {
  if (index > length(indicator_classes)) {
    return(NULL)
  }
  class_name <- indicator_classes[[index]]
  if (paste0(gsub(" ", "_", tolower(class_name)), ".html") %in% list.files("www/")) {
    html_file <- paste0("www/", paste0(gsub(" ", "_", tolower(class_name)), ".html"))
  } else {
    html_file <- NULL
  }

  if (!is.null(html_file)) {
    footer_content <- includeHTML(html_file)
  } else {
    footer_content <- HTML("<div></div>")
  }
  if (!is.null(class_name)) {
    return(
      tabPanel(
        class_name,
        fluidRow(
          column(
            12,
            top_panel_ui(paste0("main_plot_", gsub(" ", "_", class_name)), class_name)
          ),
          column(
            10,
            fluidRow(
              main_plot_ui(paste0("main_plot_", gsub(" ", "_", class_name)))
            ),
            offset = 1
          )
        ),
        width = 12,
        fluidRow(
          footer_content,
          width = 10,
          offset = 1
        )
      )
    )
  }
}
