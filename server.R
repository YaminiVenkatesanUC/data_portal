server <- function(input, output) {
  lapply(1:length(INDICATOR_CLASSES), function(i) {
    class_name <- gsub(" ", "_", INDICATOR_CLASSES[[i]])
    callModule(main_plot_server, paste0("main_plot_", class_name), INDICATOR_CLASSES[[i]])
  })
  callModule(
    download_data_server,
    "download_data"
  )
  callModule(about_dialog_server, "about_dialog", get_most_recent_update_date(data_store))
}