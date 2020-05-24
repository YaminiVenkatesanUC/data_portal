about_ui <- function(id) {  
  ns <- NS(id)
  actionButton(ns("show"), "About")
}

about_ui_mobile <- function(id) {  
  ns <- NS(id)
  actionButton(ns("show"), "About")
}

about_dialog_server <- function(input, output, session, most_recent_update_date) {
  aboutModal <- function(failed = FALSE, failedUnique = FALSE) {
    ns <- session$ns
    modalDialog(
      size = "l",
      easyClose = TRUE,
      span(h3(strong(paste(CONFIG$title, 'data portal')))),
      p(paste("Last updated:", get_most_recent_update_date(DATA_STORE))),
      includeHTML(CONFIG$about_modal_html),
      footer = tagList(
          modalButton("Close")
      )
    )
  }

  observeEvent(input$show, {
    showModal(aboutModal())
  })

  observeEvent(input$ok, {
    okay <- TRUE
    if (okay) {
      removeModal()
    } else if (isUnique) {
      showModal(dataModal(failedUnique = TRUE))
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
}