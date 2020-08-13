ui <- fluidPage(
  theme = "bootstrap.css",
  tags$head(
    tags$style(HTML(paste0(
      ":root {--custom-color: ",
      CONFIG$primary_color,
      ";}"
  )
  )
  )),
   tags$head(HTML(
     tag_manager_html
   )),
  tags$head(tags$script("
        Shiny.addCustomMessageHandler('updateSelections',
            function(data) {
                var tab_ref = ' a:contains(\"' + data.tab + '\")';
                $(tab_ref).tab('show');
            }
        )
    ")),
  useShinyjs(),
  tags$head(tags$style(".modal-dialog{ width:100%}")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$style(HTML("hr {border-top: 1px solid #4d5b61;}"))
  ),
  tags$head(tags$style(HTML('.irs-bar {
        background: var(--custom-color);
        border-top: 1px solid var(--custom-color);
        border-bottom: 1px solid var(--custom-color);}
        .irs-from, .irs-to, .irs-single { background: var(--custom-color)}'
  ))
  ),
  tags$body(HTML(
    "<!-- Google Tag Manager (noscript) -->
      <noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-T478LP4'
        height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>
    <!-- End Google Tag Manager (noscript) -->"
  )),
  mainPanel(
    width = 12, 
    div(
      class = "navbar1",
      id="navbarid",
      do.call(navbarPage, tabs)
    )
  ),
  tags$script(
    HTML(
      "var header = $('.navbar-nav');
        header.append('", createHeaderButton("Download data", 10, "download_data-show", "btn-modal"), "');
        header.append('", createHeaderButton("About", 170, "about_dialog-show", "btn-details"), "');"
    )
  )
)