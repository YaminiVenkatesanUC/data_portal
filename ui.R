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
     "<!-- Google Tag Manager -->
       <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
         new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
         j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
         'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
         })(window,document,'script','dataLayer','GTM-WT885QT');</script>
     <!-- End Google Tag Manager -->"
   )),
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
        header.append('", createHeaderButton("Download all data", 10, "download_data-show", "btn-modal"), "');
        header.append('", createHeaderButton("About", 170, "about_dialog-show", "btn-details"), "');"
    )
  )
)