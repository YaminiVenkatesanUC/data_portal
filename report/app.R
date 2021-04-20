#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
setwd("~/data_portal")
#source("scripts/dev/reports/report.R")

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Monthly report"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Performance", tabName = "performance", icon = icon("dashboard")),
            menuItem("Summary", tabName = "summary", icon = icon("th")),
            menuItem("Indicators", tabName = "indicators", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "performance"),
            tabItem(tabName = "summary",
                    fluidRow(
                        box(plotlyOutput("plot2"))
                        )
                    ),
            tabItem(tabName = "indicators",
        fluidRow(
            box(plotlyOutput("plot1", height = 250)),

            box(
                title = "Indicator",
                selectInput("indicator", label = "Select indicator:", choices = unique(df_indicators$indicator_name), selected = unique(df_indicators$indicator_name)[1])
            )
        )
            )
    )
)
)


server <- function(input, output) {
    output$plot1 <- renderPlotly({
        data <- df_ind_views %>%
            filter(indicator_name == input$indicator)
        plot_ly(
            x = data$month,
            y = data$ind_views,
            type = "bar",
            name = "Monthly views"
        )
    })
    output$plot2 <- renderPlotly({
        y_vis<- list(
            tickfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Montly visits"
        )

        fig1 <- plot_ly(
            x = df_events$month,
            y = df_events$ind_count_cum,
            type = "bar",
            name = "Number of indicators"
        )

        fig1 <- fig1 %>% add_lines(x = df_events$month, y = df_events$views_count, name = "Monthly visits", yaxis = "y2")
        fig1 <- fig1 %>% add_lines(x = df_events$month, y = df_events$download_count, name = "Monthly downloads", yaxis = "y2")
        fig1 <- fig1 %>% layout(title = "Number of indicators overlayed wth monthly visits",
                                yaxis2 = y_vis,
                                xaxis = list(title = "Month"))

        fig1
    })
}

# Run the application
shinyApp(ui = ui, server = server)
