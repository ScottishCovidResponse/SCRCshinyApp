library(dplyr)
library(ggplot2)
library(hdf5r)
library(leaflet)
library(magrittr)
library(rgdal)
library(SCRCdataAPI)
library(SCRCshinyApp)
library(SPARQL)
library(spdplyr)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)


source("exec/deaths-involving-coronavirus-covid-19.R", local = TRUE)
source("exec/leafletapp.R", local = TRUE)


# User interface ----------------------------------------------------------

library(shinythemes)

ui <- dashboardPage(
  dashboardHeader(title = "SCRC dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Covid-19 overview", tabName = "tab1",
               icon = icon("dashboard")),
      menuItem("deaths-involving-coronavirus-covid-19.h5", tabName = "tab2",
               icon = icon("dashboard"), badgeLabel = "new",
               badgeColor = "green"),
      menuItem("demographics.h5", tabName = "tab3",
               icon = icon("dashboard"), badgeLabel = "new",
               badgeColor = "green")
    )
  ),

  dashboardBody(
    tabItems(
      # First tab content -------------------------------------------------
      tabItem(tabName = "tab1",
              fluidRow(
                valueBoxOutput("totalbox", width = 2),
                valueBoxOutput("deathbox", width = 2),
                valueBoxOutput("currentbox", width = 2),
                valueBoxOutput("recoveredbox", width = 2)
              )
      ),

      # Second tab content ------------------------------------------------
      tabItem(tabName = "tab2",
              fluidRow(
                uiOutput("boxes.multilineplots"),
                uiOutput("boxes.lineplots"),
                uiOutput("boxes.stackedplots"),

                # # Stacked bar plots
                # box(title = "stacked",
                #     status = "info", solidHeader = TRUE,
                #     plotOutput("covid.councilloc.dat"),
                #     selectInput(inputId = "sortby", label = "Sort by",
                #                 choices = c("total", "carehome", "home",
                #                             "hospital", "other"),
                #                 selected = "total")
                # ),

                # Averaged over last 5 years
                box(title = "all.5years.dat",
                    status = "info", solidHeader = TRUE,
                    plotOutput("all.5years.dat")
                ),

                # Other plots
                uiOutput("boxes.otherplots")
              )
      ),

      # Third tab content ------------------------------------------------
      tabItem(tabName = "tab3",
              fluidRow(
                box(
                  shinycssloaders::withSpinner(leaflet::leafletOutput("mymap")),
                  shiny::p()
                )
              )
      )
    )
  )
)


server <- function(input, output) {

  # First tab ---------------------------------------------------------------

  output$totalbox <- renderValueBox({
    valueBox(
      value = latest.total,
      subtitle = "Total cases",
      icon = icon("heart"),
      color = "yellow"
    )
  })

  output$deathbox <- renderValueBox({
    valueBox(
      value = total.deaths,
      subtitle = "Confirmed deaths",
      icon = icon("heartbeat"),
      color = "yellow"
    )
  })

  output$currentbox <- renderValueBox({
    valueBox(
      value = latest.current,
      subtitle = "Current infected",
      icon = icon("ambulance")
    )
  })

  output$recoveredbox <- renderValueBox({
    valueBox(
      value = latest.recovered,
      subtitle = "Recovered",
      icon = icon("heart-o")
    )
  })


  # Second tab --------------------------------------------------------------

  # Multiline plots
  output$boxes.multilineplots <- renderUI({
    lapply(seq_along(multiline.plots), function(x) {
      box(renderPlot(plot_line(get(multiline.plots[x]),
                               input[[paste0("topn", x)]]),
                     bg = "transparent"),
          selectInput(inputId = paste0("topn", x), label = "Plot:",
                      choices = c("Top 5", "Top 10", "All"),
                      selected = "Top 5"),
          title = multiline.plots[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # Line plots
  output$boxes.lineplots <- renderUI({
    lapply(seq_along(line.plots), function(x) {
      box(renderPlot(get(paste0("g.", line.plots[x]))),
          title = line.plots[x],
          # width = 5,
          # style = "border:2px solid #666666;border-radius:20px;",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # Stacked bar plots
  output$boxes.stackedplots <- renderUI({
    lapply(seq_along(stacked.plots), function(x) {
      box(renderPlot(plot_stackedbar(get(stacked.plots[x]),
                                     input[[paste0("sortby", x)]]),
                     bg = "transparent"),
          selectInput(inputId = paste0("sortby", x), label = "Sort by",
                      choices = c("total", "carehome", "home",
                                  "hospital", "other"),
                      selected = "total"),
          title = stacked.plots[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # 5-year average
  output$all.5years.dat <- renderPlot({
    plot_line(all.5years.dat)
  }, bg = "transparent")

  # Other plots
  output$boxes.otherplots <- renderUI({
    lapply(seq_along(other.plots), function(x) {
      box(renderPlot(get(paste0("g.", other.plots[x]))),
          title = other.plots[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # Third tab --------------------------------------------------------------
  output$mymap <- leaflet::renderLeaflet({
    # Generate map
    map
    # Add legend to the map
    mapLegend

  })
}

shinyApp(ui = ui, server = server)
