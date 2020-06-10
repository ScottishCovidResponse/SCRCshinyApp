# library(ggplot2)
# library(magrittr)
# library(rgdal)
# library(SCRCdataAPI)
# library(spdplyr)
# library(shinycssloaders)

# source("preload_data.R", local = TRUE)
source("frontpage.R", local = TRUE)
source("deaths-involving-coronavirus-covid-19.R", local = TRUE)
# source("demographics.R", local = TRUE)


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
                uiOutput("boxes.stackeddateplots"),
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
                box(title = datasets %>%
                      filter(dataset == "all.5years.dat") %$% title ,
                    width = 12,
                    status = "info", solidHeader = TRUE,
                    plotlyOutput("all.5years.dat")
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
    lapply(seq_along(multiline.plots$dataset), function(x) {
      box(renderPlotly(plot_linedate(data = get(multiline.plots$dataset[x]),
                                     groupby = multiline.plots$groupby[x],
                                     n = input[[paste0("topn", x)]])),
          selectInput(inputId = paste0("topn", x), label = "Plot:",
                      choices = c("Top 5", "Top 10", "All"),
                      selected = "Top 5"),
          title = multiline.plots$title[x],
          width = 12,
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # Line plots
  output$boxes.lineplots <- renderUI({
    lapply(seq_along(line.plots$dataset), function(x) {
      box(renderPlotly(get(paste0("g.", line.plots$dataset[x]))),
          title = line.plots$title[x],
          width = 12,
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # Stacked bar plots
  output$boxes.stackedplots <- renderUI({
    lapply(seq_along(stacked.plots$dataset), function(x) {
      box(renderPlotly(plot_stackedbar(data = get(stacked.plots$dataset[x]),
                                       sortby = input[[paste0("sortby", x)]])),
          selectInput(inputId = paste0("sortby", x), label = "Sort by",
                      choices = c("total", "carehome", "home",
                                  "hospital", "other"),
                      selected = "total"),
          title = stacked.plots$title[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # Stacked bar date plots
  output$boxes.stackeddateplots <- renderUI({
    lapply(seq_along(stackeddate.plots$dataset), function(x) {
      box(renderPlotly(plot_stackedbardate(data = get(stackeddate.plots$dataset[x]))),
          title = stackeddate.plots$title[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent")
    })
  })

  # 5-year average
  output$all.5years.dat <- renderPlotly({
    plot_linedate(all.5years.dat, "kdfjh")
  })

  # Other plots
  output$boxes.otherplots <- renderUI({
    lapply(seq_along(other.plots$dataset), function(x) {
      box(renderPlotly(get(paste0("g.", other.plots$dataset[x]))),
          title = other.plots$title[x],
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
