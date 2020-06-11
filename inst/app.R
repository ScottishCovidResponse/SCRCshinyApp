# library(ggplot2)
# library(magrittr)
# library(rgdal)
# library(SCRCdataAPI)
# library(spdplyr)
# library(shinycssloaders)

# source("preload_data.R", local = TRUE)
# source("frontpage.R", local = TRUE)
source("deaths-involving-coronavirus-covid-19.R", local = TRUE)
# source("demographics.R", local = TRUE)


# User interface ----------------------------------------------------------

library(shinythemes)

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "SCRC dashboard"),

  shinydashboard::dashboardSidebar(
    shinydashboard:: sidebarMenu(
      # menuItem("Covid-19 overview", tabName = "tab1",
      #          icon = icon("dashboard")),
      shinydashboard::menuItem("deaths-involving-corona..",
                               tabName = "tab2",
                               icon = icon("dashboard"), badgeLabel = "new",
                               badgeColor = "green")
      # ,
      # menuItem("demographics.h5", tabName = "tab3",
      #          icon = icon("dashboard"), badgeLabel = "new",
      #          badgeColor = "green")
    )
  ),

  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # First tab content -------------------------------------------------
      shinydashboard::tabItem(
        tabName = "tab1",
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("totalbox", width = 2),
          shinydashboard::valueBoxOutput("deathbox", width = 2),
          shinydashboard::valueBoxOutput("currentbox", width = 2),
          shinydashboard::valueBoxOutput("recoveredbox", width = 2)
        )
      ),

      # Second tab content ------------------------------------------------
      shinydashboard::tabItem(
        tabName = "tab2",
        shiny::uiOutput("boxes.stackeddateplots"),
        shiny::uiOutput("boxes.comparegender.plots"),

        shiny::fluidRow(
          shiny::uiOutput("boxes.lineplots"),
          shiny::uiOutput("boxes.sparktables"),
          shiny::uiOutput("boxes.stackedplots")
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
          # shinydashboard::box(title = datasets %>%
          #                       dplyr::filter(dataset == "all.5years.dat") %$% title ,
          #                     width = 12,
          #                     status = "info", solidHeader = TRUE,
          #                     plotly::plotlyOutput("all.5years.dat")
          # )


        )
      ),

      # Third tab content ------------------------------------------------
      shinydashboard::tabItem(
        tabName = "tab3",
        shiny::fluidRow(
          shinydashboard::box(
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

  output$totalbox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = latest.total,
      subtitle = "Total cases",
      icon = icon("heart"),
      color = "yellow"
    )
  })

  output$deathbox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = total.deaths,
      subtitle = "Confirmed deaths",
      icon = icon("heartbeat"),
      color = "yellow"
    )
  })

  output$currentbox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = latest.current,
      subtitle = "Current infected",
      icon = icon("ambulance")
    )
  })

  output$recoveredbox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = latest.recovered,
      subtitle = "Recovered",
      icon = icon("heart-o")
    )
  })


  # Second tab --------------------------------------------------------------

  # Gender plots
  output$boxes.lineplots <- shiny::renderUI({
    lapply(seq_along(gender.plots$dataset), function(x) {
      shinydashboard::box(
        plotly::renderPlotly(plot_linedate(
          data = get(gender.plots$dataset[x]),
          legend = "Gender")),
        shiny::div(gender.plots$location[x],
                   style = "color:grey; text-align:right"),
        title = gender.plots$title[x],
        width = 12,
        collapsible = TRUE,
        status = "info",
        solidHeader = TRUE,
        bg = "transparent"
      )
    })
  })

  # Sparkline tables
  output$boxes.sparktables <- shiny::renderUI({
    lapply(seq_along(multiline.plots$covid_deaths), function(x) {
      shinydashboard::box(
        reactable::renderReactable(table_reactable(
          covid_dat = get(multiline.plots$covid_deaths[x]),
          all_dat = get(multiline.plots$all_deaths[x]))),
        shiny::div(paste(multiline.plots$loc_c[x], "and",
                         multiline.plots$loc_a[x]),
                   style = "color:grey; text-align:right"),
        title = multiline.plots$title[x],
        width = 12,
        collapsible = TRUE,
        status = "info",
        solidHeader = TRUE,
        bg = "transparent"
      )
    })
  })

  # Stacked bar plots
  output$boxes.stackedplots <- shiny::renderUI({
    lapply(seq_along(stacked.plots$dataset), function(x) {
      shinydashboard::box(
        plotly::renderPlotly(plot_stackedbar(
          data = get(stacked.plots$dataset[x]),
          sortby = input[[paste0("sortby", x)]])),
        shiny::selectInput(inputId = paste0("sortby", x),
                           label = "Sort by",
                           choices = c("total", "carehome", "home",
                                       "hospital", "other"),
                           selected = "total"),
        shiny::div(stacked.plots$location[x],
                   style = "color:grey; text-align:right"),
        title = stacked.plots$title[x],
        collapsible = TRUE,
        status = "info",
        solidHeader = TRUE,
        bg = "transparent"
      )
    })
  })

  # Stacked bar date plots
  output$boxes.stackeddateplots <- shiny::renderUI({
    lapply(seq_along(stackeddate.plots$dataset), function(x) {
      shiny::fluidRow(
        shinydashboard::box(
          plotly::renderPlotly(plot_stackedbardate(
            data = get(stackeddate.plots$dataset[x]))),
          shiny::div(stackeddate.plots$location[x],
                     style = "color:grey; text-align:right"),
          width = 9,
          title = stackeddate.plots$title[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent"
        ),
        shinydashboard::box(
          plotly::renderPlotly(plot_donut(
            data = get(stackeddate.plots$dataset[x]))),
          shiny::br(),
          width = 3,
          title = "Location",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent"
        )
      )
    })
  })


  # Compare genders
  output$boxes.comparegender.plots <- shiny::renderUI({
    lapply(seq_along(stackeddate.plots$dataset), function(x) {
      shiny::fluidRow(
        shinydashboard::box(
          plotly::renderPlotly(plot_stackedbardate(
            data = rbind.data.frame(
              get(comparegender.plots$female_deaths[x]),
              get(comparegender.plots$male_deaths[x])) %>%
              dplyr::mutate(rownames = c("Female", "Male")) %>%
              tibble::column_to_rownames("rownames"))),
          shiny::div(paste(comparegender.plots$loc_f[x], "and",
                           comparegender.plots$loc_m[x]),
                     style = "color:grey; text-align:right"),
          width = 9,
          title = comparegender.plots$title[x],
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent"
        ),
        shinydashboard::box(
          plotly::renderPlotly(plot_donut(
            data = rbind.data.frame(
              get(comparegender.plots$female_deaths[x]),
              get(comparegender.plots$male_deaths[x])) %>%
              dplyr::mutate(rownames = c("Female", "Male")) %>%
              tibble::column_to_rownames("rownames"))),
          shiny::br(),
          width = 3,
          title = "Location",
          collapsible = TRUE,
          status = "info",
          solidHeader = TRUE,
          bg = "transparent"
        )
      )
    })
  })


  # 5-year average
  output$all.5years.dat <- plotly::renderPlotly({
    plot_linedate(all.5years.dat, "kdfjh")
  })


  # Third tab --------------------------------------------------------------
  # output$mymap <- leaflet::renderLeaflet({
  #   # Generate map
  #   map
  #   # Add legend to the map
  #   mapLegend
  #
  # })
}

shiny::shinyApp(ui = ui, server = server)
