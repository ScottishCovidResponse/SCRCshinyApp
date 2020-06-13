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


# User interface ------------------------------------------------------------

library(semantic.dashboard)

ui <- semantic.dashboard::dashboardPage(
  dashboardHeader(title = "SCRC dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("scotgov_deaths",
               tabName = "tab1",
               icon = icon("dashboard")),
      menuItem("scotgov_management", tabName = "tab1",
               icon = icon("dashboard"))
    )
  ),

  dashboardBody(
    tabItems(
      # First tab content ---------------------------------------------------
      tabItem(
        tabName = "tab1",

        # Row 12 ------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = all.plots$title[1],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    plotly::plotlyOutput("all.summary"),
                                    div(all.plots$location[1],
                                        style = "color:grey; text-align:right"),
                                    div(all.plots$location[3],
                                        style = "color:grey; text-align:right"),
                                    div(all.plots$location[2],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),


        # Row 1 -------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = multiline.plots$title[1],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    reactable::reactableOutput("sparkcouncil"),
                                    div(multiline.plots$loc_c[1],
                                        style = "color:grey; text-align:right"),
                                    div(multiline.plots$loc_a[1],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Row 2 -------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = multiline.plots$title[2],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    reactable::reactableOutput("sparknhs"),
                                    div(multiline.plots$loc_c[2],
                                        style = "color:grey; text-align:right"),
                                    div(multiline.plots$loc_a[2],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Row 3 -------------------------------------------------------------

        # Compare gender
        shiny::fluidRow(
          semantic.dashboard::box(width = 10,
                                  title = comparegender.plots$title[1],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 10,
                                    plotly::plotlyOutput("gender.all.stacked"),
                                    div(comparegender.plots$loc_f[1],
                                        style = "color:grey; text-align:right"),
                                    div(comparegender.plots$loc_m[1],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 6,
                                  title = "Gender",
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 6,
                                    plotly::plotlyOutput("gender.all.pie"),
                                    br(), br()
                                  )
          )
        ),

        # Row 4 -------------------------------------------------------------
        shiny::fluidRow(
          semantic.dashboard::box(width = 10,
                                  title = comparegender.plots$title[2],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 10,
                                    plotly::plotlyOutput("gender.covid.stacked"),
                                    div(comparegender.plots$loc_f[2],
                                        style = "color:grey; text-align:right"),
                                    div(comparegender.plots$loc_m[2],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 6,
                                  title = "Gender",
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 6,
                                    plotly::plotlyOutput("gender.covid.pie"),
                                    br(), br()
                                  )
          )
        ),

        # Row 5 -------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 10,
                                  title = stackeddate.plots$title[1],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 10,
                                    plotly::plotlyOutput("date.all.stacked"),
                                    div(stackeddate.plots$location[1],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 6,
                                  title = "Location",
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 6,
                                    plotly::plotlyOutput("date.all.pie"),
                                    br()
                                  )
          )
        ),

        # Row 6 -------------------------------------------------------------
        shiny::fluidRow(
          semantic.dashboard::box(width = 10,
                                  title = stackeddate.plots$title[2],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 10,
                                    plotly::plotlyOutput("date.covid.stacked"),
                                    div(stackeddate.plots$location[2],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 6,
                                  title = "Location",
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 6,
                                    plotly::plotlyOutput("date.covid.pie"),
                                    br()
                                  )
          )
        ),

        # Row 7 -------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = stacked.plots$title[1],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("stacked.all.council",
                                                         height = "700px"),
                                    shiny::selectInput(
                                      inputId = paste0("sortby", 1),
                                      label = "Sort by",
                                      choices = c("total", "carehome", "home",
                                                  "hospital", "other"),
                                      selected = "total"
                                    ),
                                    div(stacked.plots$location[1],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = stacked.plots$title[2],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("stacked.covid.council",
                                                         height = "700px"),
                                    shiny::selectInput(
                                      inputId = paste0("sortby", 2),
                                      label = "Sort by",
                                      choices = c("total", "carehome", "home",
                                                  "hospital", "other"),
                                      selected = "total"
                                    ),
                                    div(stacked.plots$location[2],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Row 8 -------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = stacked.plots$title[3],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("stacked.all.nhs"),
                                    shiny::selectInput(
                                      inputId = paste0("sortby", 3),
                                      label = "Sort by",
                                      choices = c("total", "carehome", "home",
                                                  "hospital", "other"),
                                      selected = "total"
                                    ),
                                    div(stacked.plots$location[3],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = stacked.plots$title[4],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("stacked.covid.nhs"),
                                    shiny::selectInput(
                                      inputId = paste0("sortby", 4),
                                      label = "Sort by",
                                      choices = c("total", "carehome", "home",
                                                  "hospital", "other"),
                                      selected = "total"
                                    ),
                                    div(stacked.plots$location[4],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Row 9 -------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = gender.plots$title[1],
                                  color = "pink",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("female.age.all"),
                                    div(gender.plots$location[1],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = gender.plots$title[2],
                                  color = "pink",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("female.age.covid"),
                                    div(gender.plots$location[2],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Row 10 ------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = gender.plots$title[3],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("male.age.all"),
                                    div(gender.plots$location[3],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = gender.plots$title[4],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("male.age.covid"),
                                    div(gender.plots$location[4],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Row 11 ------------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = gender.plots$title[5],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("persons.all"),
                                    div(gender.plots$location[5],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = gender.plots$title[6],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("persons.covid"),
                                    div(gender.plots$location[6],
                                        style = "color:grey; text-align:right")
                                  )
          )
        )






      )
    ),

    # Second tab ------------------------------------------------------------

    tabItem(
      tabName = "tab2",

      # Row 1 ---------------------------------------------------------------

      shiny::fluidRow(
        semantic.dashboard::box(width = 16,
                                title = "dsf",
                                color = "red",
                                ribbon = TRUE,
                                title_side = "top right",
                                semantic.dashboard::column(
                                  width = 16,
                                  "sdf"
                                )
        )
      )
    )
  )

)


server <- function(input, output) {

  # First tab ---------------------------------------------------------------

  # Row 1 -------------------------------------------------------------------

  one <- as.data.frame(t(get(all.plots$dataset[1]))) %>%
    tibble::rownames_to_column("date")
  colnames(one)[2] <- all.plots$dataset[1]

  two <- as.data.frame(t(get(all.plots$dataset[3]))) %>%
    tibble::rownames_to_column("date")
  colnames(two)[2] <- all.plots$dataset[3]

  df <- merge(one, two, by = "date", all = TRUE) %>%
    reshape2::melt(id.vars = "date") %>%
    reshape2::dcast(variable ~ date, value.var = "value") %>%
    tibble::column_to_rownames("variable")

  three <- get(all.plots$dataset[2])
  rownames(three) <- all.plots$dataset[2]

  output$all.summary <- plotly::renderPlotly(plot_summary(
    bars = df,
    line = three
  ))

  # Row 2 -------------------------------------------------------------------

  output$sparkcouncil <-  reactable::renderReactable(table_reactable(
    covid_dat = get(multiline.plots$covid_deaths[1]) %>%
      convert_areacodes(conversion.table),
    all_dat = get(multiline.plots$all_deaths[1]) %>%
      convert_areacodes(conversion.table)
  ))

  # Row 2 -------------------------------------------------------------------

  output$sparknhs <-  reactable::renderReactable(table_reactable(
    covid_dat = get(multiline.plots$covid_deaths[2]) %>%
      convert_areacodes(conversion.table),
    all_dat = get(multiline.plots$all_deaths[2]) %>%
      convert_areacodes(conversion.table)
  ))


  # Row 3 -------------------------------------------------------------------

  output$gender.all.stacked <- plotly::renderPlotly(plot_stackedbardate(
    data = rbind.data.frame(
      get(comparegender.plots$female_deaths[1]),
      get(comparegender.plots$male_deaths[1])) %>%
      dplyr::mutate(rownames = c("Female", "Male")) %>%
      tibble::column_to_rownames("rownames")
  ))

  output$gender.all.pie <- plotly::renderPlotly(plot_donut(
    data = rbind.data.frame(
      get(comparegender.plots$female_deaths[1]),
      get(comparegender.plots$male_deaths[1])) %>%
      dplyr::mutate(rownames = c("Female", "Male")) %>%
      tibble::column_to_rownames("rownames")
  ))

  # Row 4 -------------------------------------------------------------------

  output$gender.covid.stacked <- plotly::renderPlotly(plot_stackedbardate(
    data = rbind.data.frame(
      get(comparegender.plots$female_deaths[2]),
      get(comparegender.plots$male_deaths[2])) %>%
      dplyr::mutate(rownames = c("Female", "Male")) %>%
      tibble::column_to_rownames("rownames")
  ))

  output$gender.covid.pie <- plotly::renderPlotly(plot_donut(
    data = rbind.data.frame(
      get(comparegender.plots$female_deaths[2]),
      get(comparegender.plots$male_deaths[2])) %>%
      dplyr::mutate(rownames = c("Female", "Male")) %>%
      tibble::column_to_rownames("rownames")
  ))

  # Row 5 -------------------------------------------------------------------

  output$date.all.stacked <- plotly::renderPlotly(plot_stackedbardate(
    data = get(stackeddate.plots$dataset[1])
  ))

  output$date.all.pie <- plotly::renderPlotly(plot_donut(
    data = get(stackeddate.plots$dataset[1])))

  # Row 6 -------------------------------------------------------------------

  output$date.covid.stacked <- plotly::renderPlotly(plot_stackedbardate(
    data = get(stackeddate.plots$dataset[2])
  ))

  output$date.covid.pie <- plotly::renderPlotly(plot_donut(
    data = get(stackeddate.plots$dataset[2])
  ))

  # Row 7 -------------------------------------------------------------------

  output$stacked.all.council <- plotly::renderPlotly(plot_stackedbar(
    data = get(stacked.plots$dataset[1]) %>%
      convert_areacodes(conversion.table),
    sortby = input[[paste0("sortby", 1)]]
  ))

  # Row 8 -------------------------------------------------------------------

  output$stacked.covid.council <- plotly::renderPlotly(plot_stackedbar(
    data = get(stacked.plots$dataset[2]) %>%
      convert_areacodes(conversion.table),
    sortby = input[[paste0("sortby", 2)]]
  ))

  # Row 9 -------------------------------------------------------------------

  output$stacked.all.nhs <- plotly::renderPlotly(plot_stackedbar(
    data = get(stacked.plots$dataset[3]) %>%
      convert_areacodes(conversion.table),
    sortby = input[[paste0("sortby", 3)]]
  ))

  # Row 10 ------------------------------------------------------------------

  output$stacked.covid.nhs <- plotly::renderPlotly(plot_stackedbar(
    data = get(stacked.plots$dataset[4]) %>%
      convert_areacodes(conversion.table),
    sortby = input[[paste0("sortby", 4)]]
  ))

  # Row 11 ------------------------------------------------------------------

  output$female.age.all <- plotly::renderPlotly(plot_linedate(
    data = get(gender.plots$dataset[1]),
    legend = "Age group"))

  output$female.age.covid <- plotly::renderPlotly(plot_linedate(
    data = get(gender.plots$dataset[2]),
    legend = "Age group"))

  output$male.age.all <- plotly::renderPlotly(plot_linedate(
    data = get(gender.plots$dataset[3]),
    legend = "Age group"))

  output$male.age.covid <- plotly::renderPlotly(plot_linedate(
    data = get(gender.plots$dataset[4]),
    legend = "Age group"))

  output$persons.all <- plotly::renderPlotly(plot_linedate(
    data = get(gender.plots$dataset[5]),
    legend = "Age group"))

  output$persons.covid <- plotly::renderPlotly(plot_linedate(
    data = get(gender.plots$dataset[6]),
    legend = "Age group"))





  # Second tab --------------------------------------------------------------

  # Row 1 -------------------------------------------------------------------




}

shiny::shinyApp(ui = ui, server = server)
