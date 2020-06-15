# library(ggplot2)
# library(magrittr)
# library(rgdal)
# library(SCRCdataAPI)
# library(spdplyr)
# library(shinycssloaders)

# source("preload_data.R", local = TRUE)
# source("frontpage.R", local = TRUE)
source("deaths-involving-coronavirus-covid-19.R", local = TRUE)
source("coronavirus-covid-19-management-information.R", local = TRUE)

# source("demographics.R", local = TRUE)


# User interface ------------------------------------------------------------

library(semantic.dashboard)

ui <- semantic.dashboard::dashboardPage(
  dashboardHeader(title = "SCRC dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("deaths",
               tabName = "tab1",
               icon = icon("heartbeat")),
      menuItem("management",
               tabName = "tab2",
               icon = icon("hospital"))
    )
  ),

  dashboardBody(
    tabItems(
      selected = 1,
      # First tab content ---------------------------------------------------
      tabItem(
        tabName = "tab1",

        # Tab 1, row 1 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = all.plots$title[1],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    shinycssloaders::withSpinner(
                                      plotly::plotlyOutput("all.summary")
                                    ),
                                    div(all.plots$location[1],
                                        style = "color:grey; text-align:right"),
                                    div(all.plots$location[3],
                                        style = "color:grey; text-align:right"),
                                    div(all.plots$location[2],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 1, row 2 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = multiline.plots$title[1],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    shinycssloaders::withSpinner(
                                      reactable::reactableOutput("sparkcouncil")
                                    ),
                                    div(multiline.plots$loc_c[1],
                                        style = "color:grey; text-align:right"),
                                    div(multiline.plots$loc_a[1],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 1, row 3 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = multiline.plots$title[2],
                                  color = "blue",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    shinycssloaders::withSpinner(
                                      reactable::reactableOutput("sparknhs")
                                    ),
                                    div(multiline.plots$loc_c[2],
                                        style = "color:grey; text-align:right"),
                                    div(multiline.plots$loc_a[2],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 1, row 4 ------------------------------------------------------

        # Compare gender
        shiny::fluidRow(
          semantic.dashboard::box(width = 10,
                                  title = comparegender.plots$title[1],
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 10,
                                    shinycssloaders::withSpinner(
                                      plotly::plotlyOutput("gender.all.stacked")
                                    ),
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
                                    shinycssloaders::withSpinner(
                                      plotly::plotlyOutput("gender.all.pie")
                                    ),
                                    br(), br()
                                  )
          )
        ),

        # Tab 1, row 5 ------------------------------------------------------

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

        # Tab 1, row 6 ------------------------------------------------------

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

        # Tab 1, row 7 ------------------------------------------------------

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

        # Tab 1, row 8 ------------------------------------------------------

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

        # Tab 1, row 9 ------------------------------------------------------

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

        # Tab 1, row 10 -----------------------------------------------------

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

        # Tab 1, row 11 -----------------------------------------------------

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

        # Tab 1, row 12 -----------------------------------------------------

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
        ) # End fluidrow


      ), # End tabitem


      # Second tab ----------------------------------------------------------
      tabItem(
        tabName = "tab2",

        h1("Testing"),

        # Tab 2, row 1 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[23],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("scot.testing.cum.tests"),
                                    div(plots$location[23],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[22],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("scot.testing.cum.people"),
                                    div(plots$location[22],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 16 -----------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[25],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("scot.testing.daily.tests"),
                                    div(plots$location[25],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[24],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("scot.testing.daily.people"),
                                    div(plots$location[24],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 2 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[4],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    shinycssloaders::withSpinner(
                                      reactable::reactableOutput("nhs.testing")
                                    ),
                                    div(plots$location[4],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = gsub("cumulative", "current",
                                               plots$title[4]),
                                  color = "grey",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    shinycssloaders::withSpinner(
                                      plotly::plotlyOutput("nhs.pie",
                                                           height = "80%")
                                    ),
                                    br()
                                  )
          )
        ),

        h1("Hospital"),

        # Tab 2, row 12 -----------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[19],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("scot.hosp"),
                                    div(plots$location[19],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[20],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("scot.icu"),
                                    div(plots$location[20],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 18 -----------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[26],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("special.hosp"),
                                    div(plots$location[26],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[27],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("special.icu"),
                                    div(plots$location[27],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 10 -----------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = plots$title[17],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    plotly::plotlyOutput("deaths"),
                                    div(plots$location[17],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 1 ------------------------------------------------------
        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = paste(gsub(" - Confirmed", "",
                                                     plots$title[1]), "(daily)"),
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    shinycssloaders::withSpinner(
                                      reactable::reactableOutput("nhs.confirmed")
                                    ),
                                    div(plots$location[1],
                                        style = "color:grey; text-align:right"),
                                    div(plots$location[2],
                                        style = "color:grey; text-align:right"),
                                    div(plots$location[3],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 11 -----------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = plots$title[18],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    plotly::plotlyOutput("discharges"),
                                    div(plots$location[18],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 14 -----------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = plots$title[21],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    plotly::plotlyOutput("nhs.workforce"),
                                    div(plots$location[21],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        h1("Adult care homes"),

        # Tab 2, row 5 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[7],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome1"),
                                    div(plots$location[7],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[8],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome2"),
                                    div(plots$location[8],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 6 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[9],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome3"),
                                    div(plots$location[9],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[10],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome4"),
                                    div(plots$location[1],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 7 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[11],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome5"),
                                    div(plots$location[11],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[12],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome6"),
                                    div(plots$location[12],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 8 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[13],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome7"),
                                    div(plots$location[13],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[14],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome8"),
                                    div(plots$location[14],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 9 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 8,
                                  title = plots$title[15],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome9"),
                                    div(plots$location[15],
                                        style = "color:grey; text-align:right")
                                  )
          ),
          semantic.dashboard::box(width = 8,
                                  title = plots$title[16],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 8,
                                    plotly::plotlyOutput("carehome10"),
                                    div(plots$location[16],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        h1("Misc"),

        # Tab 2, row 3 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = plots$title[5],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    plotly::plotlyOutput("ambulance"),
                                    div(plots$location[5],
                                        style = "color:grey; text-align:right")
                                  )
          )
        ),

        # Tab 2, row 4 ------------------------------------------------------

        shiny::fluidRow(
          semantic.dashboard::box(width = 16,
                                  title = plots$title[6],
                                  color = "red",
                                  ribbon = TRUE,
                                  title_side = "top right",
                                  semantic.dashboard::column(
                                    width = 16,
                                    plotly::plotlyOutput("calls"),
                                    div(plots$location[6],
                                        style = "color:grey; text-align:right")
                                  )
          )
        )

      ) # End tabitem (second tab)
    ) # End tabitems
  ) # End dashboardBody

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

  output$nhs.confirmed <- reactable::renderReactable(table_two(
    confirmed = get(plots$dataset[1]) %>%
      convert_areacodes(conversion.table),
    suspected = get(plots$dataset[2]) %>%
      convert_areacodes(conversion.table),
    total = get(plots$dataset[3]) %>%
      convert_areacodes(conversion.table)
  ))

  output$nhs.testing <- reactable::renderReactable(table_three(
    data = get(plots$dataset[4]) %>%
      convert_areacodes(conversion.table)
  ))

  tmp.nhs <- get(plots$dataset[4]) %>%
    convert_areacodes(conversion.table) %>%
    .[, ncol(.), drop = FALSE]
  tmp.nhs[, 1] <- as.numeric(tmp.nhs[, 1])


  output$nhs.pie <- plotly::renderPlotly(donut_two(
    data = tmp.nhs
  ))

  output$ambulance <- plotly::renderPlotly(plot_lineman(
    data = get(plots$dataset[5])
  ))

  output$calls <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[6])
  ))

  # Number with current suspected COVID-19 cases
  output$carehome1 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[7])
  ))

  # Adult care homes which submitted a return
  output$carehome2 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[8])
  ))

  tmp.care <- get(plots$dataset[9])
  tmp <- "Adult care homes - Cumulative number that have reported "
  rownames(tmp.care) <- gsub(tmp, "", rownames(tmp.care))

  # Cumulative number that have reported a suspected COVID-19 case
  output$carehome3 <- plotly::renderPlotly(plot_lineman(
    data = tmp.care
  ))

  # Cumulative number of suspected COVID-19 cases
  output$carehome4 <- plotly::renderPlotly(plot_lineman(
    data = get(plots$dataset[10])
  ))

  # Daily number of new suspected COVID-19 cases
  output$carehome5 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[11])
  ))

  # Proportion that have reported a suspected COVID-19 case
  output$carehome6 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[12])
  ))

  # Response rate
  output$carehome7 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[13])
  ))

  # Staff absence rate
  output$carehome8 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[14])
  ))

  # Number of staff reported as absent
  output$carehome9 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[15])
  ))

  # Total number of staff in adult care homes which submitted a return
  output$carehome10 <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[16])
  ))

  # Number of COVID-19 confirmed deaths registered to date (cumulative)
  output$deaths <- plotly::renderPlotly(plot_lineman(
    data = get(plots$dataset[17])
  ))

  output$discharges <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[18])
  ))

  tmp.hosp <-  get(plots$dataset[19])
  rownames(tmp.hosp) <- gsub("COVID-19 patients in hospital - ", "",
                             rownames(tmp.hosp))

  output$scot.hosp <- plotly::renderPlotly(plot_lineman(
    data = tmp.hosp
  ))

  tmp.icu <- get(plots$dataset[20])
  rownames(tmp.icu) <- gsub("COVID-19 patients in ICU - ", "",
                            rownames(tmp.icu))

  output$scot.icu <- plotly::renderPlotly(plot_lineman(
    data = tmp.icu
  ))

  output$nhs.workforce <- plotly::renderPlotly(plot_lineman(
    data = get(plots$dataset[21])))

  tmp.test <- get(plots$dataset[22])
  tmp <- "Testing - Cumulative people tested for COVID-19 - "
  rownames(tmp.test) <- gsub(tmp, "", rownames(tmp.test))

  output$scot.testing.cum.people <- plotly::renderPlotly(plot_lineman(
    data = tmp.test
  ))

  tmp.labs <- get(plots$dataset[23])
  tmp <- "Testing - Total number of COVID-19 tests carried out by "
  rownames(tmp.labs) <- gsub(tmp, "", rownames(tmp.labs))
  rownames(tmp.labs) <- gsub(" - Cumulative", "", rownames(tmp.labs))

  output$scot.testing.cum.tests <- plotly::renderPlotly(plot_lineman(
    data = tmp.labs
  ))

  output$scot.testing.daily.people <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[24])
  ))

  tmp.dailytests <- get(plots$dataset[25])
  tmp <- "Testing - Total number of COVID-19 tests carried out by "
  rownames(tmp.dailytests) <- gsub(tmp, "", rownames(tmp.dailytests))
  rownames(tmp.dailytests) <- gsub(" - Daily", "", rownames(tmp.dailytests))

  output$scot.testing.daily.tests <- plotly::renderPlotly(plot_stackedman(
    data = tmp.dailytests
  ))

  tmp.spec <- get(plots$dataset[26])
  rownames(tmp.spec) <- gsub("COVID-19 patients in hospital - ", "",
                             rownames(tmp.spec))

  output$special.hosp <- plotly::renderPlotly(plot_stackedman(
    data = tmp.spec
  ))

  output$special.icu <- plotly::renderPlotly(plot_stackedman(
    data = get(plots$dataset[27])
  ))
}

shiny::shinyApp(ui = ui, server = server)
