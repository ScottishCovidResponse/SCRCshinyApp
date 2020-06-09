library(hdf5r)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)


# Deaths involving Coronavirus Covid 19 -----------------------------------

h5filename <- "../SCRCdataAPI/deaths-involving-coronavirus-covid-19.h5"
# file_structure(h5filename)

all.councilloc.dat <- reconstruct_object(
  h5filename, "councilarea/per_location/all_deaths")
covid.councilloc.dat <- reconstruct_object(
  h5filename, "councilarea/per_location/covid_related_deaths")

all.council.dat <- reconstruct_object(
  h5filename, "councilarea/per_week/all_deaths")
covid.council.dat <- reconstruct_object(
  h5filename, "councilarea/per_week/covid_related_deaths")

all.location.dat <- reconstruct_object(
  h5filename, "location/per_week/all_deaths")
covid.location.dat <- reconstruct_object(
  h5filename, "location/per_week/covid_related_deaths")

all.nhsloc.dat <- reconstruct_object(
  h5filename, "nhsboard/per_location/all_deaths")
covid.nhsloc.dat <- reconstruct_object(
  h5filename, "nhsboard/per_location/covid_related_deaths")

all.nhs.dat <- reconstruct_object(
  h5filename, "nhsboard/per_week/all_deaths")
covid.nhs.dat <- reconstruct_object(
  h5filename, "nhsboard/per_week/covid_related_deaths")

all.females.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/females/all_ages")
all.females.group.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/females/by_agegroup")
all.males.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/males/all_ages")
all.males.group.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/males/by_agegroup")
all.persons.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/persons/all_ages")
all.persons.group.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/persons/by_agegroup")

all.5years.dat <- reconstruct_object(
  h5filename, "scotland/per_week/all_deaths/persons/averaged_over_5years")

covid.females.dat <- reconstruct_object(
  h5filename, "scotland/per_week/covid_related_deaths/females/all_ages")
covid.females.group.dat <- reconstruct_object(
  h5filename, "scotland/per_week/covid_related_deaths/females/by_agegroup")
covid.males.dat <- reconstruct_object(
  h5filename, "scotland/per_week/covid_related_deaths/males/all_ages")
covid.males.group.dat <- reconstruct_object(
  h5filename, "scotland/per_week/covid_related_deaths/males/by_agegroup")
covid.persons.dat <- reconstruct_object(
  h5filename, "scotland/per_week/covid_related_deaths/persons/all_ages")
covid.persons.group.dat <- reconstruct_object(
  h5filename, "scotland/per_week/covid_related_deaths/persons/by_agegroup")



# Generate line plots -----------------------------------------------------

datasets <- c("all.councilloc.dat", "covid.councilloc.dat",
              "all.council.dat", "covid.council.dat",
              "all.location.dat", "covid.location.dat",
              "all.nhsloc.dat", "covid.nhsloc.dat",
              "all.nhs.dat", "covid.nhs.dat",
              "all.females.dat", "all.females.group.dat",
              "all.males.dat", "all.males.group.dat",
              "all.persons.dat", "all.persons.group.dat",
              "all.5years.dat", "covid.females.dat",
              "covid.females.group.dat", "covid.males.dat",
              "covid.males.group.dat", "covid.persons.dat",
              "covid.persons.group.dat") %>%
  data.frame(dataset = .) %>%
  dplyr::mutate(plotstyle = dplyr::case_when(
    grepl("council\\.dat", dataset) ~ "plot_multiline",
    grepl("nhs\\.dat", dataset) ~ "plot_multiline",
    grepl("group", dataset) ~ "plot_line",
    grepl("location", dataset) ~ "plot_line",
    grepl("councilloc", dataset) ~ "plot_stackedbar",
    grepl("nhsloc", dataset) ~ "plot_stackedbar"))

# Multi line plots
multiline.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_multiline") %$% dataset

# Line plots
line.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_line") %>%
  dplyr::mutate(one = dplyr::case_when(
    grepl("location", dataset) ~ 3,
    T ~ 4)) %>%
  tidyr::separate(dataset, c(NA, "two", NA), "\\.", remove = FALSE,
                  extra = "drop") %>%
  dplyr::arrange(one, two) %$%
  dataset

for(i in seq_along(line.plots))
  assign(paste0("g.", line.plots[i]), plot_line(get(line.plots[i])))

# Stacked bar plots
stacked.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_stackedbar") %$% dataset

# Other plots
other.plots <- datasets %>%
  dplyr::filter(is.na(plotstyle)) %$% dataset

for(i in seq_along(other.plots))
  assign(paste0("g.", other.plots[i]), plot_line(get(other.plots[i])))


# Box values --------------------------------------------------------------

# deathbox
latest.total <- 2

# total.deaths <- plot.deaths %>%
#   filter(variable == "covid")
# total.deaths <- sum(total.deaths$value, na.rm = TRUE)
total.deaths <- 2

# latest <- max(as.Date(plot.deaths$date))
# latest.deaths <- plot.deaths %>%
#   filter(date == latest,
#          variable == "covid")
# latest.deaths <- sum(latest.deaths$value)
latest.deaths <- 5

latest.current <- 3
latest.recovered <- 3


# countbox

# userbox


# User interface ----------------------------------------------------------
library(shinythemes)

ui <- dashboardPage(
  dashboardHeader(title = "SCRC dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Covid-19 overview", tabName = "tab1",
               icon = icon("dashboard")),
      menuItem("Scottish demographics", tabName = "tab2",
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

                # Line plots
                uiOutput("boxes.lineplots"),

                # Stacked bar plots
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

}

shinyApp(ui = ui, server = server)
