#' deaths-involving-coronavirus-covid-19 dataset
#'
#'

# Extract data ------------------------------------------------------------

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


# Assign data to plots ----------------------------------------------------

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
