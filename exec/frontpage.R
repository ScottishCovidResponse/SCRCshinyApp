#' frontpage
#'
#'

# Extract data ------------------------------------------------------------

h5filename <- "data-raw/deaths-involving-coronavirus-covid-19.h5"

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
