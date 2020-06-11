#' deaths-involving-coronavirus-covid-19.h5 dataset
#'
#'

# Extract data ------------------------------------------------------------

h5filename <- system.file("data-raw/deaths-involving-coronavirus-covid-19.h5",
                          package = "SCRCshinyApp")

datasets <- file_structure(h5filename) %>%
  dplyr::rename(location = name) %>%
  # Define object names (where extracted data will be stored)
  dplyr::mutate(dataset = c("all.councilloc.dat", "covid.councilloc.dat",
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
                            "covid.persons.group.dat")) %>%
  # Generate plot titles
  dplyr::mutate(title = dplyr::case_when(
    grepl("all", dataset) ~ "Total number of deaths per week",
    grepl("covid", dataset) ~ "Number of covid-related deaths per week")) %>%
  dplyr::mutate(title = dplyr::case_when(
    grepl("council", dataset) ~ paste0(title, " by council area"),
    grepl("nhs", dataset) ~ paste0(title, " by NHS board"),
    T ~ title)) %>%
  dplyr::mutate(title = dplyr::case_when(
    grepl("loc\\.", dataset) ~ paste0(title, " and location"),
    T ~ title)) %>%
  dplyr::mutate(title = dplyr::case_when(
    grepl("group", dataset) ~ paste0(title, " by age group"),
    T ~ title)) %>%
  dplyr::mutate(title = dplyr::case_when(
    grepl("\\.males", dataset) ~ paste0(title, " (males)"),
    grepl("females", dataset) ~ paste0(title, " (females)"),
    T ~ title)) %>%
  dplyr::mutate(title = dplyr::case_when(
    dataset == "all.5years.dat" ~ paste0(title, " (averaged over 5 years)"),
    T ~ title)) %>%
  # Define what kind of plot will be generated
  dplyr::mutate(plotstyle = dplyr::case_when(
    grepl("council\\.dat", dataset) ~ "plot_multiline",
    grepl("nhs\\.dat", dataset) ~ "plot_multiline",
    grepl("group", dataset) ~ "plot_gender",
    grepl("females\\.dat", dataset)  ~ "compare_gender",
    grepl("males\\.dat", dataset) ~ "compare_gender",
    grepl("persons\\.dat", dataset) ~ "compare_all",
    grepl("location", dataset) ~ "plot_stackedbardate",
    grepl("councilloc", dataset) ~ "plot_stackedbar",
    grepl("nhsloc", dataset) ~ "plot_stackedbar",
    dataset == "all.5years.dat" ~ "compare_all")) %>%
  # Define how data will be grouped within each plot
  dplyr::mutate(groupby = dplyr::case_when(
    plotstyle == "plot_multiline" & grepl("council", location) ~
      "Council area",
    plotstyle == "plot_multiline" & grepl("nhs", location) ~ "NHS board",
    plotstyle == "plot_line" & grepl("males", location) ~ "Age group",
    plotstyle == "plot_line" & grepl("persons", location) ~ "Age group",
    T ~ "Legend title"))

# Extract data
for(i in seq_len(nrow(datasets)))
  assign(datasets$dataset[i], reconstruct_object(h5filename,
                                                 datasets$location[i]))



# Assign data to plots ----------------------------------------------------

# Sparkline tables
multiline.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_multiline") %>%
  dplyr::filter(grepl("^covid", dataset)) %>%
  dplyr::rename(covid_deaths = dataset,
                loc_c = location) %>%
  dplyr::mutate(all_deaths = gsub("covid", "all", covid_deaths),
                loc_a = gsub("covid_related", "all", covid_deaths),
                title = gsub("covid-related ", "", title))

# Line plots
gender.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_gender") %>%
  dplyr::mutate(one = dplyr::case_when(
    grepl("location", dataset) ~ 3,
    T ~ 4)) %>%
  tidyr::separate(dataset, c(NA, "two", NA), "\\.", remove = FALSE,
                  extra = "drop") %>%
  dplyr::arrange(one, two)

# Compare gender
comparegender.plots <- datasets %>%
  dplyr::filter(plotstyle == "compare_gender") %>%
  dplyr::filter(grepl("females", dataset)) %>%
  dplyr::rename(female_deaths = dataset,
                loc_f = location) %>%
  dplyr::mutate(male_deaths = gsub("fe", "", female_deaths),
                loc_m = gsub("fe", "", loc_f),
                title = gsub(" \\(females\\)", "", title))

# Stacked bar plots
stacked.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_stackedbar")

stackeddate.plots <- datasets %>%
  dplyr::filter(plotstyle == "plot_stackedbardate")



