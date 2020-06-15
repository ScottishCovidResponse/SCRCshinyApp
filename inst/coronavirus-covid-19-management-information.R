#' coronavirus-covid-19-management-information.h5 dataset
#'
#'


# Extract data ------------------------------------------------------------

h5filename <- system.file("data-raw/coronavirus-covid-19-management-information.h5",
                          package = "SCRCshinyApp")

conversion.table <- read.csv2(system.file("data-raw/convert.csv",
                                          package = "SCRCshinyApp")) %>%
  select(-X, -areatypename) %>% unique()

management.dat <- file_structure(h5filename) %>%
  dplyr::rename(location = name) %>%
  # Define object names (where extracted data will be stored)
  dplyr::mutate(dataset = c("nhs.confirmed.dat",
                            "nhs.suspected.dat",
                            "nhs.total.dat",
                            "nhs.testing.dat",
                            "scot.ambulance.dat",
                            "scot.calls.dat",
                            "scot.carehome.suspect.dat",
                            "scot.carehome.return.dat",
                            "scot.carehome.num.dat",
                            "scot.carehome.num.sus.dat",
                            "scot.carehome.new.dat",
                            "scot.carehome.proportion.dat",
                            "scot.carehome.response.dat",
                            "scot.carehome.absence.rate.dat",
                            "scot.carehome.staff.absent.dat",
                            "scot.carehome.staff.return.dat",
                            "scot.deaths.dat",
                            "scot.discharges.dat",
                            "scot.hospital.dat",
                            "scot.icu.dat",
                            "nhs.workforce.dat",
                            "scot.testcum.people.dat",
                            "scot.testcum.tests.dat",
                            "scot.testdaily.dat",
                            "scot.testdaily.total.dat",
                            "special.hospital.dat",
                            "special.icu.dat")) %>%
  # Generate plot titles
  dplyr::mutate(title = c(
    "COVID-19 patients in hospital - Confirmed",
    "COVID-19 patients in hospital - Suspected ",
    "COVID-19 patients in ICU - Total",
    "People tested postive for COVID-19 by NHS health board (cumulative)",
    paste("Numbers of ambulance attendances and number of people taken to" ,
          "hospital with suspected COVID-19 (daily)"),
    "Numbers of calls to NHS 111 and the coronavirus helpline (daily)",
    "Number with current suspected COVID-19 cases",
    "Adult care homes which submitted a return",
    "Cumulative number that have reported a suspected COVID-19 case",
    "Cumulative number of suspected COVID-19 cases",
    "Daily number of new suspected COVID-19 cases",
    "Proportion that have reported a suspected COVID-19 case",
    "Response rate",
    "Staff absence rate",
    "Number of staff reported as absent",
    "Total number of staff in adult care homes which submitted a return",
    "Number of COVID-19 confirmed deaths registered to date (cumulative)",
    "Number of people delayed in hospital (daily)",
    paste("Numbers of people in hospital with confirmed or suspected COVID-19",
          "in scotland (daily)"),
    paste("Numbers of people in ICU with confirmed or suspected COVID-19 in",
          "scotland (daily)"),
    paste("Numbers of NHS workforce reporting as absent due to a range of",
          "reasons related to Covid-19 (daily)"),
    "Cumulative people tested for COVID-19 (cumulative)",
    "Total number of COVID-19 tests carried out (cumulative)",
    "People found positive (daily)",
    "Total number of COVID-19 tests carried out (daily)",
    paste("Numbers of people in hospital with confirmed or suspected COVID-19",
          "in special health board (daily)"),
    paste("Numbers of people in ICU with confirmed or suspected COVID-19 in",
          "special health board (daily)"))) %>%
  # Define what kind of plot will be generated
  dplyr::mutate(plotstyle = "plot_multiline") %>%
  # Define how data will be grouped within each plot
  dplyr::mutate(groupby = "sdf")

# Extract data
for(i in seq_len(nrow(management.dat)))
  assign(management.dat$dataset[i],
         reconstruct_object(h5filename, management.dat$location[i]))



# plots
plots <- management.dat %>%
  dplyr::filter(plotstyle == "plot_multiline")













