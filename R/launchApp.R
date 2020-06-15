#' launchApp
#'
#' @param refresh default is FALSE
#'
#' @export
#'
launchApp <- function(refresh = FALSE) {

  if(refresh) {
    SCRCdataAPI::download_source_version(dataset = "scotgov_management")
    sourcefile <- "data-raw/coronavirus-covid-19-management-information.csv"
    h5filename <- "coronavirus-covid-19-management-information.h5"
    SCRCdataAPI::process_scotgov_management(sourcefile = sourcefile,
                               h5filename = h5filename)
    file.copy(h5filename, file.path("inst", "extdata", h5filename),
              overwrite = TRUE)
    file.remove(h5filename)

    SCRCdataAPI::download_source_version(dataset = "scotgov_deaths")
    sourcefile <- "data-raw/deaths-involving-coronavirus-covid-19.csv"
    h5filename <- "deaths-involving-coronavirus-covid-19.h5"
    SCRCdataAPI::process_scot_gov_deaths(sourcefile = sourcefile,
                            h5filename = h5filename)
    file.copy(h5filename, file.path("inst", "extdata", h5filename),
              overwrite = TRUE)
    file.remove(h5filename)
  }

  appDir <- system.file("shinyapp/app.R", package = "SCRCshinyApp")
  if (appDir == "")
    stop("Could not find SCRCshinyApp. Try re-installing `SCRCshinyApp`.",
         call. = FALSE)

  shiny::runApp(appDir, display.mode = "normal")
}
