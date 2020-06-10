#' runApp
#'
#' @export
#'
runApp <- function() {
  appDir <- system.file("inst/app.R", package = "SCRCshinyApp")
  if (appDir == "")
    stop("Could not find SCRCshinyApp. Try re-installing `SCRCshinyApp`.",
         call. = FALSE)

  shiny::runApp(appDir, display.mode = "normal")
}
