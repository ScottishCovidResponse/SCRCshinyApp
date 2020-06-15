#' launchApp
#'
#' @export
#'
launchApp <- function() {
  appDir <- system.file("shinyapp/app.R", package = "SCRCshinyApp")
  if (appDir == "")
    stop("Could not find SCRCshinyApp. Try re-installing `SCRCshinyApp`.",
         call. = FALSE)

  shiny::runApp(appDir, display.mode = "normal")
}
