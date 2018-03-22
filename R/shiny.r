#' Start a GUI for working with a tcorpus
#'
#' @export
corpusGUI <- function() {
  require_package('shiny')
  require_package('shinydashboard')
  require_package('DT')

  appDir <- system.file("shinyGUI", "corpusGUI", package = "corpustools")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}




