#' run_app
#'
#' @param app name of app to run. Options: `dashboard_test`, `upload_test`
#'
#' @returns shiny app executes
#' @export
#'
#' @importFrom shiny runApp
run_app <- function(app = 'dashboard_test') {
  # ctrl alt shift r
  appDir <- system.file(app, package = "R8DWLCP") # Replace "myPackage" with your package name
  if (appDir == "") {
    stop("Could not find shiny app directory. Try re-installing `R8DWLCP`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}