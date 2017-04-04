#' Run the App
#'
#' Run the future employment prospects app
#'
#' @author Nathan Eastwood
#'
#' @export
runEmploymentProspectsApp <- function() {
  appDir <- system.file("app2", package = "zygerria")
  if (appDir == "") {
    stop(
      paste0(
        "Could not find example directory. Try re-installing ",
        "`zygerria`."
      ),
      call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
