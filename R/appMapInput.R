#' @export
appMapInput <- function(id) {
  ns <- NS(id)
  column(
    width = 6,
    leafletOutput(ns("map"))
  )
}
