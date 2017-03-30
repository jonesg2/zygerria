#' @export
appMap <- function(input, output, session, emp, data, fill, hex) {
  output$map <- renderLeaflet({
    leafMap(
      mapData = data(),
      fill = fill(),
      hex = hex(),
      addLegend = TRUE
    )
  })
}
