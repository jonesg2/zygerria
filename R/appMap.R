#' @export
appMap <- function(input, output, session, emp, data, fill, hex) {
  output$map <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = data(),
      fill = fill(),
      hex = hex(),
      addLegend = TRUE
    )
  })
}
