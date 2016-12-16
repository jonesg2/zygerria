#' Create the hexagon map data for leaflet
#'
#' Create the hexagon map data for use in leaflet but ensure the columns names
#' are consistent with the other leaflet map data
#'
#' @author Nathan Eastwood
#'
#' @export
createHexMapJson <- function() {
  file <- system.file("data-raw", "geography.json",
                      package = "employmentProspects")
  data <- rgdal::readOGR(file)
  colnames(data@data) <- c("id", "lad15cd", "lad15nm")
  data
}
