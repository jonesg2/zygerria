#' Create data for the leaflet map
#'
#' Merge the statistic data with the map data for the leaflet plot
#'
#' @param data The statistics to be combined with the shapefile data.
#'
#' @author Nathan Eastwood
#'
#' @importFrom sp spTransform CRS
#'
#' @export
createLeafletData <- function(data) {
  trans <- sp::spTransform(shape, sp::CRS("+init=epsg:4326"))
  trans@data <- dplyr::left_join(trans@data, data,
                                 by = c("lad15nm" = "la_name")) %>%
    as.data.frame()
  trans
}
