#' Create data for the leaflet map
#'
#' Merge the statistic data with the map data for the leaflet plot
#'
#' @param data The statistics to be combined with the shapefile data.
#'
#' @author Nathan Eastwood
#'
#' @export
createLeafletData <- function(data) {
  trans <- spTransform(shape, CRS("+init=epsg:4326"))
  trans@data <- dplyr::left_join(trans@data, data,
                                 by = c("lad15nm" = "LA Name")) %>%
    as.data.frame()
  trans
}
