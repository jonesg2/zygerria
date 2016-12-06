#' Create the map data
#'
#' Extract the data from the shapefile and merge it with the local authority
#' district statistics.
#'
#' @param data The statistics to be combined with the shapefile data.
#'
#' @author Nathan Eastwood
#'
#' @export
createPlotlyData <- function(data) {
  # extract the lad data
  shapeData <- shape@data
  # ensure the regions are integers
  shapeData$objectid <- as.integer(shapeData$objectid)

  # join the shapefile data and the lad stats
  allData <- dplyr::full_join(shapeData, data, by = c("lad15cd" = "LA Code"))

  # extract the coordiantes data to plot
  coordsHold <- data.frame(long = NA, lat = NA, group = NA, order = NA)
  for (i in seq_along(shape@polygons)) {
    coords <- data.frame(shape@polygons[[i]]@Polygons[[1]]@coords)
    colnames(coords) <- c("long", "lat")
    coords$group <- i
    coords$order <- 1:nrow(coords)
    coordsHold <- rbind(coordsHold, coords)
  }
  coordsHold <- coordsHold[-1, ]

  # create the final dataset by merging the coordinates with the statistics
  dplyr::full_join(coordsHold, allData, by = c("group" = "objectid"))
}
