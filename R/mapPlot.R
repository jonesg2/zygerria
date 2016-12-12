#' Create the map of the UK
#'
#' Generate a map of the UK which interactively fills each Local Authority
#' District (LAD) based on its statistics.
#'
#' @param data The dataset produced by \code{\link{createPlotlyData}}
#' @param fill The statistic by which the map should be filled
#'
#' @author Nathan Eastwood
#'
#' @import ggplot2
#' @export
mapPlot <- function(data, fill) {
  ukMap <- ggplot(data = data,
                  aes_string(x = "long", y = "lat", group = "group")) +
    geom_polygon(aes_string(fill = paste0("`", fill, "`"))) +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Statistic\nValue")
  plotly::ggplotly(ukMap)
}
