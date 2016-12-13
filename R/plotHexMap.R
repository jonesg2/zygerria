#' Plot the UK hexagon map
#'
#' Draw each local area district of the UK as a hexagon
#'
#' @param data Data extracted using the \code{\link{createHexMapData}} and saved
#'   as the \code{link{hexMapData}} dataset.
#' @param stat The statistic to fill the hexagons with. See details for more
#'   informtation.
#' @param plotly Logical. Return the plot using plotly (\code{TRUE}) or as a
#'   ggplot object.
#'
#' @details
#' The \code{stat} data is not included in \code{link{hexMapData}} dataset. You
#' will need to join data to this dataset. Typically you can join data together
#' using the \code{lad15cd} column.
#'
#' @author Nathan Eastwood, Douglas Ashton
#'
#' @importFrom ggplot2 ggplot geom_polygon aes aes_string theme_void
#'   scale_fill_distiller scale_colour_distiller
#' @importFrom plotly ggplotly
#'
#' @export
plotHexMap <- function(data, stat = NULL, plotly = TRUE) {
  g <- ggplot2::ggplot(
    data = data, ggplot2::aes_string(x = "long", y = "lat", group = "lad15nm")
  )
  if (!is.null(stat)) {
    g <- g +
      ggplot2::geom_polygon(
        ggplot2::aes_string(fill = stat, colour = stat)
      ) +
      ggplot2::scale_fill_distiller(palette = "Greens") +
      ggplot2::scale_color_distiller(palette = "Greens")
  } else {
    g <- g + ggplot2::geom_polygon()
  }

  # Ensure the theme elements are removed
  g <-
    g +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")

  # Return the plot
  if (plotly) {
    if (!is.null(stat)) {
      plotly::ggplotly(g, tooltip = c("group", "fill"))
    } else {
      plotly::ggplotly(g, tooltip = c("group"))
    }
  } else {
    g
  }
}
