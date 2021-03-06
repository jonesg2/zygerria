#' Generate a leaflet map of the UK
#'
#' Generate a leaflet map of the UK with polygons for each LAD region filled
#' by their statistics.
#'
#' @param mapData The spatial data.
#' @param fill The data which should be used to fill the map polygons. Defaults
#'   to \code{NULL} in the event a statistics hasn't been provided.
#' @param hex Logical; defaults to \code{FALSE}. Whether to generate a hex map
#'   (\code{TRUE}) or a normal map (\code{FALSE}).
#' @param addLegend Logical; defaults to \code{TRUE}. Should a legend be added
#'   to the map?
#'
#' @author Nathan Eastwood
#'
#' @import leaflet
#' @export
leafMap <- function(mapData, fill = NULL, hex = FALSE, addLegend = TRUE) {

  # Extract the map bounds
  bounds <- mapData@bbox

  # Create the plot
  map <- if (!hex) {
    leaflet(
      options = leafletOptions(
        minZoom = 4,
        maxZoom = 8
      )
    ) %>%
      setView(
        mean(bounds[1, ]),
        mean(bounds[2, ]),
        zoom = 5
      )
  } else {
    leaflet(options = leafletOptions(
      minZoom = 7,
      maxZoom = 10
      )
    ) %>%
      setView(
        mean(bounds[1, ]),
        mean(bounds[2, ]),
        zoom = 7.5
      )
  }

  # If a specific statistic is selected, generate the polygons layer for it
  if (!is.null(fill)) {
    # Generate the popup details
    # details <- paste0(
    #   "<strong>LAD: </strong>",
    #   mapData@data$lad15nm,
    #   "<br><strong>", fill, ": </strong>",
    #   mapData@data[, fill]
    # )

    # Generate a colour palette
    pal <- if (length(table(mapData@data[, fill])) == 4) {
      domain_min <- min(mapData@data[, fill], na.rm = TRUE)
      domain_max <- max(mapData@data[, fill], na.rm = TRUE)
      colorFactor("RdBu", factor(mapData@data[, fill]))
    } else if (length(table(mapData@data[, fill])) == 5) {
      colorFactor(c("#DD2A2F", "#EF9C61", "#FFFCB4", "#A4B3C0", "#4575B3"),
                  factor(mapData@data[, fill]))
    } else {
      domain_min <- min(roundDown(mapData@data[, fill]), na.rm = TRUE)
      domain_max <- max(roundUp(mapData@data[, fill]), na.rm = TRUE)
      colorNumeric("YlOrRd", domain = domain_min:domain_max)
    }

    # Add the polygons
    map <-
      map %>%
      addPolygons(
        data = mapData,
        weight = 1,
        fillColor = pal(mapData@data[, fill]),
        fillOpacity = 0.8,
        color = "#F5BE29",
        # popup = details,
        layerId = mapData$lad15cd,
        highlightOptions = highlightOptions(
          color = "#4E0388",
          weight = 2
        )
      )
    if (addLegend) {
      if (length(table(mapData@data[, fill])) == 4) {
        map <-
          map %>%
          addLegend(pal = pal, values = mapData@data[, fill])
      } else if (length(table(mapData@data[, fill])) == 5) {
        map <-
          map %>%
          addLegend(pal = pal, values = mapData@data[, fill],
                    labels = unique(mapData@data[, fill]))
      } else {
        map <-
          map %>%
          addLegend(pal = pal, values = mapData@data[, fill],
                    labFormat = labelFormat(suffix = "%"))
      }
    }
  }

  map
}
