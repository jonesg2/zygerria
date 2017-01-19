#' Server side functionality for maps pages
#'
#' This function works with the \code{\link{twoMapPageInput}} UI function. It
#' provides the server functionality.
#'
#' @param geodata Data for the geographical map
#' @param hexdata Data for the hexagonal map
#'
#' @rdname mapPage
#'
#' @export
twoMapPage <- function(input, output, session, emp, geodata, hexdata, cols) {

  # extract the short name to match the data
  short_name <- reactive({
    dataColumnChoices[dataColumnChoices$full == input$stat, "short"]
  })

  # update the column selection
  mapFill <- reactive({
    if (input$fillType == "quint") {
      "quints"
    } else {
      short_name()
    }
  })

  # ensure the geo and hex data have quintile data
  geoMapData <- reactive({
    createLeafletData(calculateQuintiles(emp(), short_name()))
  })
  hexMapData <- reactive({
    hexQuints <- calculateQuintiles(emp(), short_name())
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, hexQuints, by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })

  # render the leaflet maps
  output$map_geo <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = geoMapData(),
      fill = mapFill(),
      hex = FALSE
    )
  })
  output$map_hex <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = hexMapData(),
      fill = mapFill(),
      hex = TRUE
    )
  })

  # on selection of an lad, place a marker on the map
  proxy_geo <- leafletProxy("map_geo")
  proxy_hex <- leafletProxy("map_hex")

  # Create the data for the marker positions
  markerDatGeo <- reactive({
    if (is.null(input$ladSel)) {
      return(NULL)
    } else {
      ladDat <- geodata()[geodata()@data$lad15nm %in% input$ladSel, ]
      ladDat <- data.frame(
        lng = do.call("c", lapply(ladDat@polygons, function(.) .@labpt[1])),
        lat = do.call("c", lapply(ladDat@polygons, function(.) .@labpt[2]))
      )
      ladDat
    }
  })
  markerDatHex <- reactive({
    if (is.null(input$ladSel)) {
      return(NULL)
    } else {
      ladDat <- hexdata()[hexdata()@data$lad15nm %in% input$ladSel, ]
      ladDat <- data.frame(
        lng = do.call("c", lapply(ladDat@polygons, function(.) .@labpt[1])),
        lat = do.call("c", lapply(ladDat@polygons, function(.) .@labpt[2]))
      )
      ladDat
    }
  })

  # Geographical Map
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    if (!is.null(input$ladSel)) {
      if (length(input$ladSel) > 0) {
        proxy_geo %>%
          removeMarkerCluster("lad_geo")
      }
      proxy_geo %>%
        addMarkers(lng = ~lng, lat = ~lat, data = markerDatGeo(),
                   clusterId = "lad_geo")
    }
  })
  # Hex Map
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    if (!is.null(input$ladSel)) {
      if (length(input$ladSel) > 0) {
        proxy_hex %>%
          removeMarkerCluster("lad_hex")
      }
      proxy_hex %>%
        addMarkers(lng = ~lng, lat = ~lat, data = markerDatHex(),
                   clusterId = "lad_hex")
    }
  })

  # on selection from the drop down menu, display the region's data in a table
  observe({
    if (!is.null(input$ladSel)) {
      colsToShow <- dataColumnChoices[c(12, 5:7, 24, 13:17, 32:54), "short"]
      rowNames <- dataColumnChoices[c(12, 5:7, 24, 13:17, 32:54), "full"]
      output$dataTable <- renderDataTable({
        subDat <- geodata()[geodata()@data$lad15nm %in% input$ladSel, ]@data
        subDat <- subDat[order(match(subDat$lad15nm, input$ladSel)), ]
        nmSub <- subDat$lad15nm
        subDat <- subDat[, colnames(subDat) %in% colsToShow]
        subDat <- as.data.frame(t(subDat))
        subDat$measure <- rownames(subDat)
        subDat <- merge(columnMeans(geodata()[, colsToShow]@data), subDat)
        subDat <- subDat[match(colsToShow, subDat$measure), ]
        subDat <- subDat[, !(colnames(subDat) %in% "measure")]
        # We need a check for a dataframe here otherwise we get an error when it
        # tries to set the columns names of something that isn't a dataframe.
        # I think this is due to the renderDataTable taking precedence over the
        # observe test
        if (is.data.frame(subDat)) {
          colnames(subDat) <- c("National Average", nmSub)
          rownames(subDat) <- rowNames
          datatable(
            subDat,
            options = list(
              pageLength = 33,
              dom = "t"
            )
          )
        }
      })
    } else {
      return(NULL)
    }
  })

  # reset region selection inputs on a button click for geo map
  observeEvent(input$clearSelection, {
    proxy_geo %>%
      clearMarkers()
    proxy_hex %>%
      clearMarkers()
    if (!is.null(input$ladSel)) {
      updateSelectizeInput(
        session,
        "ladSel",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
    }
  })

}
