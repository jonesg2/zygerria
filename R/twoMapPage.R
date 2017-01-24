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
      hex = FALSE,
      addLegend = FALSE
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
  observeEvent({
    input$stat
    markerDatGeo()
    input$fillType
  }, {
    validate(
      need(emp(), "Please upload employment statistics")
    )
    p_geo <- proxy_geo %>%
      clearMarkerClusters() %>%
      clearMarkers()
    if (length(input$ladSel) > 0) {
      p_geo <-
        p_geo %>%
        addMarkers(lng = ~lng, lat = ~lat, data = markerDatGeo(),
                   clusterId = "lad_geo")
    }
    p_geo
  })
  # Hex Map
  observeEvent({
    input$stat
    markerDatHex()
    input$fillType
  }, {
    validate(
      need(emp(), "Please upload employment statistics")
    )
    p_hex <- proxy_hex %>%
      clearMarkerClusters() %>%
      clearMarkers()
    if (length(input$ladSel) > 0) {
      p_hex <-
        p_hex %>%
        addMarkers(lng = ~lng, lat = ~lat, data = markerDatHex(),
                   clusterId = "lad_hex")
    }
    p_hex
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

  observeEvent(input$map_geo_shape_mouseover, {
    output$ladInfo <- renderUI({
      point <- as.data.frame(input$map_geo_shape_mouseover)
      info <- geodata()[geodata()@data$lad15cd %in% point$id, ]@data
      tagList(
        br(),
        valueBox(
          value = info[, short_name()],
          subtitle = info[, "lad15nm"],
          width = NULL
        )
      )
    })
  })

  # Populate the select input when a region on the map is selected
  observeEvent(input$map_geo_shape_click, {
    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(input$ladSel, shape@data[shape@data$lad15cd %in% input$map_geo_shape_click$id, ])
    )
  })
  observeEvent(input$map_hex_shape_click, {
    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(input$ladSel, shape@data[shape@data$lad15cd %in% input$map_hex_shape_click$id, ])
    )
  })

}
