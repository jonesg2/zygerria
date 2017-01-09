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

  # render the leaflet maps
  output$map_geo <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = geodata(),
      fill = short_name(),
      hex = FALSE
    )
  })
  output$map_hex <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = hexdata(),
      fill = short_name(),
      hex = TRUE
    )
  })

  # on selection of an lad, place a marker on the map
  proxy_geo <- leafletProxy("map_geo")
  proxy_hex <- leafletProxy("map_hex")

  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    eventLad1 <- input$lad1 != ""
    if (eventLad1) {
      lad1Dat <- geodata()[geodata()@data$lad15nm %in% input$lad1, ]
      lad1Dat <- data.frame(lng = lad1Dat@polygons[[1]]@labpt[1],
                            lat = lad1Dat@polygons[[1]]@labpt[2])
      proxy_geo %>% addMarkers(lng = ~lng, lat = ~lat, data = lad1Dat,
                               layerId = "lad1_geo")
    }
  })
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    eventLad2 <- input$lad2 != ""
    if (eventLad2) {
      lad2Dat <- geodata()[geodata()@data$lad15nm %in% input$lad2, ]
      lad2Dat <- data.frame(lng = lad2Dat@polygons[[1]]@labpt[1],
                            lat = lad2Dat@polygons[[1]]@labpt[2])
      proxy_geo %>% addMarkers(lng = ~lng, lat = ~lat, data = lad2Dat,
                               layerId = "lad2_geo")
    }
  })
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    eventLad3 <- input$lad3 != ""
    if (eventLad3) {
      lad3Dat <- geodata()[geodata()@data$lad15nm %in% input$lad3, ]
      lad3Dat <- data.frame(lng = lad3Dat@polygons[[1]]@labpt[1],
                            lat = lad3Dat@polygons[[1]]@labpt[2])
      proxy_geo %>% addMarkers(lng = ~lng, lat = ~lat, data = lad3Dat,
                               layerId = "lad3_geo")
    }
  })
  # Hex Map
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    eventLad1 <- input$lad1 != ""
    if (eventLad1) {
      lad1Dat <- hexdata()[hexdata()@data$lad15nm %in% input$lad1, ]
      lad1Dat <- data.frame(lng = lad1Dat@polygons[[1]]@labpt[1],
                            lat = lad1Dat@polygons[[1]]@labpt[2])
      proxy_hex %>% addMarkers(lng = ~lng, lat = ~lat, data = lad1Dat,
                               layerId = "lad1_hex")
    }
  })
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    eventLad2 <- input$lad2 != ""
    if (eventLad2) {
      lad2Dat <- hexdata()[hexdata()@data$lad15nm %in% input$lad2, ]
      lad2Dat <- data.frame(lng = lad2Dat@polygons[[1]]@labpt[1],
                            lat = lad2Dat@polygons[[1]]@labpt[2])
      proxy_hex %>% addMarkers(lng = ~lng, lat = ~lat, data = lad2Dat,
                               layerId = "lad2_hex")
    }
  })
  observe({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    eventLad3 <- input$lad3 != ""
    if (eventLad3) {
      lad3Dat <- hexdata()[hexdata()@data$lad15nm %in% input$lad3, ]
      lad3Dat <- data.frame(lng = lad3Dat@polygons[[1]]@labpt[1],
                            lat = lad3Dat@polygons[[1]]@labpt[2])
      proxy_hex %>% addMarkers(lng = ~lng, lat = ~lat, data = lad3Dat,
                               layerId = "lad3_hex")
    }
  })

  # on selection from the drop down menu, display the region's data in a table
  observe({
    event <- input$lad1 != "" | input$lad2 != "" | input$lad3 != ""
    if (event) {
      colsToShow <- dataColumnChoices[c(12, 5:7, 24, 13:17), "short"]
      rowNames <- dataColumnChoices[c(12, 5:7, 24, 13:17), "full"]
      output$dataTable <- renderDataTable({
        ins <- c(input$lad1, input$lad2, input$lad3)
        subDat <- geodata()[geodata()@data$lad15nm %in% ins, ]@data
        subDat <- subDat[order(match(subDat$lad15nm, ins)), ]
        nmSub <- subDat$lad15nm
        subDat <- subDat[, colnames(subDat) %in% colsToShow]
        subDat <- t(subDat)
        colnames(subDat) <- nmSub
        rownames(subDat) <- rowNames
        datatable(
          subDat,
          options = list(
            pageLength = 10,
            dom = "t"
          )
        )
      })
    } else {
      return(NULL)
    }
  })

  # reset region selection inputs on a button click for geo map
  observeEvent(input$clearSelection, {
    if (input$lad1 != "") {
      updateSelectInput(
        session,
        "lad1",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy_geo %>% removeMarker("lad1_geo")
    }
    if (input$lad2 != "") {
      updateSelectInput(
        session,
        "lad2",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy_geo %>% removeMarker("lad2_geo")
    }
    if (input$lad3 != "") {
      updateSelectInput(
        session,
        "lad3",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy_geo %>% removeMarker("lad3_geo")
    }
  })
  # reset region selection inputs on a button click for hex map
  observeEvent(input$clearSelection, {
    if (input$lad1 != "") {
      updateSelectInput(
        session,
        "lad1",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy_hex %>% removeMarker("lad1_hex")
    }
    if (input$lad2 != "") {
      updateSelectInput(
        session,
        "lad2",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy_hex %>% removeMarker("lad2_hex")
    }
    if (input$lad3 != "") {
      updateSelectInput(
        session,
        "lad3",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy_hex %>% removeMarker("lad3_hex")
    }
  })

}
