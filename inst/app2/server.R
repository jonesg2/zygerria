server <- function(input, output, session) {

  #############################################################################
  ## Load in Data

  # extract the short name to match the data
  shortNameOne <- reactive({
    dataColumnChoices[dataColumnChoices$full == input[["mapOneChoices-stat"]], "short"]
  })
  shortNameTwo <- reactive({
    dataColumnChoices[dataColumnChoices$full == input[["mapTwoChoices-stat"]], "short"]
  })

  # create the leaflet and hex datasets
  geoData <- reactive({
    createLeafletData(data = emp)
  })
  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, emp, by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })

  #############################################################################
  ## Create the maps

  # Map One
  mapOneData <- reactive({
    if (input[["mapOneChoices-mapType"]] == "hex") {
      hexQuints <- calculateQuintiles(emp, shortNameOne())
      hexMapJson@data <- dplyr::left_join(
        hexMapJson@data, hexQuints, by = c("lad15nm" = "la_name")
      )
      hexMapJson
    } else {
      createLeafletData(calculateQuintiles(emp, shortNameOne()))
    }
  })
  mapOneType <- reactive({
    if (input[["mapOneChoices-mapType"]] == "hex") {
      TRUE
    } else {
      FALSE
    }
  })
  mapOneFill <- reactive({
    if (input[["mapOneChoices-fillType"]] == "quint") {
      "quints"
    } else {
      shortNameOne()
    }
  })
  callModule(
    appMap,
    "mapOne",
    emp = emp,
    data = mapOneData,
    fill = mapOneFill,
    hex = mapOneType
  )

  # Map Two
  mapTwoData <- reactive({
    if (input[["mapTwoChoices-mapType"]] == "hex") {
      hexQuints <- calculateQuintiles(emp, shortNameTwo())
      hexMapJson@data <- dplyr::left_join(
        hexMapJson@data, hexQuints, by = c("lad15nm" = "la_name")
      )
      hexMapJson
    } else {
      createLeafletData(calculateQuintiles(emp, shortNameTwo()))
    }
  })
  mapTwoType <- reactive({
    if (input[["mapTwoChoices-mapType"]] == "hex") {
      TRUE
    } else {
      FALSE
    }
  })
  mapTwoFill <- reactive({
    if (input[["mapTwoChoices-fillType"]] == "quint") {
      "quints"
    } else {
      shortNameTwo()
    }
  })
  callModule(
    appMap,
    "mapTwo",
    emp = emp,
    data = mapTwoData,
    fill = mapTwoFill,
    hex = mapTwoType
  )

  #############################################################################
  ## Add marker events to the maps

  mapOneProxy <- leafletProxy("mapOne-map")

  mapOneMarkers <- reactive({
    markerData(mapOneData(), input$ladSel)
  })

  observeEvent({
    input[["mapOneChoices-stat"]]
    mapOneMarkers()
    input[["mapOneChoices-fillType"]]
  }, {
    ladOne <- mapOneProxy %>%
      clearMarkerClusters() %>%
      clearMarkers()
    if (length(input$ladSel) > 0) {
      ladOne <-
        ladOne %>%
        addMarkers(lng = ~lng, lat = ~lat, data = mapOneMarkers(),
                   clusterId = "ladOne")
    }
    ladOne
  })

  mapTwoProxy <- leafletProxy("mapTwo-map")

  mapTwoMarkers <- reactive({
    markerData(mapTwoData(), input$ladSel)
  })

  observeEvent({
    input[["mapTwoChoices-stat"]]
    mapTwoMarkers()
    input[["mapTwoChoices-fillType"]]
  }, {
    ladTwo <- mapTwoProxy %>%
      clearMarkerClusters() %>%
      clearMarkers()
    if (length(input$ladSel) > 0) {
      ladTwo <-
        ladTwo %>%
        addMarkers(lng = ~lng, lat = ~lat, data = mapTwoMarkers(),
                   clusterId = "ladTwo")
    }
    ladTwo
  })

  #############################################################################
  ## Clear the select inputs

  # reset region selection inputs on a button click
  observeEvent(input$clearSelection, {
    clickData$e <- NULL
    updateSelectizeInput(
      session,
      "ladSel",
      choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
    )
  })

  #############################################################################
  ## Add inputs on map click

  observeEvent(input[["mapOne-map_shape_click"]], {
    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(
        input$ladSel,
        shape@data[shape@data$lad15cd %in% input[["mapOne-map_shape_click"]]$id, ]
      )
    )
  })
  observeEvent(input[["mapTwo-map_shape_click"]], {
    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(
        input$ladSel,
        shape@data[shape@data$lad15cd %in% input[["mapTwo-map_shape_click"]]$id, ]
      )
    )
  })

  #############################################################################
  ## Create the scatter plot

  # observe click events and enable them to be cleared
  clickData <- reactiveValues(e = NULL)

  observe({
    clickData$e <- event_data("plotly_click")
  })

  # output the scatter graph
  output$scatFig <- renderPlotly({

    # calculate the scatter plot colour statistic
    fullColour <- dataColumnChoices[dataColumnChoices$full %in% input[["scatColour"]], "short"]
    colourQuant <- quantile(emp[, fullColour], probs = c(1/3, 2/3), na.rm = TRUE)

    compDat <- emp %>%
      mutate(
        colourCol = factor(if_else(
            emp[, fullColour] < colourQuant[1], "red",
            if_else(
              emp[, fullColour] >= colourQuant[1] &
                emp[, fullColour] < colourQuant[2],
              "orange",
              "green"
            )
          ),
          levels = c("red", "orange", "green"),
          labels = c(paste0("<", colourQuant[1]),
                     paste0(colourQuant[1], "-", colourQuant[2]),
                     paste0(">", colourQuant[2]))
        ))

    updateSelectizeInput(
      session,
      "ladSel",
      choices = sort(unique(shape@data$lad15nm)),
      selected = c(
        input$ladSel,
        clickData$e$key
      )
    )

    compositeScatter(
      compDat,
      x = dataColumnChoices[dataColumnChoices$full %in% input[["mapTwoChoices-stat"]], "short"],
      y = dataColumnChoices[dataColumnChoices$full %in% input[["mapOneChoices-stat"]], "short"],
      colour = "colourCol",
      xLab = dataColumnChoices[dataColumnChoices$full %in% input[["mapTwoChoices-stat"]], "full"],
      yLab = dataColumnChoices[dataColumnChoices$full %in% input[["mapOneChoices-stat"]], "full"],
      highlight = input$ladSel
    )
  })

  #############################################################################
  ## Create the datatable

  # on selection from the drop down menu, display the region's data in a table
  observe({
    if (!is.null(input$ladSel)) {
      colsToShow <- dataColumnChoices[c(12, 5:7, 24, 13:17, 32:54), "short"]
      rowNames <- dataColumnChoices[c(12, 5:7, 24, 13:17, 32:54), "full"]
      output$dataTable <- renderDataTable({
        subDat <- geoData()[geoData()@data$lad15nm %in% input$ladSel, ]@data
        subDat <- subDat[order(match(subDat$lad15nm, input$ladSel)), ]
        nmSub <- subDat$lad15nm
        subDat <- subDat[, colnames(subDat) %in% colsToShow]
        subDat <- as.data.frame(t(subDat))
        subDat$measure <- rownames(subDat)
        subDat <- merge(columnMeans(geoData()[, colsToShow]@data), subDat)
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
              pageLength = 10,
              lengthMenu = c(10, 20, 30, 40),
              dom = "tlp"
            )
          )
        }
      })
    } else {
      return(NULL)
    }
  })

  #############################################################################
  ## Download Handler

  DL <- reactive(subDat)

  output$downloadData <- downloadHandler(
    filename = "Download.csv",
    content = function(file) {
      write.csv(DL(), file)
    }
  )


  #############################################################################
  ## Create the time series plot

  output$timeseries <- renderPlotly({
    validate(
      need(input$timeIns, "Please select some LADs")
    )
    timeSeriesPlot(empTime[empTime$la_name %in% input$timeIns, ],
                   x = "year", y = "val", color = "la_name")
  })
}
