server <- function(input, output){

  # load in the statistics data
  emp <- reactive({
    if (is.null(input$employData)) return(NULL)
    read.csv(input$employData$datapath,
             stringsAsFactors = FALSE,
             check.names = FALSE) %>%
      mutate(
        emp_rate_hml = if_else(
          emp_rate < 73, "red",
          if_else(emp_rate >= 73 & emp_rate < 77,
                  "orange",
                  "green"))
      )
  })

  # extract the short name to match the data
  short_name_geo <- reactive({
    dataColumnChoices[dataColumnChoices$full == input$statGeo, "short"]
  })

  # create the data for the leaflet map
  geoData <- reactive({
    createLeafletData(data = emp())
  })

  # calculate the edges of the map
  geoBounds <- reactive({
    req(input$employData)
    bbox(geoData())
  })

  # render the leaflet map
  output$mapGeo <- renderLeaflet({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    leafMap(
      mapData = geoData(),
      fill = short_name_geo(),
      bounds = geoBounds()
    )
  })

  # on a plot click, display the region's data in a table
  observe({
    event <- input$mapGeo_shape_click
    if (is.null(event)) return()
    output$ladDatGeo <- renderDataTable({
      selectData <-
        data.frame(
          "Statistic" = colnames(geoData()@data)[8:36],
          "Value" = t(
            geoData()@data[geoData()@data$la_code %in% event, 8:36]
          ),
          stringsAsFactors = FALSE
        )
      colnames(selectData) <- c("Statistic", "Value")
      datatable(selectData, rownames = FALSE)
    })
  })

  # extract the short name to match the data
  short_name_hex <- reactive({
    dataColumnChoices[dataColumnChoices$full == input$statHex, "short"]
  })

  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, emp(), by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })

  hexBounds <- reactive({
    req(input$employData)
    bbox(hexData())
  })

  output$mapHex <- renderLeaflet({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    leafMap(
      mapData = hexData(),
      fill = short_name_hex(),
      bounds = hexBounds(),
      hex = TRUE
    )
  })

  # on a plot click, display the region's data in a table
  observe({
    event <- input$mapHex_shape_click
    if (is.null(event)) return()
    output$ladDatHex <- renderDataTable({
      selectData <-
        data.frame(
          "Statistic" = colnames(geoData()@data)[8:36],
          "Value" = t(
            geoData()@data[geoData()@data$la_code %in% event, 8:36]
          ),
          stringsAsFactors = FALSE
        )
      colnames(selectData) <- c("Statistic", "Value")
      datatable(selectData, rownames = FALSE)
    })
  })

  # output the scatter graph
  output$scatFig <- renderPlot({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    compositeScatter(emp(), plotly = FALSE)
  })

}
