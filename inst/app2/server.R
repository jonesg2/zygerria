server <- function(input, output){

  # extract the short name to match the data
  short_name <- reactive({
    dataColumnChoices[dataColumnChoices$full == input$stat, "short"]
  })

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

  # create the data for the leaflet map
  geoData <- reactive({
    createLeafletData(data = emp())
  })

  # calculate the edges of the map
  geoBounds <- reactive({
    req(input$employData)
    bbox(geoData())
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

  # render the leaflet map
  output$map <- renderLeaflet({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    switch(
      input$plotType,
      "geo" =
        leafMap(
          mapData = geoData(),
          fill = short_name(),
          bounds = geoBounds()
        ),
      "hex" =
        leafMap(
          mapData = hexData(),
          fill = short_name(),
          bounds = hexBounds(),
          hex = TRUE
        )
    )
  })


  # on a plot click, display the region's data in a table
  observe({
    event <- input$map_shape_click
    if (is.null(event)) return()
    output$ladDat <- renderDataTable({
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
