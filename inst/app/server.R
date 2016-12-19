server <- function(input, output) {

  # extract the short name to match the data
  short_name <- reactive({
    dataColumnChoices[dataColumnChoices$full == input$measure, "short"]
  })

  # load in the statistics data
  empStat <- reactive({
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

  # create a dynamic ui for the statistical data to be shown
  output$underlying <- renderUI({
    switch(
      input$stat,
      "Measure A" = selectInput(
        "measure", "Select the underlying variable",
        choices = dataColumnChoices[c(12, 5:7), "full"],
        selected = dataColumnChoices[12, "full"]
      ),
      "Measure B" = selectInput(
        "measure", "Select the underlying variable",
        choices = dataColumnChoices[c(24, 13:17), "full"],
        selected = dataColumnChoices[24, "full"]
      )
    )
  })

  # create the data for the leaflet map
  leafData <- reactive({
    createLeafletData(data = empStat())
  })

  # calculate the edges of the map
  boundsLeaf <- reactive({
    req(input$employData)
    bbox(leafData())
  })

  # render the leaflet map
  output$leafMap <- renderLeaflet({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    leafMap(mapData = leafData(), fill = short_name(), bounds = boundsLeaf())
  })

  # on a plot click, display the region's data in a table
  observe({
    event <- input$leafMap_shape_click
    if (is.null(event)) return()
    output$ladDat <- renderDataTable({
      selectData <-
        data.frame(
          "Statistic" = colnames(leafData()@data)[8:36],
          "Value" = t(
            leafData()@data[leafData()@data$la_code %in% event, 8:36]
          ),
          stringsAsFactors = FALSE
        )
      colnames(selectData) <- c("Statistic", "Value")
      datatable(selectData, rownames = FALSE)
    })
  })

  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, empStat(), by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })

  boundsHex <- reactive({
    req(input$employData)
    bbox(hexData())
  })

  # output the hex map
  output$hexMap <- renderLeaflet({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    leafMap(hexData(), fill = short_name(), bounds = boundsHex(), hex = TRUE)
  })

  # output the scatter graph
  output$scatFig <- renderPlot({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    compositeScatter(empStat(), plotly = FALSE)
  })

  output$test <- renderTable({
    res <- nearPoints(empStat(), input$plot_click, "measure_a", "measure_b")
    if (nrow(res) == 0) return()
    res
  })
}
