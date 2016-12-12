server <- function(input, output) {

  # load in the statistics data
  empStat <- reactive({
    if (is.null(input$employData)) return(NULL)
    read.csv(input$employData$datapath,
             stringsAsFactors = FALSE,
             check.names = FALSE)
  })

  # create the data for the leaflet map
  leafData <- reactive({
    createLeafletData(data = empStat())
  })

  # calculate the edges of the map
  bounds <- reactive({
    req(input$employData)
    bbox(leafData())
  })

  # render the leaflet map
  output$leafMap <- renderLeaflet({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    leafMap(mapData = leafData(), fill = input$colLeaf, bounds = bounds())
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
            leafData()@data[leafData()@data$`LA Code` %in% event, 8:36]
          ),
          stringsAsFactors = FALSE
        )
      colnames(selectData) <- c("Statistic", "Value")
      datatable(selectData, rownames = FALSE)
    })
  })

  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    dplyr::left_join(hexMapData, empStat(), by = c("lad15nm" = "LA Name"))
  })

  # output the hex map
  output$hexMap <- renderPlotly({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    plotHexMap(hexData(), stat = paste0("`", input$colHex, "`"))
  })
}
