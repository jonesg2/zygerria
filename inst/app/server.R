server <- function(input, output) {

  # load in the statistics data
  empStat <- reactive({
    if (is.null(input$employData)) return(NULL)
    read.csv(input$employData$datapath,
             stringsAsFactors = FALSE,
             check.names = FALSE) %>%
      mutate(
        empRate = if_else(
          `Employment rate (%)` < 73, "red",
          if_else(`Employment rate (%)` >= 73 & `Employment rate (%)` < 77,
                  "orange",
                  "green"))
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
    leafMap(mapData = leafData(), fill = input$stat, bounds = boundsLeaf())
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
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, empStat(), by = c("lad15nm" = "LA Name")
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
    leafMap(hexData(), fill = input$stat, bounds = boundsHex(), hex = TRUE)
  })

  # output the scatter graph
  output$scatFig <- renderPlot({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    compositeScatter(empStat(), plotly = FALSE)
  })

  output$test <- renderTable({
    res <- nearPoints(empStat(), input$plot_click,
                      "`Measure A (Total / (3*379) *100)`",
                      "`Measure B (Total / (5*379)*100)`")
    if (nrow(res) == 0) return()
    res
  })
}
