server <- function(input, output) {

  # create the data for the leaflet map
  leafData <- reactive({
    if (is.null(input$employData)) return(NULL)
    empData <- read.csv(input$employData$datapath,
                        stringsAsFactors = FALSE,
                        check.names = FALSE)
    createLeafletData(data = empData)
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
}
