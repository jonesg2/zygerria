server <- function(input, output, session){

#############################################################################
  ## Load in Data

  # make the csv file path reactive for use in the modules
  empInput <- reactive({
    input$employData
  })

  # load in the statistics data
  emp <- reactive({
    if (is.null(empInput())) return(NULL)
    read.csv(empInput()$datapath,
             stringsAsFactors = FALSE,
             check.names = FALSE)
  })

  # extract the data column names
  namesSub <- reactive({
    colnames(emp())[!(colnames(emp()) %in% c("la_code", "la_name"))]
  })

#############################################################################
## Create the page for both maps

  # create the data for the leaflet map
  geoData <- reactive({
    createLeafletData(data = emp())
  })
  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, emp(), by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })
  callModule(twoMapPage, "two", emp = emp, cols = namesSub,
             geodata = geoData, hexdata = hexData)


#############################################################################
  ## Create the scatter plot

  # output the scatter graph
  output$scatFig <- renderPlotly({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    compositeScatter(
      emp(),
      x = dataColumnChoices[dataColumnChoices$full %in% input$xAxis, "short"],
      y = dataColumnChoices[dataColumnChoices$full %in% input$yAxis, "short"]
    )
  })

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
