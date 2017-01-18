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
  ## Create the geographical map

  # create the data for the leaflet map
  geoData <- reactive({
    createLeafletData(data = emp())
  })

  # create the leaflet map page
  callModule(mapPage, "geo", emp = emp, cols = namesSub,
             data = geoData, hex = FALSE)

#############################################################################
  ## Create the hexagonal map

  # merge the statistics data with the hexagon map data
  hexData <- reactive({
    hexMapJson@data <- dplyr::left_join(
      hexMapJson@data, emp(), by = c("lad15nm" = "la_name")
    )
    hexMapJson
  })

  # create the hexagon map page
  callModule(mapPage, "hex", emp = emp, cols = namesSub,
             data = hexData, hex = TRUE)

#############################################################################
## Create the page for both maps

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

}
