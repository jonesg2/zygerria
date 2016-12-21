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
             check.names = FALSE) %>%
      mutate(
        emp_rate_hml = if_else(
          emp_rate < 73, "red",
          if_else(emp_rate >= 73 & emp_rate < 77,
                  "orange",
                  "green"))
      )
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
  callModule(mapPage, "geo", emp = empInput, cols = namesSub,
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
  callModule(mapPage, "hex", emp = empInput, cols = namesSub,
             data = hexData, hex = TRUE)

#############################################################################
  ## Create the scatter plot

  # output the scatter graph
  output$scatFig <- renderPlot({
    validate(
      need(input$employData, "Please upload employment statistics")
    )
    compositeScatter(emp(), plotly = FALSE)
  })

}
