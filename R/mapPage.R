#' Server side functionality for maps pages
#'
#' This function works with the \code{\link{mapPageInput}} UI function. It
#' provides the server functionality.
#'
#' @param input Shiny parameter.
#' @param output Shiny parameter.
#' @param session Shiny parameter.
#' @param emp The csv input data.
#' @param data The data for the map graphic.
#' @param cols The names of the columns to display in the datatable.
#' @param hex Logical; whether to produce a hexagonal map (\code{TRUE}) or a
#'   geographical map (\code{FALSE}).
#'
#' @author Nathan Eastwood
#'
#' @export
mapPage <- function(input, output, session, emp, data, cols, hex = TRUE) {

  # extract the short name to match the data
  short_name <- reactive({
    dataColumnChoices[dataColumnChoices$full == input$stat, "short"]
  })

  # render the leaflet map
  output$map <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = data(),
      fill = short_name(),
      hex = hex
    )
  })

  # on selection of an lad, place a marker on the map
  proxy <- leafletProxy("map")

  observe({
    eventLad1 <- input$lad1 != ""
    if (eventLad1) {
      lad1Dat <- data()[data()@data$lad15nm %in% input$lad1, ]
      lad1Dat <- data.frame(lng = lad1Dat@polygons[[1]]@labpt[1],
                            lat = lad1Dat@polygons[[1]]@labpt[2])
      proxy %>% addMarkers(lng = ~lng, lat = ~lat, data = lad1Dat,
                           layerId = "lad1")
    }
  })
  observe({
    eventLad2 <- input$lad2 != ""
    if (eventLad2) {
      lad2Dat <- data()[data()@data$lad15nm %in% input$lad2, ]
      lad2Dat <- data.frame(lng = lad2Dat@polygons[[1]]@labpt[1],
                            lat = lad2Dat@polygons[[1]]@labpt[2])
      proxy %>% addMarkers(lng = ~lng, lat = ~lat, data = lad2Dat,
                           layerId = "lad2")
    }
  })
  observe({
    eventLad3 <- input$lad3 != ""
    if (eventLad3) {
      lad3Dat <- data()[data()@data$lad15nm %in% input$lad3, ]
      lad3Dat <- data.frame(lng = lad3Dat@polygons[[1]]@labpt[1],
                            lat = lad3Dat@polygons[[1]]@labpt[2])
      proxy %>% addMarkers(lng = ~lng, lat = ~lat, data = lad3Dat,
                           layerId = "lad3")
    }
  })

  # on a plot click, display the region's data in a table
  observe({
    event <- input$map_shape_click
    if (is.null(event)) return()
    output$dataTable <- renderDataTable({
      selectData <-
        data.frame(
          "Statistic" = cols(),
          "Value" = t(
            data()@data[data()@data$la_code %in% event, cols()]
          ),
          stringsAsFactors = FALSE
        )
      colnames(selectData) <- c("Statistic", "Value")
      datatable(selectData, rownames = FALSE)
    })
  })

  # reset region selection inputs on a button click
  observeEvent(input$clearSelection, {
    if (input$lad1 != "") {
      updateSelectInput(
        session,
        "lad1",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy %>% removeMarker("lad1")
    }
    if (input$lad2 != "") {
      updateSelectInput(
        session,
        "lad2",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy %>% removeMarker("lad2")
    }
    if (input$lad3 != "") {
      updateSelectInput(
        session,
        "lad3",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
      )
      proxy %>% removeMarker("lad3")
    }
  })

}
