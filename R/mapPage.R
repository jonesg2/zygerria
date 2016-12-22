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

  # calculate the edges of the map
  bounds <- reactive({
    req(emp())
    bbox(data())
  })

  # render the leaflet map
  output$map <- renderLeaflet({
    validate(
      need(emp(), "Please upload employment statistics")
    )
    leafMap(
      mapData = data(),
      fill = short_name(),
      bounds = bounds(),
      hex = hex
    )
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
    updateSelectInput(
      session,
      "lad1",
      choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
    )
    updateSelectInput(
      session,
      "lad2",
      choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
    )
    updateSelectInput(
      session,
      "lad3",
      choices = c(Choose = "", sort(unique(shape@data$lad15nm)))
    )
  })

}
