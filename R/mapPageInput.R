#' Input a page for map graphics
#'
#' Input either the hexagonal or geographical maps into a page within the app.
#'
#' @param id The id for the module
#'
#' @author Nathan Eastwood
#'
#' @export
mapPageInput <- function(id) {
  ns <- shiny::NS(id)

  #shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shinydashboard::box(
          height = 500,
          width = NULL,
          solidHeader = TRUE,
          leaflet::leafletOutput(ns("map"), height = 500)
        ),
        shinydashboard::box(
          width = NULL,
          DT::dataTableOutput(ns("dataTable"))
        )
      ),
      shiny::column(
        width = 3,
        shinydashboard::box(
          width = NULL,
          shiny::selectInput(
            ns("stat"),
            label = "Select a composite variable",
            choices = list(
              "Measure A" = dataColumnChoices[c(12, 5:7), "full"],
              "Measure B" = dataColumnChoices[c(24, 13:17), "full"]
            ),
            selected = dataColumnChoices[12, "full"]
          )
        )
      )
  #  )
  )
}
