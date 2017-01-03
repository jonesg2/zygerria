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
  ns <- NS(id)

  fluidRow(
    column(
      width = 9,
      box(
        height = 525,
        width = NULL,
        solidHeader = TRUE,
        leafletOutput(ns("map"), height = 500)
      ),
      conditionalPanel(
        condition = paste0("input['", id, "-lad1'] != '' || ",
                           "input['", id, "-lad2'] != '' || ",
                           "input['", id, "-lad3'] != ''"),
        box(
          width = NULL,
          dataTableOutput(ns("dataTable"))
        )
      )
    ),
    column(
      width = 3,
      box(
        width = NULL,
        selectInput(
          ns("stat"),
          label = "Select a composite variable",
          choices = list(
            "Measure A" = dataColumnChoices[c(12, 5:7), "full"],
            "Measure B" = dataColumnChoices[c(24, 13:17), "full"]
          ),
          selected = dataColumnChoices[12, "full"]
        )
      ),
      box(
        width = NULL,
        selectInput(
          ns("lad1"),
          label = "LAD 1",
          choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
          selectize = TRUE
        ),
        selectInput(
          ns("lad2"),
          label = "LAD 2",
          choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
          selectize = TRUE
        ),
        selectInput(
          ns("lad3"),
          label = "LAD 3",
          choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
          selectize = TRUE
        ),
        actionButton(
          ns("clearSelection"),
          "Clear Selection(s)",
          icon = icon("trash-o")
        )
      )
    )
  )
}
