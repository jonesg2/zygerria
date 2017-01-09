#' Input a page for map graphics
#'
#' Input both the hexagonal or geographical maps into a page within the app.
#'
#' @rdname mapPageInput
#'
#' @export
twoMapPageInput <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      leafletOutput(ns("map_geo"), height = 500)
    ),
    column(
      width = 4,
      leafletOutput(ns("map_hex"), height = 500)
    ),
    column(
      width = 4,
      selectInput(
        ns("stat"),
        label = "Select a composite variable",
        choices = list(
          "Measure A" = dataColumnChoices[c(12, 5:7), "full"],
          "Measure B" = dataColumnChoices[c(24, 13:17), "full"]
        ),
        selected = dataColumnChoices[12, "full"]
      ),
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
    ),
    column(
      width = 8,
      conditionalPanel(
        condition = paste0("input['", id, "-lad1'] != '' || ",
                           "input['", id, "-lad2'] != '' || ",
                           "input['", id, "-lad3'] != ''"),
        dataTableOutput(ns("dataTable"))
      )
    )
  )
}
