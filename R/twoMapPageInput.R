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
      radioButtons(
        ns("fillType"),
        label = "Choose a fill type",
        choices = c("Continuous" = "cont", "Quantiles" = "quint"),
        selected = "cont",
        inline = TRUE
      ),
      selectizeInput(
        ns("ladSel"),
        label = "Select up to 5 LADs",
        choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
        multiple = TRUE,
        options = list(
          maxItems = 5
        )
      ),
      actionButton(
        ns("clearSelection"),
        "Clear Selection(s)",
        icon = icon("trash-o")
      ),
      uiOutput(ns("ladInfo"))
    ),
    column(
      width = 8,
      conditionalPanel(
        condition = paste0("input['", id, "-ladSel'] != ''"),
        dataTableOutput(ns("dataTable"))
      )
    )
  )
}
