#' @export
mapChoicesUI <- function(id) {
  ns <- NS(id)
  column(
    width = 3,
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
      ns("mapType"),
      label = "Choose a map type",
      choices = c("Hexagonal" = "hex", "Geographical" = "geo"),
      selected = "hex",
      inline = TRUE
    ),
    radioButtons(
      ns("fillType"),
      label = "Choose a fill type",
      choices = c("Continuous" = "cont", "Quantiles" = "quint"),
      selected = "cont",
      inline = TRUE
    )
  )
}
