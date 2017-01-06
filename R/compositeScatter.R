#' Comparison of Composite Variables
#'
#' Create a scatter plot of the composite variables
#'
#' @param data The employment data, see details for more information.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#'
#' @details
#' The \code{data} needs to contain \code{measureA}, \code{measureB} and the
#' employment rate statistics.
#'
#' @author Nathan Eastwood
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats setNames
#' @importFrom dplyr mutate if_else
#'
#' @export
compositeScatter <- function(data, x, y) {

  data <- data %>%
    mutate(
      emp_rate_hml = if_else(
        emp_rate < 73, "red",
        if_else(emp_rate >= 73 & emp_rate < 78,
                "orange",
                "green"))
    )

  pal <- c("green3", "#FFC200", "red")
  pal <- setNames(pal, c("green", "orange", "red"))

  p <- plotly::plot_ly(
    symbols = c("circle", "x", "o"),
    colors = pal
  ) %>%
    plotly::add_trace(
      x = c(0, 100),
      y = c(50, 50),
      type = "scatter",
      mode = "lines",
      color = I("grey51"),
      showlegend = FALSE
    ) %>%
    plotly::add_trace(
      x = c(50, 50),
      y = c(0, 100),
      type = "scatter",
      mode = "lines",
      color = I("grey51"),
      showlegend = FALSE
    ) %>%
    plotly::add_trace(
      data = data,
      type = "scatter",
      mode = "markers",
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      color = ~emp_rate_hml,
      symbol = ~emp_rate_hml,
      text = ~paste0(
        "Region: ", la_name,
        "<br>Measure A: ", x, "%",
        "<br>Measure B: ", y, "%"
      ),
      hoverinfo = "text",
      marker = list(
        size = 10
      )
    ) %>%
    plotly::layout(
      xaxis = list(title = "Fragility of Current Jobs"),
      yaxis = list(title = "Conditions for Future Jobs Growth"),
      legend = list(x = 100, y = 0.5)
    )

  p
}
