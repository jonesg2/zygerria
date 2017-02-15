#' Comparison of Composite Variables
#'
#' Create a scatter plot of the composite variables
#'
#' @param data The employment data, see details for more information.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#' @param xLab The x-axis label.
#' @param yLab The y-axis label.
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
compositeScatter <- function(data, x, y, xLab, yLab) {

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

  # Calculate the coordinates for the graph lines
  minH <- min(roundDown(data[, x]), na.rm = TRUE)
  maxH <- max(roundUp(data[, x]), na.rm = TRUE)
  horizontalLine <- c(minH, maxH)
  midX <- c(median(horizontalLine), median(horizontalLine))
  minV <- min(roundDown(data[, y]), na.rm = TRUE)
  maxV <- max(roundUp(data[, y]), na.rm = TRUE)
  verticalLine <- c(minV, maxV)
  midY <- c(median(verticalLine), median(verticalLine))

  p <- plotly::plot_ly(
    symbols = c("circle", "x", "o"),
    colors = pal
  ) %>%
    plotly::add_trace(
      x = horizontalLine,
      y = midY,
      type = "scatter",
      mode = "lines",
      color = I("grey51"),
      showlegend = FALSE
    ) %>%
    plotly::add_trace(
      x = midX,
      y = verticalLine,
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
        "<br>Measure A: ", data[, x], "%",
        "<br>Measure B: ", data[, y], "%"
      ),
      hoverinfo = "text",
      marker = list(
        size = 10
      )
    ) %>%
    plotly::layout(
      xaxis = list(title = xLab),
      yaxis = list(title = yLab),
      legend = list(x = 100, y = 0.5)
    )

  p
}
