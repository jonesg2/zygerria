#' Comparison of Composite Variables
#'
#' Create a scatter plot of the composite variables
#'
#' @param data The employment data, see details for more information.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#' @param colour The column to colour the points by.
#' @param xLab The x-axis label.
#' @param yLab The y-axis label.
#' @param highlight A vector of Local Authority District names to highlight on
#'   the plot.
#' @param discrete Logical. Whether the data are discrete or continuous.
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
compositeScatter <- function(data, x, y, colour, xLab, yLab, highlight = NULL,
                             discrete = TRUE) {

  pal <- if (discrete) {
    c("red", "#EF9C61", "#4575B3")
  } else {
    c("red", "#FFFCB4", "#4575B3")
  }

  # Calculate the coordinates for the graph lines
  maxH <- max(data[, x], na.rm = TRUE)
  horizontalLine <- c(0, maxH)
  midX <- c(median(horizontalLine), median(horizontalLine))
  maxV <- max(data[, y], na.rm = TRUE)
  verticalLine <- c(0, maxV)
  midY <- c(median(verticalLine), median(verticalLine))

  p <- plotly::plot_ly(
    colors = pal
  ) %>%
    plotly::add_trace(# Add horizontal and vertical lines
      x = horizontalLine,
      y = midY,
      type = "scatter",
      mode = "lines",
      color = I("grey51"),
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
    plotly::add_trace(
      x = midX,
      y = verticalLine,
      type = "scatter",
      mode = "lines",
      color = I("grey51"),
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
    plotly::layout(# Define the x and y axis labels, define the legend position
      xaxis = list(title = xLab),
      yaxis = list(title = yLab),
      legend = list(x = 100, y = 0.5)
    )

  colourBar <- if (discrete) {
    FALSE
  } else {
    list(len = 0.5, title = "")
  }

  p <- if (is.null(highlight)) {
    plotly::add_trace(# Add all points to the plot
      p = p,
      data = data,
      type = "scatter",
      mode = "markers",
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      color = as.formula(paste0("~", colour)),
      key = ~la_name,
      text = ~paste0(
        "Region: ", la_name,
        "<br>(", data[, x], "%, ", data[, y], "%)"
      ),
      hoverinfo = "text",
      marker = list(
        size = 10,
        colorbar = colourBar
      ),
      showlegend = ifelse(discrete == TRUE, TRUE, FALSE)
    )
  } else {
    plotly::add_trace(# When regions are selected, highlight them on the plot
      p = p,
      data = data[!(data$la_name %in% highlight), ],
      type = "scatter",
      mode = "markers",
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      color = as.formula(paste0("~", colour)),
      key = ~la_name,
      opacity = 0.3,
      text = ~paste0(
        "Region: ", la_name,
        "<br>(", data[!(data$la_name %in% highlight), x], "%, ",
        data[!(data$la_name %in% highlight), y], "%)"
      ),
      hoverinfo = "text",
      marker = list(
        size = 10,
        colorbar = colourBar
      ),
      showlegend = ifelse(discrete == TRUE, TRUE, FALSE)
    ) %>%
      plotly::add_trace(
        data = data[data$la_name %in% highlight, ],
        type = "scatter",
        mode = "markers",
        x = as.formula(paste0("~", x)),
        y = as.formula(paste0("~", y)),
        color = as.formula(paste0("~", colour)),
        key = ~la_name,
        text = ~paste0(
          "Region: ", la_name,
          "<br>(", data[data$la_name %in% highlight, x], "%, ",
          data[data$la_name %in% highlight, y], "%)"
        ),
        hoverinfo = "text",
        marker = list(
          size = 10,
          line = list(color = "black", width = 2),
          colorbar = colourBar
        ),
        showlegend = FALSE
      )
  }

  p
}
