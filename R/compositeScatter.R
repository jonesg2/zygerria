#' Comparison of Composite Variables
#'
#' Create a scatter plot of the composite variables
#'
#' @param data The employment data, see details for more information.
#' @param plotly Logical; defaults to TRUE. Whether to return the plot as an
#'   interactive plotly graphic or not.
#'
#' @details
#' The \code{data} needs to contain \code{measureA}, \code{measureB} and the
#' employment rate statistics, where employment rate is defined as a categorical
#' variable; red is <73%, amber is 73-77% and green is >77%.
#'
#' @author Nathan Eastwood
#'
#' @importFrom ggplot2 ggplot geom_vline geom_hline geom_point aes
#'   scale_colour_manual scale_shape_manual xlim ylim labs coord_fixed theme_bw
#' @importFrom plotly ggplotly
#'
#' @export
compositeScatter <- function (data, plotly = TRUE) {
  p <-
    ggplot2::ggplot(data) +
    ggplot2::geom_vline(xintercept = 50) +
    ggplot2::geom_hline(yintercept = 50) +
    ggplot2::geom_point(
      ggplot2::aes(x = measure_a,
                   y = measure_b,
                   colour = emp_rate_hml,
                   shape = emp_rate_hml),
      size = 2
    ) +
    ggplot2::scale_colour_manual(
      "Current\nEmployment\nRate",
      values = c("red" = "red",
                 "orange" = "#FFC200",
                 "green" = "green3"),
      labels = c("red" = "<73%",
                 "orange" = "73-77%",
                 "green" = ">77%")
    ) +
    ggplot2::scale_shape_manual(
      "Current\nEmployment\nRate",
      values = c("red" = 15,
                 "orange" = 18,
                 "green" = 17),
      labels = c("red" = "<73%",
                 "orange" = "73-77%",
                 "green" = ">77%")
    ) +
    ggplot2::xlim(c(0, 100)) +
    ggplot2::ylim(c(0, 100)) +
    ggplot2::labs(x = "Fragility of Current Jobs",
                  y = "Conditions for Future Jobs Growth") +
    ggplot2::coord_fixed() +
    ggplot2::theme_bw()

  if (plotly) {
    plotly::ggplotly(p, tooltip = c("x", "y"), source = "compscat")
  } else {
    p
  }
}
