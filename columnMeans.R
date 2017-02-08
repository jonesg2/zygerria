#' Column Means
#'
#' Return the column means of a dataframe or matrix as a dataframe.
#'
#' @param data A matrix or dataframe.
#' @param na.rm Logical. Should missing values (including NaN) be omitted from
#'   the calculations?
#' @param ... Additional arguments to be passed to \code{\link[base]{colMeans}}.
#'
#' @author Nathan Eastwood
#'
#' @export
columnMeans <- function(data, na.rm = TRUE, ...) {
  means <- round(colMeans(data, na.rm = na.rm, ...), 2)
  data.frame(measure = names(means), Mean = means)
}
