#' Round up to nearest number
#'
#' @param x A vector.
#' @param to The nearest digit to round up to.
#'
#' @author Nathan Eastwood
roundUp <- function(x, to = 10) {
  to * (x %/% to + as.logical(x %% to))
}

#' Round down to nearest 10
#'
#' @param x A vector
#'
#' @author Nathan Eastwood
roundDown <- function(x) {
  floor(x / 10) * 10
}
