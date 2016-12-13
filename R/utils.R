#' Round up to nearest number
#'
#' @author Nathan Eastwood
roundUp <- function(x, to = 10) {
  to * (x %/% to + as.logical(x %% to))
}

#' Round down to nearest 10
#'
#' @author Nathan Eastwood
roundDown <- function(x) {
  floor(x / 10) * 10
}
