#' Extract the hexagon map data
#'
#' Extract the data from the svg file to produce the local area district map of
#' the UK.
#'
#' @author Nathan Eastwood, Douglas Ashton
#'
#' @importFrom xml2 read_xml xml_find_all
#' @export
createHexMapData <- function() {
  file <- system.file("data-raw", "hexmap.xml", package = "employmentProspects")
  x <- xml2::read_xml(file)
  xlist <- xml2::xml_find_all(x, "path")
  dlist <- lapply(xlist, getAttr)
  alldf <- do.call("rbind", dlist)
  alldf
}

#' Extract individual hexagon data
#'
#' For each hexagon, extract the corner points (coordinates), the region ID and
#' the region name.
#'
#' @param x A single element of the list extracted from the SVG.
#'
#' @author Nathan Eastwood, Douglas Ashton
#'
#' @importFrom xml2 xml_attr
getAttr <- function(x) {
  d <- xml2::xml_attr(x, "d")
  d <- unlist(strsplit(d, split = "[A-Z]"))
  d <- matrix(as.numeric(unlist(strsplit(d[-1], split = ","))), ncol = 2,
              byrow = TRUE)
  colnames(d) <- c("X", "Y")
  d <- as.data.frame(d)
  data.frame(
    long = d$X,
    lat = -d$Y,
    lad15cd =  as.character(gsub("reg", "", xml2::xml_attr(x, "id"))),
    lad15nm = as.character(xml2::xml_attr(x, "data-nm")),
    stringsAsFactors = FALSE
  )#,
  # Value = as.numeric(xml2::xml_attr(x, "data_val")),
  # Boundary = sample(c(TRUE, FALSE), size = 6, replace = TRUE),
  # = substr(xml2::xml_attr(x, "id"), 4, 6))
}
