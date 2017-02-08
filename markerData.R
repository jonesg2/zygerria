#' @export
markerData <- function(data, lads = NULL) {
  if (is.null(lads)) {
    return(NULL)
  } else {
    ladDat <- data[data@data$lad15nm %in% lads, ]
    ladDat <- data.frame(
      lng = do.call("c", lapply(ladDat@polygons, function(.) .@labpt[1])),
      lat = do.call("c", lapply(ladDat@polygons, function(.) .@labpt[2]))
    )
    ladDat
  }
}
