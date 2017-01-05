#' Shapefile data for the UK
#'
#' Ultra Generalised Clipped Shapefile data for the Local Authority Districts
#' (LAD) in the UK from December 2015.
#'
#' @docType data
#'
#' @usage data(shape)
#'
#' @format As S4 object of class \code{"sp"}; see \code{\link[sp]{sp}}.
#'
#' @source \href{http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_4}{ONS Geoportal}
"shape"

#' UK Employment Statistics Choices
#'
#' Choices of UK Employment Statistics available in the data. This data set is
#' used within the Shiny App and maps the full column names to their short hand
#' counter parts.
#'
#' @docType data
#'
#' @usage data(dataColumnChoices)
#'
#' @format A \code{data.frame} wth 31 observations and 2 columns.
"dataColumnChoices"

#' UK Hex Map Data
#'
#' The LAD data and coordinates for Local Area Districts (LADs) broken down into
#' equally sized hexagons. The data are of class
#' \code{SpatialPolygonsDataFrame}.
#'
#' @docType data
#'
#' @usage data(hexMapData)
#'
#' @format A \code{list} of length 391 - one for each LAD.
"hexMapJson"
