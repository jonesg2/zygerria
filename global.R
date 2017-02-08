# load data files
load("dataColumnChoices.rda")
load("emp.rda")
load("empTime.rda")
load("hexMapJson.rda")
load("shape.rda")

# source .R scripts
source("appMap.R")
source("appMapInput.R")
source("calculateQuintiles.R")
source("columnMeans.R")
source("compositeScatter.R")
source("createLeafletData.R")
source("leafletMap.R")
source("mapChoicesUI.R")
source("markerData.R")
source("timeSeriesPlot.R")
source("utils.R")

# shiny libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)

# data manipulation libraries
library(dplyr)
library(sp)

# plotting libraries
library(plotly)
library(leaflet)
