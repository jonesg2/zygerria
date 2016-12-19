# load the package library
library(employmentProspects)

# shiny libraries
library(shiny)
library(shinydashboard)
library(DT)

# data manipulation libraries
library(dplyr)
library(sp)

# plotting libraries
library(ggplot2)
library(plotly)
library(leaflet)

# select input choices
colChoices <- dataColumnChoices[c(12, 5:7, 24, 13:17), "full"]
