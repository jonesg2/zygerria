header <- dashboardHeader(title = "Local Authority Map")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    # upload the employment statistics data
    fileInput(
      "employData",
      label = "Upload the employment data"

    ),
    menuItem("Leaflet Map", icon = icon("envira"), tabName = "leaf"),
    menuItem("Hex Map", icon = icon("map-marker"), tabName = "hex")
  )
)

body <- dashboardBody(
  includeCSS("www/custom.css"),
  tabItems(
    # Welcome page content
    tabItem(
      tabName = "info",
      fluidRow(
        column(
          width = 12,
          includeMarkdown("welcome.md")
        )
      )
    ),
    tabItem(
      tabName = "leaf",
      column(
        width = 12,
        box(
          width = 6,
          # offer a choice of statistics
          selectInput(
            "colLeaf",
            label = "Select a statistic",
            choices = dataColumnChoices
          )
        )
      ),
      column(
        width = 12,
        box(
          width = 6,
          height = 600,
          leafletOutput("leafMap", height = 575)
        ),
        box(
          width = 6,
          height = 600,
          dataTableOutput("ladDat")
        )
      )
    ),
    tabItem(
      tabName = "hex",
      column(
        width = 12,
        box(
          width = 6,
          # offer a choice of statistics
          selectInput(
            "colHex",
            label = "Select a statistic",
            choices = dataColumnChoices
          )
        )
      ),
      column(
        width = 12,
        box(
          width = 6,
          height = 600,
          plotlyOutput("hexMap", height = "600px")
        )
      )
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black")
