header <- dashboardHeader(
  title = "Employment Prospects"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    menuItem("Geographical Map", icon = icon("map-marker"), tabName = "mapGeo"),
    menuItem("Hexagonal Map", icon = icon("map-marker"), tabName = "mapHex"),
    menuItem("Scatter Graph", icon = icon("line-chart"), tabName = "scat"),
    # upload the employment statistics data
    fileInput(
      "employData",
      label = "Upload the employment data"
    )
  )
)

body <- dashboardBody(
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
      tabName = "mapGeo",
      tags$div(mapPageInput("geo"), class = "tab-pane")
    ),
    tabItem(
      tabName = "mapHex",
      tags$div(mapPageInput("hex"), class = "tab-pane")
    ),
    tabItem(
      tabName = "scat",
      column(
        width = 9,
        box(
          width = NULL,
          height = 525,
          plotlyOutput("scatFig", height = 500)
        )
      ),
      column(
        width = 3,
        box(
          width = NULL,
          selectInput(
            "xAxis",
            label = "Choose the x-axis",
            choices = dataColumnChoices[c(12, 5:7, 24, 13:17), "short"],
            selected = "measure_a"
          ),
          selectInput(
            "yAxis",
            label = "Choose the y-axis",
            choices = dataColumnChoices[c(12, 5:7, 24, 13:17), "short"],
            selected = "measure_b"
          )
        )
      )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body,
  tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css", href = "custom.css"
    )
  )
)
