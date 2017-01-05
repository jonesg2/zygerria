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
        width = 12,
        box(
          width = 12,
          height = 525,
          plotlyOutput("scatFig", height = 500)
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
