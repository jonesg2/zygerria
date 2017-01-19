header <- dashboardHeader(
  title = "Employment Prospects"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    menuItem("Geographical Map", icon = icon("map-marker"), tabName = "mapGeo"),
    menuItem("Hexagonal Map", icon = icon("map-marker"), tabName = "mapHex"),
    menuItem("Dual Maps", tabName = "maps", icon = icon("map-marker")),
    menuItem("Scatter Graph", icon = icon("line-chart"), tabName = "scat"),
    menuItem("Time Series", icon = icon("line-chart"), tabName = "time"),
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
      tabName = "maps",
      tags$div(twoMapPageInput("two"), class = "tab-pane")
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
            choices = list(
              "Measure A" = dataColumnChoices[c(12, 5:7), "full"],
              "Measure B" = dataColumnChoices[c(24, 13:17), "full"]
            ),
            selected = dataColumnChoices[12, "full"]
          ),
          selectInput(
            "yAxis",
            label = "Choose the y-axis",
            choices = list(
              "Measure A" = dataColumnChoices[c(12, 5:7), "full"],
              "Measure B" = dataColumnChoices[c(24, 13:17), "full"]
            ),
            selected = dataColumnChoices[24, "full"]
          )
        )
      )
    ),
    tabItem(
      tabName = "time",
      column(
        width = 9,
        plotlyOutput("timeseries", height = 500)
      ),
      column(
        width = 3,
        selectizeInput(
          "timeIns",
          label = "Select up to 5 LADs",
          choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
          multiple = TRUE,
          options = list(
            maxItems = 5
          )
        ),
        selectInput(
          "timeVar",
          label = "Select the measure",
          choices = "val"
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
