header <- dashboardHeader(
  title = "Employment Prospects"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    menuItem("Map", icon = icon("map-marker"), tabName = "map"),
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
      tabName = "map",
      fluidRow(
        column(
          width = 9,
          box(
            height = 500,
            width = NULL,
            solidHeader = TRUE,
            leafletOutput("map", height = 500)
          ),
          box(
            width = NULL,
            dataTableOutput("ladDat")
          )
        ),
        column(
          width = 3,
          box(
            width = NULL,
            radioButtons(
              "plotType",
              label = "Choose a plot type",
              choices = c("Geographical" = "geo", "Hexagonal" = "hex"),
              selected = "geo",
              inline = TRUE
            )
          ),
          box(
            width = NULL,
            selectInput(
              "stat",
              label = "Select a composite variable",
              choices = list(
                "Measure A" = dataColumnChoices[c(12, 5:7), "full"],
                "Measure B" = dataColumnChoices[c(24, 13:17), "full"]
              ),
              selected = dataColumnChoices[12, "full"]
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "scat",
      column(
        width = 12,
        box(
          width = 6,
          height = 600,
          plotOutput("scatFig", height = 600, click = "plot_click")
        )
      )
    )
  )
)

dashboardPage(
  header,
  sidebar,
  body
)
