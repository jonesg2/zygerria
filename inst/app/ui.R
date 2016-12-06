header <- dashboardHeader(title = "Local Authority Map")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    #menuItem("Map", icon = icon("map-marker"), tabName = "map"),
    menuItem("Leaflet Map", icon = icon("envira"), tabName = "leaf")
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
    # plotly map
    # tabItem(
    #   tabName = "map",
    #   fluidRow(
    #     selectInput(
    #       "colSelect",
    #       label = "Select a statistic",
    #       choices = colnames(ladData)[-c(1, 2)]
    #     ),
    #     column(
    #       width = 12,
    #       plotlyOutput(outputId = "ly_map", width = 520, height = 820)
    #     )
    #   )
    # ),
    # leaflet map
    tabItem(
      tabName = "leaf",
      column(
        width = 12,
        # upload the employment statistics data
        box(
          width = 6,
          fileInput(
            "employData",
            label = "Upload the employment data"

          )
        ),
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
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black")
