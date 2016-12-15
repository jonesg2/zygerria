header <- dashboardHeader(title = "Local Authority Map")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    menuItem("Hex Map", icon = icon("map-marker"), tabName = "map"),
    # upload the employment statistics data
    fileInput(
      "employData",
      label = "Upload the employment data"
    )
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
      tabName = "map",
      column(
        width = 12,
        box(
          # offer a choice of statistics
          selectInput(
            "stat",
            label = "Select a statistic",
            choices = dataColumnChoices
          )
        )
      ),
      column(
        width = 12,
        tabBox(
          width = 6,
          selected = "Hex Map",
          height = 600,
          tabPanel(
            "Hex Map",
            plotlyOutput("hexMap", height = 600)
          ),
          tabPanel(
            "Leaf Map",
            leafletOutput("leafMap", height = 575)
          )
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
