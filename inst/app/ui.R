header <- dashboardHeader(title = "Local Authority Map")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Information", tabName = "info", icon = icon("info-circle")),
    menuItem("Hex Map", icon = icon("map-marker"), tabName = "map"),
    menuItem("Scatter Graph", icon = icon("line-chart"), tabName = "scat"),
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
          width = 6,
          selectInput(
            "stat",
            label = "Select a composite variable",
            choices = c("Measure A", "Measure B")
          )
        ),
        box(
          width = 6,
          uiOutput("underlying")
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
            leafletOutput("hexMap", height = 600)
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
    ),
    tabItem(
      tabName = "scat",
      column(
        width = 12,
        box(
          width = 6,
          height = 600,
          plotOutput("scatFig", height = 600, click = "plot_click")
        ),
        box(
          width = 6,
          tableOutput("test")
        )
      )
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black")
