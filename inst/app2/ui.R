ui <- navbarPage(
  "Employment Prospects",
  tabPanel(
    "Introduction",
    fluidRow(
      column(
        width = 12,
        includeHTML("welcome.html")
      )
    )
  ),
  tabPanel(
    "Maps",
    fluidRow(
      column(width = 12,
             tags$div(p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
                        etdolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
                        aliquip ex ea commodo consequat. Duisaute irure dolor in reprehenderit in voluptate velit esse
                        cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa
                        qui officia deserunt mollit anim id est laborum.", align = "justify")))),
    fluidRow(
      tags$div(mapChoicesUI("mapOneChoices"), class = "tab-pane"),
      tags$div(mapChoicesUI("mapTwoChoices"), class = "tab-pane"),
      column(
        width = 3,
        selectizeInput(
          "ladSel",
          label = "Select up to 5 LADs",
          choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
          multiple = TRUE,
          options = list(
            maxItems = 5
          )
        ),
        actionButton(
          "clearSelection",
          "Clear Selection(s)",
          icon = icon("trash-o")
        )
      )
    ),
    fluidRow(
      tags$div(appMapInput("mapOne"), class = "tab-pane"),
      tags$div(appMapInput("mapTwo"), class = "tab-pane")
    ),
    br(),
    fluidRow(
      column(width = 12,
             tags$div(p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
                        etdolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
                        aliquip ex ea commodo consequat. Duisaute irure dolor in reprehenderit in voluptate velit esse
                        cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa
                        qui officia deserunt mollit anim id est laborum.", align = "justify")))),
    fluidRow(
      column(
        width = 6,
        selectInput(
          "scatColour",
          label = "Choose a statistic",
          choices = dataColumnChoices[c(3, 25:53), "full"],
          selected = dataColumnChoices[3, "full"]
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          plotlyOutput("scatFig", height = 500)
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          conditionalPanel(
            condition = "input$ladSel != ''",
            dataTableOutput("dataTable"),
            downloadButton("downloadData", "Download.csv")
          )
        )
      )
    )
  ),
  tabPanel(
    "Time Series",
    fluidRow(
      column(width = 12,
             tags$div(p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore
                        etdolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
                        aliquip ex ea commodo consequat. Duisaute irure dolor in reprehenderit in voluptate velit esse
                        cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa
                        qui officia deserunt mollit anim id est laborum.", align = "justify")))),
    fluidRow(
      column(
        width = 6,
        selectizeInput(
          "timeIns",
          label = "Select up to 5 LADs",
          choices = c(Choose = "", sort(unique(shape@data$lad15nm))),
          multiple = TRUE,
          options = list(
            maxItems = 5
          )
        )
      ),
      column(
        width = 6,
        selectInput(
          "timeVar",
          label = "Select the measure",
          choices = "Employment rate (%)"
        )
      ),
      column(
        width = 12,
        plotlyOutput("timeseries", height = 500)
      )
    )
  ),
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)
