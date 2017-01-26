ui <- navbarPage(
  "Employment Prospects",
  tabPanel(
    "Introduction",
    fluidRow(
      column(
        width = 12,
        includeMarkdown("welcome.md")
      )
    )
  ),
  tabPanel(
    "Maps",
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
      ),
      column(
        width = 3,
        fileInput(
          "employData",
          label = "Upload the employment data"
        )
      )
    ),
    fluidRow(
      tags$div(appMapInput("mapOne"), class = "tab-pane"),
      tags$div(appMapInput("mapTwo"), class = "tab-pane")
    ),
    br(),
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
            dataTableOutput("dataTable")
          )
        )
      )
    )
  ),
  tabPanel(
    "Time Series",
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
