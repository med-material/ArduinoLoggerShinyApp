library(shiny)
library(plotly)
library(shinyjs)

ui <- fluidPage(
  
  includeCSS("custom.css"),
  useShinyjs(),
  fluidRow(
    column(8, titlePanel("Arduino Logger Visualizer (Cohort 2020)")),
    column(4,
           column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib="font-awesome")),
           column(11,style = "margin-top : 20px; text-align: center;",
            selectInput("emailSelect", NULL, choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Email", tablename = "synch")),
           )
    )
  ),
  strong("CHOOSE EXERCISE"),
  tabsetPanel(id = "tabs", type = "tabs",
    tabPanel(id = "reactpan", strong("ReactionTime"),
        navlistPanel(
          widths = c(4, 8),
          "Choose Visualization",
          tabPanel("Reaction Time per Trial",
              plotlyOutput("rtTrialPlot"),
          ),
          tabPanel("Reaction Time based on Intensity",
              plotlyOutput("rtIntensityPlot"),
          )
        )
    ),
    tabPanel(id = "synchtab",title = strong("SynchTime"),
      sidebarPanel( 
        p("choose parameter"),
        selectInput("Param", NULL,
                    choices = GenerateSelectChoices(default = "No Comment", text = "", fieldName = "Comment" , tablename = "synch")),
        checkboxInput("showled", "Show LED Data", value = TRUE),
        checkboxInput("showerm", "Show ERM Data", value = TRUE),
        checkboxInput("synchAlldatacheck", "Compare To Everyone's Data", value = FALSE),
      ),
    
      mainPanel(
        #Output: Histogram ----
        plotlyOutput("plot2"),
        plotlyOutput("plot3"),
      )
    ),
    tabPanel(strong("Physiological Data"),
      tableOutput("table")
    )
  ),
  tags$footer()
)