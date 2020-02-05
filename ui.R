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
            selectInput("emailSelect", NULL, choices = GenerateSelectChoices(default = "Everyone\'s Data", text = "", fieldName = "Email", tablename = "synch")),
           )
    )
  ),
  tabsetPanel(id = "tabs", type = "tabs",
    tabPanel(id = "reactpan", strong("ReactionTime"),
        navlistPanel(
          widths = c(4, 8),
          "Choose Visualization:",
          tabPanel("Reaction Time per Trial",
              plotlyOutput("rtTrialPlot"),
          ),
          tabPanel("Reaction Time based on Intensity",
              plotlyOutput("rtIntensityPlot"),
          )
        )
    ),
    tabPanel(id = "synchpan", strong("SynchTime"),
             navlistPanel(
               widths = c(4, 8),
               "Choose Visualization:",
               tabPanel("Synchronization based on Intensity",
                        plotlyOutput("synchViolinPlot"),
                        p("Filter data:"),
                        selectInput("Param", NULL, choices = GenerateSelectChoices(default = "No Filter", text = "", fieldName = "Comment" , tablename = "synch")),
               ),
               tabPanel("Synchronization based on MusicalAbility",
                        plotlyOutput("synchMAPlot"),
                        p("Filter data:"),
                        selectInput("Param", NULL, choices = GenerateSelectChoices(default = "No Filter", text = "", fieldName = "Comment" , tablename = "synch")),
               )
             ),

    ),
    tabPanel(strong("Physiological Data"),
      tableOutput("table")
    )
  ),
  tags$footer()
)