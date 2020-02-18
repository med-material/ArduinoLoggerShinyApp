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
            selectInput("emailSelect", NULL, choices=c("Loading.." = -1)),
           )
    )
  ),
  fluidRow(
    column(12, checkboxGroupInput("pidChooser", label = "Loading...", choices = NULL, inline = TRUE))
  ),
  tabsetPanel(id = "subjectChooser", type = "tabs",
    tabPanel(value  = "reactiontime", id = "reactPan", strong("ReactionTime"),
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
    tabPanel(value  = "synch", id = "synchPan", strong("SynchTime"),
             navlistPanel(
               widths = c(4, 8),
               "Choose Visualization:",
               tabPanel("Synchronization based on Intensity",
                        plotlyOutput("synchViolinPlot"),
                        p("Filter data:"),
                        selectInput("Param", NULL, choices = c(levels(dfsynch$Comment),"NA")),
               ),
               tabPanel("Synchronization based on MusicalAbility",
                        #plotlyOutput("synchHighMAPlot"),
                        #plotlyOutput("synchLowMAPlot"),
                        p("Coming soon..."),
                        #selectInput("Param", NULL, choices = c(levels(dfsynch$Comment),"NA")),
               )
             ),

    ),
    tabPanel(value = "EDAIBISerial", strong("Physiological Data"),
      p("Coming soon..")
    )
  ),
  tags$footer()
)