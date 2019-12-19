library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel(
    fluidRow(
        column(9,"Arduino Logger Visualizer (Cohort 2020)"),
        column(3,selectInput("variable", NULL,
                           c("Everyone's Data" = "alldata",
                             "variousemail" = "email")), 
              )
            )
  ),
  strong("CHOOSE EXERCISE"),
  tabsetPanel(type = "tabs",
              tabPanel(strong("ReactionTime"), sidebarPanel(
                p("CHOOSE VISUALIZATION"),
                div(style="inline-block",actionButton("trialbutton","Reaction Time per Trial")),
                div(style="inline-block",actionButton("intensbutton", "Reaction Time based on Intensity")),
                checkboxInput("Alldatacheck", "Compare To Everyone's Data", FALSE),
              ), 
              
              mainPanel(
                
                # Output: Histogram ----
                plotOutput(outputId = "distPlot")
                
              )),
            
  tabPanel(strong("SynchTime"), verbatimTextOutput("summary")),
  tabPanel(strong("Physiological Data"), tableOutput("table"))),

)