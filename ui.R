library(shiny)
library(plotly)

ui <- fluidPage(
    fluidRow(
        column(8, titlePanel("Arduino Logger Visualizer (Cohort 2020)")),
        column(4, column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib="font-awesome")),
                  column(11,style = "margin-top : 20px; text-align: center;",selectInput("test", NULL,
                             choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Email")),
              ))
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
                plotlyOutput("plot1"),
                
              )),
            
  tabPanel(strong("SynchTime"), verbatimTextOutput("summary")),
  tabPanel(strong("Physiological Data"), tableOutput("table"))),

)