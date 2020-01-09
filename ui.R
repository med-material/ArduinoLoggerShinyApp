library(shiny)
library(plotly)
library(shinyjs)

ui <- fluidPage(
  
  includeCSS("style.css"),

  
  useShinyjs(),
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
                radioButtons("visual", NULL,
                             c("Reaction Time per Trial" = "trialbutton",
                               "Reaction Time based on Itensity" = "intensbutton")),
                disabled(checkboxInput("Alldatacheck", "Compare To Everyone's Data", value = FALSE)),
              ), 
              
              mainPanel(
                
                # Output: Histogram ----
                plotlyOutput("plot1"),
                
              )),
            
              tabPanel(strong("SynchTime"), sidebarPanel( 
                #p("choose parameter"),
                #selectInput("test", NULL,
                #choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Email"),
               # disabled(checkboxInput("Alldatacheck", "Compare To Everyone's Data", value = FALSE)),
              #), 
              
              #mainPanel(
                
                # Output: Histogram ----
               # plotlyOutput("plot1"),
                
              )),
              tabPanel(strong("Physiological Data"), tableOutput("table"))),

)
