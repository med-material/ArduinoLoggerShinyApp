library(shiny)
library(plotly)
library(shinyjs)

ui <- fluidPage(
  includeCSS("custom.css"),
  useShinyjs(),
  fluidRow(
    column(8, titlePanel("Arduino Logger Visualizer (Cohort 2021)")),
    column(
      4,
      column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib = "font-awesome")),
      column(11,
        style = "margin-top : 20px; text-align: center;",
        selectInput("emailSelect", NULL, choices = c("Loading.." = -1))
      )
    )
  ),
  fluidRow(
    column(12, checkboxGroupInput("pidChooser", label = "Loading...", choices = NULL, inline = TRUE))
  ),
  # RT part------
  tabsetPanel(
    id = "subjectChooser", type = "tabs",
    tabPanel(
      value = "reactiontime", id = "reactPan", strong("ReactionTime"),
      navlistPanel(
        widths = c(4, 8),
        "Choose Visualization:",
        tabPanel(
          "Reaction Time per Trial",
          plotlyOutput("rtTrialPlot"),
          tags$div(class = "vizcontrols-explainer")
        ),
        tabPanel(
          "Reaction Time based on Intensity",
          plotlyOutput("rtIntensityPlot"),
          tags$div(class = "vizcontrols-explainer")
        ),
        tabPanel(
          "Reaction Time density plots",
          plotlyOutput("rtDensityPlot"),
          tags$div(class = "vizcontrols-explainer")
        )
      )
    ),
    # synch panel -----------
    tabPanel(
      value = "synch", id = "synchPan", strong("SynchTime"),
      navlistPanel(
        widths = c(4, 8),
        "Choose Visualization:",
        tabPanel(
          "Synchronization based on Intensity",
          plotlyOutput("synchViolinPlot"),
          tags$div(class = "vizcontrols-explainer"),
          p("Filter data:"),
          selectInput("Param", NULL, choices = c(levels(dfsynch$Comment), "NA"))
        ),
        tabPanel(
          "Synchronization based on MusicalAbility",
          plotlyOutput("synchAbilityByMusicalityPlot"),
          tags$div(class = "vizcontrols-explainer")
          # selectInput("Param", NULL, choices = c(levels(dfsynch$Comment),"NA")),
        ),
        tabPanel(
          "Getting into synch on MusicalAbility",
          # plotlyOutput("GettingIntoSynchByMusicalityPlot"),
          plotlyOutput("GettingIntoSynchByMusicalityPlotPower"),
          tags$div(class = "vizcontrols-explainer")
          # textOutput("The grey band is a moving aver")
          # plot("The grey band is a moving aver")
        )
      )
    ),
    # EDAIBIserial panel ----------
    tabPanel(
      value = "EDAIBISerial", strong("Physiological Data"),
      plotlyOutput("physioIBIplot"),

      tableOutput("HRVtable"),
      plotOutput("powerBandPlot"),
      plotOutput("poincarePlot"),
      plotlyOutput("EDAplot"),
      plotlyOutput("EDAplotBW"),

      # p()
      tags$div(class = "vizcontrols-explainer")
    ),
    tags$footer()
  )
)
