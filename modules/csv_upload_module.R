#csv_upload_df = NULL
csv_upload_UI <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>CSV File Upload</h3>",
         "<p>Upload files in here, which represent the
         <strong>Reaction Time</strong>, <strong>Synch</strong> or <strong>Physiological</strong>
         data.</p>"),
    
    fileInput(
      ns("filereactiontime"),
      "Choose Reaction Time CSV Files", placeholder = "Select CSV file", multiple=T, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    fileInput(
      ns("filesynch"),
      "Choose SynchTime CSV Files", placeholder = "Select CSV file", multiple=T, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    fileInput(
      ns("fileEDAIBISerial"),
      "Choose Physiological Data Files", placeholder = "Select CSV file", multiple=T, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    actionButton(ns("actionSubmit"), "Submit"),
    textOutput(ns("statusText"))
  )
}

csv_upload <- function(input, output, session) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    df = NULL,
    dfsynch = NULL,
    dfreactiontime = NULL,
    dfEDAIBISerial = NULL,
    trigger = 0
  )
  
  observeEvent(input$actionSubmit, {
    if (!is.null(input$filereactiontime)) {
      toReturn$dfreactiontime = csv_upload_combine_data(input$filereactiontime$datapath)
      toReturn$trigger <- toReturn$trigger + 1
      toReturn$df = toReturn$dfreactiontime
    }
    if (!is.null(input$filesynch)) {
      toReturn$dfsynch <- csv_upload_combine_data(input$filesynch$datapath)
      toReturn$trigger <- toReturn$trigger + 1
      toReturn$df = toReturn$dfsynch
    }
    if (!is.null(input$fileEDAIBISerial)) {
      toReturn$dfEDAIBISerial <- csv_upload_combine_data(input$fileEDAIBISerial$datapath)
      toReturn$trigger <- toReturn$trigger + 1
      toReturn$df = toReturn$dfEDAIBISerial
    }
  })
  
  observeEvent(toReturn$df, {
    req(!is.null(toReturn$df))
    output$statusText <- renderText({ " Data Received Successfully!" })
    insertUI(selector = paste0("#", ns("statusText")), where="afterBegin",
            ui = icon("check", class = "fa-1x", lib="font-awesome"))
  })
  
  return(toReturn)
}

csv_upload_combine_data <- function(files) {
  data_list = lapply(files, function(file) {
    read.csv(file, na.strings="NULL", sep=";")
  })
  
  if (length(data_list) > 1) {
    data = Reduce(function(x, y) bind_rows(x, y), data_list)
  } else {
    data <- data_list[[1]]
  }
  return(data)
}