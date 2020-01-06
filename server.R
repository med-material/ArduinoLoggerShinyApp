server = function(input, output, session) {
  
  observeEvent(input$trialbutton,{
    observeEvent({input$test},{
      var<-list()
      if (input$test != -1){
        var = list(list(paste("Email = '", input$test, "'", sep = "")))
      }
      
      pal <- c("red", "blue", "green")
      
      output$plot1 <- renderPlotly(plot_ly(type = 'scatter', mode='lines+markers',color = var, colors = pal, data = FetchDatas(conditionLists = var,
                                     option = "TrialNo, ReactionTime"), 
                                     x = ~TrialNo, y = ~ReactionTime)%>% 
                                     layout(xaxis = list(title = "Trial Number"), yaxis = list(title = "Reaction Time")))          
      
    })
  })
  
  observeEvent(input$intensbutton,{
    observeEvent({input$test},{
      var<-list()
      if (input$test != -1){
        var = list(list(paste("Email = '", input$test, "'", sep = "")))
      }
      
      pal <- c("red", "blue", "green")
      
      output$plot1 <- renderPlotly(plot_ly(type = 'scatter', mode='lines+markers',color = var, colors = pal, data = FetchDatas(conditionLists = var,
                                    option = "Intens, ReactionTime"), 
                                     x = ~Intens, y = ~ReactionTime)%>% 
                                     layout(xaxis = list(title = "Intensity"), yaxis = list(title = "Reaction Time")))          
      
    })
  })
  
}
