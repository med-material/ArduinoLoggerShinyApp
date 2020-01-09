server = function(input, output, session) {
  
  observeEvent({input$test},{
    
  observeEvent(input$visual,{
    
    if(input$visual == "trialbutton"){
    
    show(id = "Alldatacheck", anim = TRUE, animType = "slide", time = 0.5,
         selector = NULL)
      observeEvent({input$Alldatacheck}, {
      var<-list()
      varbound<-list()
      varmodal<-list()
      if (input$test != -1){
        var = list(list(paste("Email = '", input$test,"'", sep = "")))
        #varbound = list(list(paste("Email = '", input$test,"'", sep = "")))
        varmodal = list(list(paste("Email = '", input$test,"'"," AND Modal = 'ERM'", sep = "")))
        enable(id = "Alldatacheck")
        
      }
      else{
        disable(id = "Alldatacheck")
        updateCheckboxInput(session, "Alldatacheck", value = FALSE)
        }
      
      pal <- c("red","green")

                                   
                                   if(input$Alldatacheck){
                                     output$plot1 <- renderPlotly(plot_ly(type = 'scatter', mode='lines+markers', name = ~Modal , color = ~Modal , colors = pal, split = ~TimeStamp, data = FetchDatas(conditionLists = var,                                                                                                                                                             option = "TimeStamp, Email, TrialNo, ReactionTime, Modal"), 
                                     x = ~TrialNo, y = ~ReactionTime, tablename = "reactiontime")%>% 
                                     add_trace(data = FetchDatas(conditionLists = varbound, 
                                     option = "TimeStamp, Email, TrialNo, ReactionTime, Modal", tablename = "reactiontime"),type = 'scatter', mode='lines',fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'), color = "grey") %>%
                                    layout(xaxis = list(title = "Trial Number"), yaxis = list(title = "Reaction Time (ms)")))
                                   }
      
      
      else {
        output$plot1 <- renderPlotly(plot_ly(type = 'scatter', mode='lines+markers', name = ~Modal, color = ~Modal , colors = pal, split = ~TimeStamp, data = FetchDatas(conditionLists = var,
        option = "TimeStamp, Email, TrialNo, ReactionTime, Modal", tablename = "reactiontime"), 
        x = ~TrialNo, y = ~ReactionTime)%>% 
        layout(xaxis = list(title = "Trial Number"), yaxis = list(title = "Reaction Time (ms)")))
      }
      
    })
    
    }
    
    if(input$visual == "intensbutton"){
      hide(id = "Alldatacheck", anim = TRUE, animType = "slide", time = 0.5,
           selector = NULL)
      observeEvent({input$test},{
        var<-list()
        varmodal<-list()
        if (input$test != -1){
          var = list(list(paste("Email = '", input$test, "'", sep = "")))
          varmodal = list(list(paste("Email = '", input$test,"'"," AND Modal = 'ERM'", sep = "")))
        }
        
        pal <- c("red","green")
        
        output$plot1 <- renderPlotly(plot_ly(type = 'scatter', mode='markers', name = ~Modal,  color = ~Modal , colors = pal,split = ~TimeStamp, data = FetchDatas(conditionLists = var,
                                      option = "TimeStamp, Email, Intens, ReactionTime, Modal", tablename = "reactiontime"), 
                                      x = ~Intens, y = ~ReactionTime)%>% 
                                      layout(xaxis = list(title = "Intensity"), yaxis = list(title = "Reaction Time (ms)")))          
        
      })
    }
  })
  })
}
  
