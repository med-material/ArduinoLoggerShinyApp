library(plyr)

server = function(input, output, session) {
  
  observeEvent({input$tabs},{
    print(input$tabs)
    if(input$tabs == "<strong>ReactionTime</strong>"){
      updateSelectInput(session , "test", choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Email" , tablename = "v_reactiontime"))
     
    }
    else if(input$tabs == "<strong>SynchTime</strong>"){
      updateSelectInput(session , "test", choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Email", tablename = "synch"))
    }
    observeEvent({input$test} ,{
      if (input$test != -1){
      varparam = list(list(paste("Email = '", input$test,"'", sep = "")))
      updateSelectInput(session , "Param", choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Comment", tablename = "synch", conditions = varparam))
      show(id = "synchAlldatacheck", anim = TRUE, animType = "slide", time = 0.5,
           selector = NULL)
      updateCheckboxInput(session, "synchAlldatacheck", value = FALSE)
      }
      else if (input$test == -1) {
        updateSelectInput(session , "Param", choices = GenerateSelectChoices(default = "All test", text = "", fieldName = "Comment", tablename = "synch"))
        hide(id = "synchAlldatacheck", anim = TRUE, animType = "slide", time = 0.5,
             selector = NULL)
        updateCheckboxInput(session, "synchAlldatacheck", value = FALSE)
      }
    })
    })
  
  
  observeEvent({input$test},{
    
  observeEvent(input$visual,{
    
    if(input$visual == "radio1"){
    
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
      reactiondata = FetchDatas(conditionLists= var , option = "TimeStamp, Email, TrialNo, ReactionTime, Modal", tablename = "v_reactiontime")
      reactiondata$ReactionTimeRounded = round(reactiondata$ReactionTime, digits=-1)

                                   
                                   if(input$Alldatacheck){
                                     output$plot1 <- renderPlotly(plot_ly(reactiondata, x = ~reactiondata$TrialNo, y = ~reactiondata$ReactionTimeRounded)%>% 
                                                                  add_trace(type = 'scatter', mode='lines+markers', name = ~Modal , color = ~Modal , colors = pal, split = ~TimeStamp)%>%
                                                                  add_trace(data = varbound, type = 'scatter', mode='lines',fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'), color = "grey")%>%
                                                                  layout(xaxis = list(title = "Trial Number"), yaxis = list(title = "Reaction Time (ms)")))
                                      
                                   }
      
      
      else {
        output$plot1 <- renderPlotly(plot_ly(reactiondata, x = ~reactiondata$TrialNo, y = ~reactiondata$ReactionTimeRounded)%>% 
                                       add_trace(type = 'scatter', mode='lines+markers', name = ~Modal , color = ~Modal , colors = pal, split = ~TimeStamp)%>%
                                      layout(xaxis = list(title = "Trial Number"), yaxis = list(title = "Reaction Time (ms)")))
      }
      
    })
    
    }
    
    if(input$visual == "radio2"){
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
        reactiondataintens = FetchDatas(conditionLists = var,option = "TimeStamp, Email, Intens, ReactionTime, Modal", tablename = "v_reactiontime")
        reactiondataintens$ReactionTimeRounded = round(reactiondataintens$ReactionTime, digits=-1)
        
        output$plot1 <- renderPlotly(plot_ly(reactiondataintens ,x = ~reactiondataintens$Intens, y = ~ reactiondataintens$ReactionTimeRounded)%>% 
                                    add_trace(type = 'scatter', mode='markers', name = ~Modal,  color = ~Modal , colors = pal,split = ~TimeStamp)%>%
                                    layout(xaxis = list(title = "Intensity"), yaxis = list(title = "Reaction Time (ms)")))          
        
      })
    }
  })
    
observeEvent(input$Param, {    
  observeEvent(input$showled,{
    observeEvent(input$showerm, {
    #plot if led is checked
    if (input$test != -1){
      var1<-contage[contage$synchdataLED.Email == input$test & contage$synchdataLED.MusicalAbility == 'High',]
      var2<-contage[contage$synchdataLED.Email == input$test & contage$synchdataLED.MusicalAbility == 'Low',]

      pal <- c("red","green")

      output$plot2 <- renderPlotly(plot_ly(contage,x = ~var1$synchdataLED.ReactionTimeRounded, y = ~var1$freq)%>%
                                     add_trace(type = 'bar',name = ~var1$synchdataLED.Modal,  color = ~var1$synchdataLED.Modal , colors = pal)%>% 
                                    layout(title = "Has MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
      output$plot3 <- renderPlotly(plot_ly(contage,x = ~var2$synchdataLED.ReactionTimeRounded, y = ~var2$freq)%>%
                                     add_trace(type = 'bar',name = ~var2$synchdataLED.Modal,color = ~var2$synchdataLED.Modal , colors = pal)%>% 
                                     layout(title = "Has No MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
    }
    
    else{
      var1<-contage[contage$synchdataLED.MusicalAbility == 'High',]
      var2<-contage[contage$synchdataLED.MusicalAbility == 'Low',]
  
      pal <- c("red","green")

      output$plot2 <- renderPlotly(plot_ly(contage,x = ~var1$synchdataLED.ReactionTimeRounded, y = ~var1$freq)%>%
                                     add_trace(type = 'bar',name = ~var1$synchdataLED.Modal,color = ~var1$synchdataLED.Modal , colors = pal)%>% 
                                  layout(title = "Has MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
      output$plot3 <- renderPlotly(plot_ly(contage,x = ~var2$synchdataLED.ReactionTimeRounded, y = ~var2$freq)%>%
                                     add_trace(type = 'bar',name = ~var2$synchdataLED.Modal,color = ~var2$synchdataLED.Modal , colors = pal)%>% 
                                     layout(title = "Has No MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
    }
    
    if (input$Param !=-1){
      var1<-contage[contage$synchdataLED.MusicalAbility == 'High' & contage$synchdataLED.Comment == input$Param,]
      var2<-contage[contage$synchdataLED.MusicalAbility == 'Low' & contage$synchdataLED.Comment == input$Param,]

      pal <- c("red","green")
      output$plot2 <- renderPlotly(plot_ly(contage,x = ~var1$synchdataLED.ReactionTimeRounded, y = ~var1$freq)%>%
                                     add_trace(type = 'bar',name = ~var1$synchdataLED.Modal,color = ~var1$synchdataLED.Modal , colors = pal)%>% 
                                     layout(title = "Has MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
      output$plot3 <- renderPlotly(plot_ly(contage,x = ~var2$synchdataLED.ReactionTimeRounded, y = ~var2$freq)%>%
                                     add_trace(type = 'bar',name = ~var2$synchdataLED.Modal,color = ~var2$synchdataLED.Modal , colors = pal)%>% 
                                     layout(title = "Has No MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
    } 
})
})
})
    
  })
}
  
