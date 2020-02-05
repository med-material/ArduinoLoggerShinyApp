library(plyr)
library(Rmisc)

server = function(input, output, session) {
  
  # define colors to use in plots.
  colorPalette <- c("#c94232","#239a37")
  
  observeEvent({input$tabs},{
    print(input$tabs)
    if(input$tabs == "<strong>ReactionTime</strong>"){
      updateSelectInput(session , "emailSelect", choices = GenerateSelectChoices(default = "Everyone\'s Data", text = "", fieldName = "Email" , tablename = "v_reactiontime"))
     
    }
    else if(input$tabs == "<strong>SynchTime</strong>"){
      updateSelectInput(session , "emailSelect", choices = GenerateSelectChoices(default = "Everyone\'s Data", text = "", fieldName = "Email", tablename = "synch"))
    }
    observeEvent({input$emailSelect} ,{
      if (input$emailSelect != -1){
      varparam = list(list(paste("Email = '", input$emailSelect,"'", sep = "")))
      updateSelectInput(session , "Param", choices = GenerateSelectChoices(default = "Everyone\'s Data", text = "", fieldName = "Comment", tablename = "synch", conditions = varparam))
      show(id = "synchAlldatacheck", anim = TRUE, animType = "slide", time = 0.5,
           selector = NULL)
      updateCheckboxInput(session, "synchAlldatacheck", value = FALSE)
      }
      else if (input$emailSelect == -1) {
        updateSelectInput(session , "Param", choices = GenerateSelectChoices(default = "Everyone\'s Data", text = "", fieldName = "Comment", tablename = "synch"))
        hide(id = "synchAlldatacheck", anim = TRUE, animType = "slide", time = 0.5,
             selector = NULL)
        updateCheckboxInput(session, "synchAlldatacheck", value = FALSE)
      }
    })
  })
  
  
  observeEvent({input$emailSelect},{
  print(input$emailSelect)
  if (input$emailSelect != -1){
    current_dfrt = dfrt %>% filter(Email == input$emailSelect)
    summary(current_dfrt)
  } else {
    current_dfrt = dfrt
  }
  output$rtTrialPlot <- renderPlotly(plot_ly(current_dfrt, x = ~current_dfrt$TrialNo, y = ~current_dfrt$ReactionTimeRounded) %>% 
                                     add_trace(type = 'scatter', mode='lines+markers', line = list(width = 1.5), name = ~Modal , color = ~Modal , colors = colorPalette, split = ~TimeStamp)%>%
                                     layout(showlegend = FALSE, xaxis = list(title = "Trial Number"), yaxis = list(title = "Reaction Time (ms)")))
  # IMPROVED INTENSITY PLOT.
  #get medians of each participant per group (Intens x Modal)
  dfmed<-current_dfrt%>%group_by(Email, Intens, Modal)%>%summarise(median=median(ReactionTime))
  #create means of medians by group (Intens x Modal)
  dfm<-dfmed%>%group_by(Intens, Modal)%>%summarise(mean=mean(median))
  #dfmc<-dfmed%>%group_by(Intens, Modal)%>%count
  #create confidence intervals for each condition ((Intens x Modal))
  dfmci<-summarySE(data = dfmed, measurevar = "median", groupvars = c("Intens","Modal"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
  #pair up the confidence intervals and medians with the means
  dfrt_violin<-merge(dfm,dfmci)
  dodge<-position_dodge(width=0.9)
  ggviolinplot <- ggplot(dfrt_violin,
                  aes(Intens,mean,group=Modal, color=Modal)) +
                  geom_point(data=current_dfrt,aes(x=Intens,y=ReactionTime,group=Modal, color=Modal),alpha=.15,position= position_jitterdodge()) +
                  geom_point(aes(group=Modal),position=dodge) +
                  geom_errorbar(aes(ymin = median-ci, ymax = median+ci),width=0.2 ,position = dodge) +
                  geom_line(position = dodge) +
                  theme_bw() +
                  theme(legend.title=element_blank()) +
                  ylab("Movement Time (ms)") + 
                  xlab("Intensity")
  output$rtIntensityPlot <- renderPlotly(ggplotly(p = ggviolinplot))

    
observeEvent(input$Param, {    
  observeEvent(input$showled,{
    observeEvent(input$showerm, {
    #plot if led is checked
    if (input$emailSelect != -1){
      var1<-contage[contage$synchdataLED.Email == input$emailSelect & contage$synchdataLED.MusicalAbility == 'High',]
      var2<-contage[contage$synchdataLED.Email == input$emailSelect & contage$synchdataLED.MusicalAbility == 'Low',]


      output$plot2 <- renderPlotly(plot_ly(contage,x = ~var1$synchdataLED.ReactionTimeRounded, y = ~var1$freq)%>%
                                     add_trace(type = 'bar',name = ~var1$synchdataLED.Modal,  color = ~var1$synchdataLED.Modal , colors = colorPalette)%>% 
                                    layout(title = "Has MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
      output$plot3 <- renderPlotly(plot_ly(contage,x = ~var2$synchdataLED.ReactionTimeRounded, y = ~var2$freq)%>%
                                     add_trace(type = 'bar',name = ~var2$synchdataLED.Modal,color = ~var2$synchdataLED.Modal , colors = colorPalette) %>% 
                                     layout(title = "Has No MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
    }
    
    else{
      var1<-contage[contage$synchdataLED.MusicalAbility == 'High',]
      var2<-contage[contage$synchdataLED.MusicalAbility == 'Low',]

      output$plot2 <- renderPlotly(plot_ly(contage,x = ~var1$synchdataLED.ReactionTimeRounded, y = ~var1$freq)%>%
                                     add_trace(type = 'bar',name = ~var1$synchdataLED.Modal,color = ~var1$synchdataLED.Modal , colors = colorPalette)%>% 
                                  layout(title = "Has MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
      output$plot3 <- renderPlotly(plot_ly(contage,x = ~var2$synchdataLED.ReactionTimeRounded, y = ~var2$freq)%>%
                                     add_trace(type = 'bar',name = ~var2$synchdataLED.Modal,color = ~var2$synchdataLED.Modal , colors = colorPalette)%>% 
                                     layout(title = "Has No MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
    }
    
    if (input$Param !=-1){
      var1<-contage[contage$synchdataLED.MusicalAbility == 'High' & contage$synchdataLED.Comment == input$Param,]
      var2<-contage[contage$synchdataLED.MusicalAbility == 'Low' & contage$synchdataLED.Comment == input$Param,]

      output$plot2 <- renderPlotly(plot_ly(contage,x = ~var1$synchdataLED.ReactionTimeRounded, y = ~var1$freq)%>%
                                     add_trace(type = 'bar',name = ~var1$synchdataLED.Modal,color = ~var1$synchdataLED.Modal , colors = colorPalette)%>% 
                                     layout(title = "Has MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
      output$plot3 <- renderPlotly(plot_ly(contage,x = ~var2$synchdataLED.ReactionTimeRounded, y = ~var2$freq)%>%
                                     add_trace(type = 'bar',name = ~var2$synchdataLED.Modal,color = ~var2$synchdataLED.Modal , colors = colorPalette)%>% 
                                     layout(title = "Has No MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Diversity")))
    } 
})
})
})
    
  })
}
  
