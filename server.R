library(plyr)
library(Rmisc)
library(reshape2)

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
      updateSelectInput(session , "Param", choices = GenerateSelectChoices(default = "No Filter", text = "", fieldName = "Comment", tablename = "synch", conditions = varparam))
      }
      else if (input$emailSelect == -1) {
        updateSelectInput(session , "Param", choices = GenerateSelectChoices(default = "No Filter", text = "", fieldName = "Comment", tablename = "synch"))
      }
    })
  })
  
  
  observeEvent({input$emailSelect},{
  print(input$emailSelect)
  if (input$emailSelect != -1){
    current_dfrt = dfrt %>% filter(Email == input$emailSelect)
    current_dfsynch = dfsynch %>% filter(Email == input$emailSelect)
  } else {
    current_dfrt = dfrt
    current_dfsynch = dfsynch
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
  dfrt_intensity<-merge(dfm,dfmci)
  dodge<-position_dodge(width=0.9)
  ggintensityplot <- ggplot(dfrt_intensity,
                  aes(Intens,mean,group=Modal, color=Modal)) +
                  geom_point(data=current_dfrt,aes(x=Intens,y=ReactionTime,group=Modal, color=Modal),alpha=.15,position= position_jitterdodge()) +
                  geom_point(aes(group=Modal),position=dodge) +
                  geom_errorbar(aes(ymin = median-ci, ymax = median+ci),width=0.2 ,position = dodge) +
                  geom_line(position = dodge) +
                  theme_bw() +
                  theme(legend.title=element_blank()) +
                  ylab("Movement Time (ms)") + 
                  xlab("Intensity")
  output$rtIntensityPlot <- renderPlotly(ggplotly(p = ggintensityplot))
    
  observeEvent(input$Param, {
  
  # SYNCH ABILITY VS INTENSITY PLOT
  dfsynch_filtered = current_dfsynch[!is.na(dfsynch$ReactionTime),]
  ggsynchViolinPlot = ggplot(dfsynch_filtered,
                             aes(Intens,ReactionTime,fill=Modal)) +
                             geom_violin() +
                             facet_wrap(vars(MusicalAbility)) +
                             geom_point(data=dfsynch_filtered,aes(Intens,ReactionTime,fill=Modal),alpha=0.2,position= position_jitterdodge()) +
                             xlab("Intensity") +
                             ylab("Synch Offset (ms)") +
                             theme_minimal() +
                             theme(legend.title=element_blank(), plot.title=element_blank())
  output$synchViolinPlot <- renderPlotly(ggplotly(p = ggsynchViolinPlot))
  })
    
  })
}
  
