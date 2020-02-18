library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)

server = function(input, output, session) {
  
  # VARIABLES
  
  # define colors to use in plots.
  colorPalette <- c("#c94232","#239a37")
  
  # a variable we use, if we filter based on pid.
  pid_selection <- NULL
  pid_query <- NULL
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    if (!is.null(query[['email']])) {
      sel = query[['email']]
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"))
    }
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[['subject']])) {
      subject = query[['subject']]
      updateTabsetPanel(session, "subjectChooser", selected = subject)
    }
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[['pid']])) {
      pid = query[['pid']]
      print(pid)
      pid_query <<- pid
      pid_selection <<- pid
    }
  })
  observeEvent(ignoreNULL=FALSE, {input$pidChooser}, {
    # prevent infinite loop - only update pid_selection to null, if the value is not already null.
    if (is.null(pid_selection) & is.null(input$pidChooser)) {
      return()
    }
    # CheckboxInputGroup sends an initial NULL value which overrides any query values.
    # Make sure we check whether a specific PID was specified as URL param before.
    if (!is.null(pid_query)) {
      pid_selection <<- pid_query
      pid_query <<- NULL
    } else {
      pid_selection <<- input$pidChooser
    }
    print(pid_selection)
    UpdateVisualizations()
  })
  observeEvent({input$emailSelect},{
    if (input$emailSelect == "-1") {
      return()
    }
    RefreshDataSets(input$emailSelect)
    
    # Update PID Choosers to show PID numbers based on the data
    participants = unique(c(dfrt$PID,dfsynch$PID))
    if (is.null(pid_selection)) {
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = participants, selected = NULL, inline = TRUE)
    } else {
      print(pid_selection)
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = participants, selected = pid_selection, inline = TRUE)
    }
    
    UpdateVisualizations()
  })
  observeEvent(input$Param, {  
    UpdateVisualizations()
  })
  
  

  UpdateVisualizations <- function() {
    
    # Filter visualization data based on pid_selection
    if (!is.null(pid_selection)) {
      dfrt <- dfrt %>% filter(PID %in% pid_selection)
      dfsynch <- dfsynch %>% filter(PID %in% pid_selection)
    }
    
    output$rtTrialPlot <- renderPlotly(plot_ly(dfrt, x = ~dfrt$TrialNo, y = ~dfrt$ReactionTime) %>% 
                                         add_trace(type = 'scatter', mode='markers', name = ~Modal , color = ~Modal , colors = colorPalette) %>%
                                         layout(showlegend = TRUE, xaxis = list(dtick = 1, title = "Trial Number"), yaxis = list(range = c(0,500), title = "Reaction Time (ms)")) %>%
                                         config(scrollZoom = TRUE)
    )
    
    # IMPROVED INTENSITY PLOT.
    #get medians of each participant per group (Intens x Modal)
    dfmed<-dfrt%>%group_by(Email, Intens, Modal)%>%summarise(median=median(ReactionTime))
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
      geom_point(data=dfrt,aes(x=Intens,y=ReactionTime,group=Modal, color=Modal),alpha=.15,position= position_jitterdodge()) +
      geom_point(aes(group=Modal),position=dodge) +
      geom_errorbar(aes(ymin = median-ci, ymax = median+ci),width=0.2 ,position = dodge) +
      geom_line(position = dodge) +
      theme_bw() +
      theme(legend.title=element_blank()) +
      ylab("Reaction Time (ms)") + 
      xlab("Intensity") +
      ylim(0,500)
    output$rtIntensityPlot <- renderPlotly(ggplotly(p = ggintensityplot) %>%
                                             config(scrollZoom = TRUE)
    )
    

      
      # SYNCH ABILITY VS INTENSITY PLOT
      dfsynch_filtered = dfsynch[!is.na(dfsynch$ReactionTime),]
      ggsynchViolinPlot = ggplot(dfsynch_filtered,
                                 aes(Intens,ReactionTime,fill=Modal)) +
        geom_violin() +
        facet_wrap(vars(MusicalAbility)) +
        geom_point(data=dfsynch_filtered,aes(Intens,ReactionTime,fill=Modal),alpha=0.2,position= position_jitterdodge()) +
        xlab("Intensity") +
        ylab("Synch Offset (ms)") +
        theme_minimal() +
        theme(legend.title=element_blank(), plot.title=element_blank())
      output$synchViolinPlot <- renderPlotly(ggplotly(p = ggsynchViolinPlot) %>%
                                               config(scrollZoom = TRUE)
      )

    # SYNCH ABILITY VS MUSICAL ABILITY PLOT
    #dfsynch_music_filtered = dfsynch_filtered[!is.na(dfsynch$MusicalAbility),]
    #dfsynch_music_filtered$ReactionTimeRounded <<- round(dfsynch_music_filtered$ReactionTime, digits=-1)
    #dfsynch_music_filtered$freq = count(dfsynch_music_filtered, vars =c("dfsynch_music_filtered$ReactionTimeRounded"))
    #HighMA<-dfsynch_music_filtered %>% filter(MusicalAbility == "High")
    #LowMA<-dfsynch_music_filtered %>% filter(MusicalAbility == "Low")
    #print(LowMA)
    #if ((dim(HighMA)[1] != 0)) {
    #output$synchHighMAPlot <- renderPlotly(plot_ly(HighMA,x = ~HighMA$ReactionTimeRounded, y = ~HighMA$freq) %>%
    #                               add_trace(type = 'bar',name = ~HighMA$Modal,  color = ~HighMA$Modal , colors = colorPalette) %>% 
    #                               layout(title = "High MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Amount")))
    #}
    #if ((dim(LowMA)[1] != 0)) {
    #output$synchLowMAPlot <- renderPlotly(plot_ly(LowMA, x = ~LowMA$ReactionTimeRounded, y = ~LowMA$freq)%>%
    #                               add_trace(type = 'bar',name = ~LowMA$Modal,color = ~LowMA$Modal , colors = colorPalette) %>% 
    #                               layout(title = "Low MusicalAbility", xaxis = list(title = "Reaction Time (ms)"), yaxis = list(title = "Amount")))
    #}
  }
  
}
  
