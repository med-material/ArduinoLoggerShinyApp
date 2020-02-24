library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)

server = function(input, output, session) {
  
  # VARIABLES
  
  # define colors to use in plots.
  colorPalette <- c("#c94232","#239a37")
  
  # a variable we use, if we filter based on pid.
  pid_index <- NULL
  pid_name <- NULL
  pid_email <- NULL
  pid_query <- NULL
  subject <- 'reactiontime'
  
  # a variable we use to keep track of the currently available participants
  participants <- NULL
  choices <- NULL
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[['subject']])) {
      subject <<- query[['subject']]
      updateTabsetPanel(session, "subjectChooser", selected = subject)
    }
    if (!is.null(query[['pid']])) {
      pid = query[['pid']]
      pid_query <<- pid
      pid_name <<- pid
    }
    if (!is.null(query[['email']])) {
      sel = query[['email']]
      pid_email = query[['email']]
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session , "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"))
    }
  })

  observeEvent({input$subjectChooser}, {
    if (input$subjectChooser != subject) {
      subject <<- input$subjectChooser
      UpdatePIDSelection()      
      UpdateVisualizations()      
    }
  })
  
  observeEvent(ignoreNULL=FALSE, {input$pidChooser}, {
    print(paste("email: ", input$emailSelect))
    # prevent infinite loop - only update pid_name to null, if the value is not already null.
    if (is.null(pid_name) & is.null(input$pidChooser)) {
      print(paste("pidChooser: pid_index ", pid_index))
      print(paste("pidChooser: pid_name ", pid_name))
      print("ignored..")
      return()
    }
    # CheckboxInputGroup sends an initial NULL value which overrides any query values.
    # Make sure we check whether a specific PID was specified as URL param before.
    if (!is.null(pid_query)) {
      print("pid_query exists, ignoring pidChooser")
    } else if (!is.null(input$pidChooser)) {
      pid_index <<- input$pidChooser
      pid_name <<- unlist(participants[input$pidChooser,"PID"])
      pid_email <<- unlist(participants[input$pidChooser,"Email"])
    } else {
      pid_index <<- NULL
      pid_name <<- NULL
      pid_email <<- NULL
    }
    print(paste("pidChooser: pid_index ", pid_index))
    print(paste("pidChooser: pid_name ", pid_name))
    UpdateVisualizations()  
  })
  observeEvent({input$emailSelect},{
    if (input$emailSelect == "-1") {
      return()
    }
    RefreshDataSets(input$emailSelect)

    UpdatePIDSelection()
    
    UpdateVisualizations()
  })
  observeEvent(input$Param, {  
    UpdateVisualizations()
  })
  
  UpdatePIDSelection <- function() {
    # Update PID Choosers to show PID numbers based on the data
    if (subject == "synch") {
      participants <<- unique(dfsynch %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    } else if (subject == "reactiontime") {
      participants <<- unique(dfrt %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    }
    if (!is.null(pid_query)) {
      pid_name <<- pid_query
      pid_query <<- NULL
      pid_index <<- unname(choices[names(choices) == pid_name])
      print(paste("PIDQuery: e-mail", input$emailSelect))
      print(paste("PIDQuery: pid_name", pid_name))
      print(paste("PIDQuery: pid_index", pid_index))
      #pid_name <<- unlist(participants[pid_index,"PID"])
      #pid_query <<- NULL
    }
    print(choices)
    print(nrow(participants))
    if (is.null(choices)) {
      updateCheckboxGroupInput(session, label = "No Participant Data", "pidChooser", choices = NULL, selected = NULL, inline = TRUE)
    }
    else if (is.null(pid_index)) {
      print("UpdateCheckbox: pid is null")
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = NULL, inline = TRUE)
    } else {
      print(paste("UpdateCheckbox: ", pid_index))
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = pid_index, inline = TRUE)
    }
  }

  UpdateVisualizations <- function() {
    if (input$emailSelect == "-1") {
      return()
    }
    print(paste("UpdateVis pid: ", pid_name))
    print(paste("dfrt nrow:",nrow(dfrt)))
    print(paste("dfsynch nrow:",nrow(dfsynch)))
    # Filter visualization data based on pid_name
    if (!is.null(pid_name)) {
      dfrt <- dfrt %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
      dfsynch <- dfsynch %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
    }
    if (subject == "reactiontime") {
      print(paste("dfrt filtered nrow:",nrow(dfrt)))

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
    } else if (subject == "synch") {
      print(paste("dfsynch filtered nrow:",nrow(dfsynch)))

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
  
}
  
