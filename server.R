library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
# library(RHRV) #doesn't work on ShineyServer

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
  
  print_nodata_msg <- function() {
    msg <- paste("No ", subject, " data to show for ", sep='')
    
    mail <- all_accounts[[1]]
    if (!is.null(pid_email)) {
      mail <- pid_email
    }
    msg <- paste(msg, mail, sep='')
    
    if (!is.null(pid_name)) {
      msg <- paste(msg, ", Participant ", pid_name, sep='')
    }
    msg <- paste(msg, ".", sep='')
    
  }
  
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
    # for synch -------
    if (subject == "synch") {
      participants <<- unique(dfsynch %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    }
    # for reaction time  -------
    else if (subject == "reactiontime") {
      participants <<- unique(dfrt %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)),participants$PID)
      } else {
        choices <<- NULL
      }
    }
    # for physio -------
     else if (subject == "EDAIBISerial") {
       participants <<- unique(dfphysio %>% group_by(Email) %>% distinct(PID))
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
    print(paste("dfphysio nrow:",nrow(dfphysio)))
    
    # Filter visualization data based on pid_name
    if (!is.null(pid_name)) {
      dfrt <- dfrt %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
      dfsynch <- dfsynch %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
      dfphysio<- dfphysio %>% filter(Email %in% pid_email) %>% filter(PID %in% pid_name)
    }
    if (subject == "reactiontime") {
      # RT ABILITY PLOT -------
      print(paste("dfrt filtered nrow:",nrow(dfrt)))
      
      output$rtTrialPlot <- renderPlotly({
         validate( need(nrow(dfrt) > 0, print_nodata_msg()))
        
         plot_ly(dfrt, x = ~dfrt$TrialNo, y = ~dfrt$ReactionTime) %>%
         add_trace(type = 'scatter', mode='markers', name = ~Modal , color = ~Modal , colors = colorPalette) %>%
         layout(showlegend = TRUE, xaxis = list(dtick = 1, title = "Trial Number"), yaxis = list(range = c(0,500), title = "Reaction Time (ms)")) %>%
         config(scrollZoom = TRUE)
      })

      output$rtIntensityPlot <- renderPlotly({
        validate( need(nrow(dfrt) > 0, print_nodata_msg()))
        
        # IMPROVED INTENSITY PLOT.
        #get medians of each participant/PID combination per group (Intens x Modal)
        dfmed<-dfrt%>% filter(ReactionTime < 421) %>% group_by(Email, PID, Intens, Modal)%>%summarise(median=median(ReactionTime))
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
          #geom_point(data=dfrt,aes(x=Intens,y=ReactionTime,group=Modal, color=Modal),alpha=.15,position= position_jitterdodge()) +
          geom_point(data=dfmed,aes(x=Intens,y=median,group=Modal, color=Modal),alpha=.15,position= position_jitterdodge()) +
          geom_point(aes(group=Modal),position=dodge) +
          geom_errorbar(aes(ymin = median-ci, ymax = median+ci),width=0.2 ,position = dodge) +
          geom_line(position = dodge) +
          theme_bw() +
          theme(legend.title=element_blank()) +
          ylab("Reaction Time (ms)") +
          xlab("Intensity \n .95 confidence error bars are based on the \n median reaction time values of the participants (included as dots) ") +
          ylim(0,500)
        
        ggplotly(p = ggintensityplot) %>% config(scrollZoom = TRUE)
      })
      
      
      # density plot
      output$rtDensityPlot <- renderPlotly({
        validate( need(nrow(dfrt) > 0, print_nodata_msg()))
        
        ggdensityPlot<-ggplot(dfrt, aes(ReactionTime,color=Intens)) +geom_density()+scale_x_continuous(limits = c(-50, 800),breaks = seq(0, 800, by = 100))+xlab("reaction time in ms")+theme_bw()+facet_grid(cols = vars(Modal))        
        ggplotly(p = ggdensityPlot) %>% config(scrollZoom = TRUE)
        })
    } else if (subject == "synch") {
      print(paste("dfsynch filtered nrow:",nrow(dfsynch)))
      
      # SYNCH ABILITY VS INTENSITY PLOT -------
      output$synchViolinPlot <- renderPlotly({
        validate( need(nrow(dfsynch) > 0, print_nodata_msg()))
        
        dfsynch = dfsynch[!is.na(dfsynch$ReactionTime),]
        ggsynchViolinPlot = ggplot(dfsynch,
          aes(Intens,ReactionTime,fill=Modal)) +
          geom_violin() +
          facet_wrap(vars(MusicalAbility)) +
          geom_point(data=dfsynch,aes(Intens,ReactionTime,fill=Modal),alpha=0.2,position= position_jitterdodge()) +
          xlab("Intensity") +
          ylab("Synch Offset (ms)") +
          theme_minimal() +
          theme(legend.title=element_blank(), plot.title=element_blank())        
        
        ggplotly(p = ggsynchViolinPlot) %>% config(scrollZoom = TRUE)
      })
      
      # Synch Performance based on Musical Ability Plot
      output$synchAbilityByMusicalityPlot <- renderPlotly({
        validate( need(nrow(dfsynch) > 0, print_nodata_msg()))
        ggsynchMusicalAbilityPlot =  ggplot(dfsynch, aes(ReactionTime,color=MusicalAbility))+geom_vline(xintercept=0) +geom_density()+scale_x_continuous(limits = c(-500, 500),breaks = seq(-500, 500, by = 100))+xlab("synch offset in ms")+theme_bw()+facet_grid(cols = vars(Modal))
        ggplotly(p = ggsynchMusicalAbilityPlot) %>% config(scrollZoom = TRUE)
      })

      output$GettingIntoSynchByMusicalityPlotPower <- renderPlotly({
        validate( need(nrow(dfsynch) > 0, print_nodata_msg()))
        GettingIntoSynchByMusicalityPlotPowerX = ggplot(dfsynch,aes(x=runTrialNo,y=absSynchOffset))+geom_point()+ geom_smooth(size=0)+ stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+ylab("absolute offset from beat in ms")+xlab("attempt number #")+theme_bw()+facet_grid(~MusicalAbility)
        ggplotly(p = GettingIntoSynchByMusicalityPlotPowerX) %>% config(scrollZoom = TRUE)
        })
      
    }
    
    else if (subject == "EDAIBISerial") {
      # physio  PLOT -------
      dfIBI <- dfphysio[dfphysio$IBI!=0,]
      output$physioIBIplot <- renderPlotly({
        validate( need(nrow(dfIBI) > 0, print_nodata_msg()))
        IBIplot = ggplot(dfIBI,aes(x=TimeLine,y=IBI))+geom_point()+ylab("inter-beat interval in ms")+xlab("time line in seconds")+geom_line()+theme_bw() + scale_y_continuous(breaks=seq(0,max(dfIBI$IBI),200))+scale_x_continuous(breaks=seq(0,max(dfIBI$TimeLine),1))+ expand_limits(x = 0, y = 0)+geom_hline(yintercept=300,color='red')+geom_hline(yintercept=2000, color='green')
        ggplotly(p = IBIplot) %>% config(scrollZoom = TRUE)
      })
      
      # output$HRVtable <- renderDataTable(dfHRV)
      
      output$EDAplot <- renderPlotly({
        validate( need(nrow(dfphysio) > 0, print_nodata_msg()))
        EDAplotX = ggplot(dfphysio,aes(x=TimeLine,y=EDAsmoothed))+ylab("conductivity in...?")+xlab("time line in seconds")+geom_line()+theme_bw() + scale_y_continuous(breaks=seq(0,max(dfIBI$IBI),200))+scale_x_continuous(breaks=seq(0,max(dfIBI$TimeLine),1))+ expand_limits(x = 0, y = 0)+facet_grid(rows=vars(TimeStamp))        
        ggplotly(p = EDAplotX) %>% config(scrollZoom = TRUE)
        })
      
      output$EDAplotBW <- renderPlotly({
        validate( need(nrow(dfphysio) > 0, print_nodata_msg()))
        EDAplotbwX = ggplot(dfphysio,aes(x=TimeLine,y=EDAsmoothedbw))+ylab("conductivity in...?")+xlab("time line in seconds")+geom_line()+theme_bw() + scale_y_continuous(breaks=seq(0,max(dfIBI$IBI),200))+scale_x_continuous(breaks=seq(0,max(dfIBI$TimeLine),1))+ expand_limits(x = 0, y = 0)+facet_grid(rows=vars(TimeStamp))
        ggplotly(p = EDAplotbwX) %>% config(scrollZoom = TRUE)
        })
      
    }
  }
  
}

