library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
library(gsheet)
# library(stats)
# library(RHRV) #doesn't work on ShinyServer


server <- function(input, output, session) {

  # VARIABLES
  dfrt_g<<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1jZgv08ud058pIUk-MPrBAH9_2hXKokg-VA6ejNn06rk/edit?usp=sharing')
  dfrt_g<<-melt(dfrt_g, id.vars=c(1:5))
  
  # define colors to use in plots.
  colorPalette <- c("#c94232", "#239a37")

  # a variable we use, if we filter based on pid.
  pid_index <- NULL
  pid_name <- NULL
  pid_email <- NULL
  pid_query <- NULL
  subject <- "reactiontime"

  print_nodata_msg <- function() {
    msg <- paste("No ", subject, " data to show for ", sep = "")

    mail <- all_accounts[[1]]
    if (is.null(input$pidChooser)) {
      mail <- input$emailSelect
    }
    msg <- paste(msg, mail, sep = "")

    if (!is.null(pid_name)) {
      msg <- paste(msg, ", Participant ", pid_name, sep = "")
    }
    msg <- paste(msg, ".", sep = "")
  }

  # a variable we use to keep track of the currently available participants
  participants <- NULL
  choices <- NULL

  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[["subject"]])) {
      subject <<- query[["subject"]]
      updateTabsetPanel(session, "subjectChooser", selected = subject)
    }
    if (!is.null(query[["pid"]])) {
      pid <- query[["pid"]]
      # in case no PID was specified, the URL uses "NULL" but the data uses "NA".
      if (pid == "NULL") {
        pid = NULL
      }
      pid_query <<- pid
      pid_name <<- pid
    }
    gsheet_accounts = unique(dfrt_g$`Your student email`)
    if (!is.null(query[["email"]])) {
      sel <- query[["email"]]
      pid_email <- query[["email"]]
      updateSelectInput(session, "emailSelect", choices = c(all_accounts,gsheet_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session, "emailSelect", choices = c(all_accounts, gsheet_accounts, "Everyone\'s Data" = "NA"))
    }
  })

  observeEvent(
    {
      input$subjectChooser
    },
    {
      if (input$subjectChooser != subject) {
        subject <<- input$subjectChooser
        UpdatePIDSelection()
        UpdateVisualizations()
      }
    }
  )

  observeEvent(
    ignoreNULL = FALSE,
    {
      input$pidChooser
    },
    {
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
        pid_name <<- unlist(participants[input$pidChooser, "PID"])
        pid_email <<- unlist(participants[input$pidChooser, "Email"])
      } else {
        pid_index <<- NULL
        pid_name <<- NULL
        pid_email <<- NULL
      }
      print(paste("pidChooser: pid_index ", pid_index))
      print(paste("pidChooser: pid_name ", pid_name))
      UpdateVisualizations()
    }
  )
  observeEvent(
    {
      input$emailSelect
    },
    {
      if (input$emailSelect == "-1") {
        return()
      }
      RefreshDataSets(input$emailSelect)

      UpdatePIDSelection()
      print(input$emailSelect)
      if (input$emailSelect == "NA") {
        dfrt_gf <<- dfrt_g
      } else {
        dfrt_gf <<- dfrt_g %>% filter(`Your student email` == input$emailSelect)
      }
      UpdateVisualizations()
    }
  )
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
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
      } else {
        choices <<- NULL
      }
    }
    # for reaction time  -------
    else if (subject == "reactiontime") {
      participants <<- unique(dfrt %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
      } else {
        choices <<- NULL
      }
    }
    # for physio -------
    else if (subject == "EDAIBISerial") {
      participants <<- unique(dfphysio %>% group_by(Email) %>% distinct(PID))
      participants$PID[is.na(participants$PID)] <<- "NA"
      if (nrow(participants) > 0) {
        choices <<- setNames(c(1:nrow(participants)), participants$PID)
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
      # pid_name <<- unlist(participants[pid_index,"PID"])
      # pid_query <<- NULL
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
    print(paste("dfrt nrow:", nrow(dfrt)))
    print(paste("dfsynch nrow:", nrow(dfsynch)))
    print(paste("dfphysio nrow:", nrow(dfphysio)))
    print(paste("dfrt_gf nrow:", nrow(dfrt_gf)))
    print(paste("dfIBI nrow:", nrow(dfIBI)))

    # Filter visualization data based on pid_name
    if (!is.null(pid_name)) {
      dfrt <- dfrt %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name)
      dfsynch <- dfsynch %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name)
      dfphysio <- dfphysio %>%
        filter(Email %in% pid_email) %>%
        filter(PID %in% pid_name)
    }
    if (subject == "reactiontime") {
      # RT ABILITY PLOT -------
      print(paste("dfrt filtered nrow:", nrow(dfrt)))

      output$rtTrialPlot <- renderPlotly({
        validate(need(nrow(dfrt) > 0, print_nodata_msg()))

        plot_ly(dfrt, x = ~ dfrt$TrialNo, y = ~ dfrt$ReactionTime) %>%
          add_trace(type = "scatter", mode = "markers", name = ~Modal, color = ~Modal, colors = colorPalette) %>%
          layout(showlegend = TRUE, xaxis = list(dtick = 1, title = "Trial Number"), yaxis = list(range = c(0, 500), title = "Reaction Time (ms)")) %>%
          config(scrollZoom = TRUE)
      })

      output$rtIntensityPlot <- renderPlotly({
        validate(need(nrow(dfrt) > 0, print_nodata_msg()))

        # IMPROVED INTENSITY PLOT.
        # get medians of each participant/PID combination per group (Intens x Modal)
        dfmed <- dfrt %>%
          filter(ReactionTime < 421) %>%
          group_by(Email, PID, Intens, Modal) %>%
          summarise(median = median(ReactionTime))
        # create means of medians by group (Intens x Modal)
        dfm <- dfmed %>%
          group_by(Intens, Modal) %>%
          summarise(mean = mean(median))
        # dfmc<-dfmed%>%group_by(Intens, Modal)%>%count
        # create confidence intervals for each condition ((Intens x Modal))
        dfmci <- summarySE(data = dfmed, measurevar = "median", groupvars = c("Intens", "Modal"), na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
        # pair up the confidence intervals and medians with the means
        dfrt_intensity <- merge(dfm, dfmci)
        dodge <- position_dodge(width = 0.9)
        ggintensityplot <- ggplot(
          dfrt_intensity,
          aes(Intens, mean, group = Modal, color = Modal)
        ) +
          # geom_point(data=dfrt,aes(x=Intens,y=ReactionTime,group=Modal, color=Modal),alpha=.15,position= position_jitterdodge()) +
          geom_point(data = dfmed, aes(x = Intens, y = median, group = Modal, color = Modal), alpha = .15, position = position_jitterdodge()) +
          geom_point(aes(group = Modal), position = dodge) +
          geom_errorbar(aes(ymin = median - ci, ymax = median + ci), width = 0.2, position = dodge) +
          geom_line(position = dodge) +
          theme_bw() +
          theme(legend.title = element_blank()) +
          ylab("Reaction Time (ms)") +
          xlab("Intensity \n .95 confidence error bars are based on the \n median reaction time values of the participants (included as dots) ") +
          ylim(0, 500)

        ggplotly(p = ggintensityplot) %>% config(scrollZoom = TRUE)
      })


      # density plot
      output$rtDensityPlot <- renderPlotly({
        validate(need(nrow(dfrt) > 0, print_nodata_msg()))

        ggdensityPlot <- ggplot(dfrt, aes(ReactionTime, color = Intens)) +
          geom_density() +
          scale_x_continuous(limits = c(-50, 800), breaks = seq(0, 800, by = 100)) +
          xlab("reaction time in ms") +
          theme_bw() +
          facet_grid(cols = vars(Modal))
        ggplotly(p = ggdensityPlot) %>% config(scrollZoom = TRUE)
      })
    } else if (subject == "synch") {
      print(paste("dfsynch filtered nrow:", nrow(dfsynch)))

      # SYNCH ABILITY VS INTENSITY PLOT -------
      output$synchViolinPlot <- renderPlotly({
        validate(need(nrow(dfsynch) > 0, print_nodata_msg()))

        dfsynch <- dfsynch[!is.na(dfsynch$ReactionTime), ]
        ggsynchViolinPlot <- ggplot(
          dfsynch,
          aes(Intens, ReactionTime, fill = Modal)
        ) +
          geom_violin() +
          facet_wrap(vars(MusicalAbility)) +
          geom_point(data = dfsynch, aes(Intens, ReactionTime, fill = Modal), alpha = 0.2, position = position_jitterdodge()) +
          xlab("Intensity") +
          ylab("Synch Offset (ms)") +
          theme_minimal() +
          theme(legend.title = element_blank(), plot.title = element_blank())

        ggplotly(p = ggsynchViolinPlot) %>% config(scrollZoom = TRUE)
      })

      # Synch Performance based on Musical Ability Plot
      output$synchAbilityByMusicalityPlot <- renderPlotly({
        validate(need(nrow(dfsynch) > 0, print_nodata_msg()))
        ggsynchMusicalAbilityPlot <- ggplot(dfsynch, aes(ReactionTime, color = MusicalAbility)) +
          geom_vline(xintercept = 0) +
          geom_density() +
          scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 100)) +
          xlab("synch offset in ms") +
          theme_bw() +
          facet_grid(cols = vars(Modal))
        ggplotly(p = ggsynchMusicalAbilityPlot) %>% config(scrollZoom = TRUE)
      })

      output$GettingIntoSynchByMusicalityPlotPower <- renderPlotly({
        validate(need(nrow(dfsynch) > 0, print_nodata_msg()))
        GettingIntoSynchByMusicalityPlotPowerX <- ggplot(dfsynch, aes(x = runTrialNo, y = absSynchOffset)) +
          geom_point() +
          geom_smooth(size = 0) +
          stat_smooth(aes(color = "red"), method = "nls", formula = "y~a*x^b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
          ylab("absolute offset from beat in ms") +
          xlab("attempt number #") +
          theme_bw() +
          facet_grid(~MusicalAbility)
        ggplotly(p = GettingIntoSynchByMusicalityPlotPowerX) %>% config(scrollZoom = TRUE)
      })
    }

    else if (subject == "EDAIBISerial") {
      # physio  PLOT -------
      # dfIBI <<- dfphysio[dfphysio$IBI!=0,]
      # IBI<<-dfIBI[,c("IBI")]
      # source("HRCalculations.R")
      output$physioIBIplot <- renderPlotly({
        validate(need(nrow(dfphysio) > 0, print_nodata_msg()))
        IBIplot <- ggplot(dfIBI, aes(x = TimeLine, y = IBI)) +
          geom_point() +
          ylab("inter-beat interval in ms") +
          xlab("time line in seconds") +
          geom_line() +
          theme_bw() +
          scale_y_continuous(breaks = seq(0, max(dfIBI$IBI), 200)) +
          scale_x_continuous(breaks = seq(0, max(dfIBI$TimeLine), 1)) +
          expand_limits(x = 0, y = 0) +
          geom_hline(yintercept = 300, color = "red") +
          geom_hline(yintercept = 2000, color = "green")
        ggplotly(p = IBIplot) %>% config(scrollZoom = TRUE)
      })

      # ##### HRV stuff -------
      if (nrow(dfIBI) < 1000) {
        return()
      }
      tsIBI <<- as.data.frame(cumsum(c(0, dfIBI[2:nrow(dfIBI), ]$IBI / 1000)))
      names(tsIBI) <<- c("beats")
      #
      # #need to write data back to file as I couldn't figure out how to simply inject it into the data structure, the file should be safe to delete after this
      # beatAscii <- write.table(tsIBI$beats, file = "", sep = ",", qmethod = "double", row.names = FALSE, col.names = FALSE)
      #
      # #create data structure

      hrv.data <<- CreateHRVData()
      #
      # #load the beat data
      hrv.data <<- LoadBeatString(hrv.data, tsIBI$beats)
      #
      # #make a non-interpolated plot of the heart rate
      hrv.data <<- BuildNIHR(hrv.data)
      hrv.data <<- FilterNIHR(hrv.data)
      hrv.data <<- InterpolateNIHR(hrv.data, freqhr = 4)
      #
      # PlotNIHR(hrv.data, main = "niHR",Tags = "all")
      #
      # #Create a time analysis, the values here are the same as the default
      hrv.data <<- CreateTimeAnalysis(hrv.data, size = floor(max(dfphysio$TimeLine)) / 2, interval = 7.8125)
      #
      # #Do the frequency analysis
      hrv.data <<- CreateFreqAnalysis(hrv.data)
      #
      #
      # #Creates a power bands plot to see the values of LF/HF etc. over time
      hrv.data <<-
        CalculatePowerBand(hrv.data,
          indexFreqAnalysis = 1,
          size = 100, shift = 2, type = "fourier",
          ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
          LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4
        )

      # #create nonlinear analysis
      hrv.data <<- CreateNonLinearAnalysis(hrv.data)
      hrv.data <<- NonlinearityTests(hrv.data)
      hrv.data <<- PoincarePlot(hrv.data, indexNonLinearAnalysis = 1, timeLag = 1, confidenceEstimation = TRUE, confidence = 0.9, doPlot = TRUE)
      poincareRecordplot <<- recordPlot()
      dev.off()
      #
      #
      # #Put all the values into a single variable each for easier display  in a table
      SDNN <<- round(hrv.data$TimeAnalysis[[1]]$SDNN, 1)
      pNN50 <<- round(hrv.data$TimeAnalysis[[1]]$pNN50, 1)
      rMSSD <<- round(hrv.data$TimeAnalysis[[1]]$rMSSD, 1)
      avgLF <<- round(mean(hrv.data$FreqAnalysis[[1]]$LF), 1)
      avgHF <<- round(mean(hrv.data$FreqAnalysis[[1]]$HF), 1)
      avgLFHF <<- round(avgLF / avgHF, 1)
      SD1 <<- round(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1, 1)
      SD2 <<- round(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD2, 1)

      types <- c("Time domain", "", "", "Frequency domain", "", "", "Non-linear", "")
      measures <- c("SDNN", "pNN50", "rMSSD", "avg LF", "avg HF", "avg LF/HF", "SD1", "SD2")
      mvalues <- c(SDNN, pNN50, rMSSD, avgLF, avgHF, avgLFHF, SD1, SD2)
      dfHRV <<- data.frame(cbind(types, measures, mvalues))
      dfHRV$types <- as.character(dfHRV$types)
      dfHRV$measures <- as.character(dfHRV$measures)
      output$HRVtable <- renderTable({
        validate(need(nrow(dfphysio) > 0, print_nodata_msg()))
        dfHRV
      })


      # ###################
      # #Plots the powerband calculations from above, ymax can be changed to change the y-max value on ULF VLF LF and HF graphs while ymaxratio changes the max y value on the LF/HF graph.
      powerBandPlotX <<- PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 1200, ymaxratio = 16)
      powerBandPlotXRec <<- recordPlot()
      dev.off()
      output$powerBandPlot <<- renderPlot({
        validate(need(nrow(dfphysio) > 0, print_nodata_msg()))
        print(powerBandPlotXRec)
      })




      output$poincarePlot <- renderPlot({
        validate(need(nrow(dfphysio) > 0, print_nodata_msg()))
        print(poincareRecordplot)
      })

      output$EDAplot <- renderPlotly({
        validate(need(nrow(dfphysio) > 0, print_nodata_msg()))
        EDAplotX <- ggplot(dfphysio, aes(x = TimeLine, y = EDAsmoothed)) +
          ylab("conductivity in...?") +
          xlab("time line in seconds") +
          geom_line() +
          theme_bw() +
          scale_y_continuous(breaks = seq(0, max(dfIBI$IBI), 200)) +
          scale_x_continuous(breaks = seq(0, max(dfIBI$TimeLine), 1)) +
          expand_limits(x = 0, y = 0) +
          facet_grid(rows = vars(TimeStamp))
        ggplotly(p = EDAplotX) %>% config(scrollZoom = TRUE)
      })

      output$EDAplotBW <- renderPlotly({
        validate(need(nrow(dfphysio) > 0, print_nodata_msg()))
        EDAplotbwX <- ggplot(dfphysio, aes(x = TimeLine, y = EDAsmoothedbw)) +
          ylab("conductivity in...?") +
          xlab("time line in seconds") +
          geom_line() +
          theme_bw() +
          scale_y_continuous(breaks = seq(0, max(dfIBI$IBI), 200)) +
          scale_x_continuous(breaks = seq(0, max(dfIBI$TimeLine), 1)) +
          expand_limits(x = 0, y = 0) +
          facet_grid(rows = vars(TimeStamp))
        ggplotly(p = EDAplotbwX) %>% config(scrollZoom = TRUE)
      })
    } else if (subject == "GSheetReactionTime") {
      output$gsheetsreactplot <- renderPlotly({
        validate(need(nrow(dfrt_gf %>% filter(task=="reaction time only")) > 0, print_nodata_msg()))
        dfrt_gf %>% filter(task=="reaction time only") %>%
          ggplot(aes(value,color=factor(intensity))) +geom_density()+scale_x_continuous(limits = c(-50, 800),breaks = seq(0, 800, by = 50))+xlab("reaction time in ms")+theme_bw()+facet_grid(rows=vars(modality))
      })
      
      output$gsheetsreactplot2 <- renderPlotly({
        validate(need(nrow(dfrt_gf %>% filter(task!="reaction time only")) > 0, "No data available here."))
        dfrt_gf %>% filter(task!="reaction time only") %>%
          ggplot(aes(value,color=factor(intensity))) +geom_density()+scale_x_continuous(limits = c(-50, 800),breaks = seq(0, 800, by = 50))+xlab("reaction time in ms")+theme_bw()+facet_grid(rows=vars(modality))
      })
    }
  }
}
