library(plyr)
library(Rmisc)
library(reshape2)
library(dplyr)
library(tidyr)
# library(stats)
# library(RHRV) #doesn't work on ShinyServer

server <- function(input, output, session) {

  # VARIABLES

  # define colors to use in plots.
  colorPalette <- c("#c94232", "#239a37")
  
  csv_data <- callModule(csv_upload, "uploadData")
  
  r <- reactiveValues(dfsynch = data.frame(),
                      dfphysio = data.frame(),
                      dfrt = data.frame(),
                      dfIBI = data.frame(),
                      emails = vector(),
                      tsIBI = data.frame(),
                      hrv.data = data.frame(),
                      dfHRV = data.frame(),
                      pid_index = NULL,
                      pid_name = NULL,
                      pid_email = NULL,
                      pid_query = NULL,
                      subject = "reactiontime",
                      participants = NULL,
                      choices = NULL
  )

  # a variable we use, if we filter based on pid.



  print_nodata_msg <- function() {
    msg <- paste("No ", r$subject, " data to show for ", sep = "")

    mail <- all_accounts[[1]]
    if (is.null(input$pidChooser)) {
      mail <- input$emailSelect
    }
    msg <- paste(msg, mail, sep = "")

    if (!is.null(r$pid_name)) {
      msg <- paste(msg, ", Participant ", r$pid_name, sep = "")
    }
    msg <- paste(msg, ".", sep = "")
  }

  # a variable we use to keep track of the currently available participants
  r$participants <- NULL
  r$choices <- NULL

  observe({
    query <- parseQueryString(session$clientData$url_search)
    # Change E-mail dropdown based on the ?email=XXX URL parameter
    # Filter visualizations based on the ?pid=XXX URL parameter (based on the tab's value attribute)
    # Change Tab based on the ?subject=XXX URL parameter (based on the tab's value attribute)
    if (!is.null(query[["subject"]])) {
      r$subject <- query[["subject"]]
      updateTabsetPanel(session, "subjectChooser", selected = r$subject)
    }
    if (!is.null(query[["pid"]])) {
      pid <- query[["pid"]]
      # in case no PID was specified, the URL uses "NULL" but the data uses "NA".
      if (pid == "NULL") {
        pid = NULL
      }
      r$pid_query <- pid
      r$pid_name <- pid
    }
    if (!is.null(query[["email"]])) {
      sel <- query[["email"]]
      r$pid_email <- query[["email"]]
      updateSelectInput(session, "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"), selected = sel)
    } else {
      updateSelectInput(session, "emailSelect", choices = c(all_accounts, "Everyone\'s Data" = "NA"))
    }
  })

  observeEvent(input$CsvButton, {
    insertUI(selector = "#CsvButton", where = "afterEnd",
             ui = showModal(modalDialog(csv_upload_UI("uploadData"), easyClose = TRUE)))
  })
  
  observeEvent(csv_data$trigger, {
    req(csv_data$trigger > 0)
    if (!is.null(csv_data$dfreactiontime)) {
    r$dfrt <- csv_data$dfreactiontime
    r$emails = r$dfrt$Email
    }
    if (!is.null(csv_data$dfsynch)) {
    r$dfsynch <- csv_data$dfsynch
    r$emails = c(r$emails, r$dfsynch$Email) 
    }
    if (!is.null(csv_data$dfEDAIBISerial)) {
    r$dfphysio <- csv_data$dfEDAIBISerial
    r$emails = c(r$emails, r$dfphysio$Email)
    }
    new = RefreshDataLocal(r$dfrt, r$dfsynch, r$dfphysio, r$dfIBI)
    r$dfrt = new$dfrt
    r$dfsynch = new$dfsynch
    r$dfphysio = new$dfphysio
    r$dfIBI = new$dfIBI
    
  })
  
  observeEvent(
    {
      input$subjectChooser
    },
    {
      if (input$emailSelect %in% c("-1","NA") || length(r$emails) == 0) {
        return()
      } else {
        if (input$subjectChooser != r$subject) {
          r$subject <- input$subjectChooser
          UpdatePIDSelection()
          UpdateVisualizations()
        }
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
      if (is.null(r$pid_name) & is.null(input$pidChooser)) {
        print(paste("pidChooser: pid_index ", r$pid_index))
        print(paste("pidChooser: pid_name ", r$pid_name))
        print("ignored..")
        return()
      }
      # CheckboxInputGroup sends an initial NULL value which overrides any query values.
      # Make sure we check whether a specific PID was specified as URL param before.
      if (!is.null(r$pid_query)) {
        print("pid_query exists, ignoring pidChooser")
      } else if (!is.null(input$pidChooser)) {
        r$pid_index <- input$pidChooser
        r$pid_name <- unlist(r$participants[input$pidChooser, "PID"])
        r$pid_email <- unlist(r$participants[input$pidChooser, "Email"])
      } else {
        r$pid_index <- NULL
        r$pid_name <- NULL
        r$pid_email <- NULL
      }
      print(paste("pidChooser: pid_index ", r$pid_index))
      print(paste("pidChooser: pid_name ", r$pid_name))
      UpdateVisualizations()
    }
  )
  observeEvent(
    {
      input$emailSelect
    },
    {
      
      if (input$emailSelect %in% c("-1","NA") || length(r$emails) == 0) {
        return()
      } else {
        print(paste("emailSelect is: ", input$emailSelect))
        RefreshDataSets(input$emailSelect)
    
        UpdatePIDSelection()
    
        UpdateVisualizations()
      }
    }
  )
  observeEvent(input$Param, {
    UpdateVisualizations()
  })

  UpdatePIDSelection <- function() {
    # Update PID Choosers to show PID numbers based on the data
    # for synch -------
    if (r$subject == "synch") {
      r$participants <- unique(r$dfsynch %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    }
    # for reaction time  -------
    else if (r$subject == "reactiontime") {
      r$participants <- unique(r$dfrt %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    }
    # for physio -------
    else if (r$subject == "EDAIBISerial") {
      r$participants <- unique(r$dfphysio %>% group_by(Email) %>% distinct(PID))
      r$participants$PID[is.na(r$participants$PID)] <- "NA"
      if (nrow(r$participants) > 0) {
        r$choices <- setNames(c(1:nrow(r$participants)), r$participants$PID)
      } else {
        r$choices <- NULL
      }
    }
    if (!is.null(r$pid_query)) {
      r$pid_name <- r$pid_query
      r$pid_query <- NULL
      r$pid_index <- unname(r$choices[names(r$choices) == r$pid_name])
      print(paste("PIDQuery: e-mail", input$emailSelect))
      print(paste("PIDQuery: pid_name", r$pid_name))
      print(paste("PIDQuery: pid_index", r$pid_index))
      # pid_name <- unlist(participants[pid_index,"PID"])
      # pid_query <- NULL
    }
    print(r$choices)
    print(nrow(r$participants))
    if (is.null(r$choices)) {
      updateCheckboxGroupInput(session, label = "No Participant Data", "pidChooser", choices = NULL, selected = NULL, inline = TRUE)
    }
    else if (is.null(r$pid_index)) {
      print("UpdateCheckbox: pid is null")
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = r$choices, selected = NULL, inline = TRUE)
    } else {
      print(paste("UpdateCheckbox: ", r$pid_index))
      updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = r$choices, selected = r$pid_index, inline = TRUE)
    }
  }

  UpdateVisualizations <- function() {
    if (input$emailSelect == "-1") {
      return()
    }
    print(paste("UpdateVis pid: ", r$pid_name))
    print(paste("dfrt nrow:", nrow(r$dfrt)))
    print(paste("dfsynch nrow:", nrow(r$dfsynch)))
    print(paste("dfphysio nrow:", nrow(r$dfphysio)))
    # print(paste("dfIBI nrow:", nrow(dfIBI)))


    # Filter visualization data based on pid_name
    if (!is.null(r$pid_name)) {
      r$dfrt <- r$dfrt %>%
        filter(Email %in% r$pid_email) %>%
        filter(PID %in% r$pid_name)
      r$dfsynch <- r$dfsynch %>%
        filter(Email %in% r$pid_email) %>%
        filter(PID %in% r$pid_name)
      r$dfphysio <- r$dfphysio %>%
        filter(Email %in% r$pid_email) %>%
        filter(PID %in% r$pid_name)
    }
    if (r$subject == "reactiontime") {
      # RT ABILITY PLOT -------
      print(paste("dfrt filtered nrow:", nrow(r$dfrt)))

      output$rtTrialPlot <- renderPlotly({
        validate(need(nrow(r$dfrt) > 0, print_nodata_msg()))
        plot_ly(r$dfrt %>% group_by(SessionID), x = ~ r$dfrt$TrialNo, y = ~ r$dfrt$ReactionTime) %>%
          add_trace(type = "scatter", mode = "markers+lines", name = ~paste(Modal,runningTrialNum), color = ~Modal, colors = colorPalette) %>%
          layout(showlegend = TRUE, xaxis = list(dtick = 1, title = "Trial Number"), yaxis = list(range = c(0, 500), title = "Reaction Time (ms)")) %>%
          config(scrollZoom = TRUE)
      })

      output$rtIntensityPlot <- renderPlotly({
        validate(need(nrow(r$dfrt) > 0, print_nodata_msg()))

        # IMPROVED INTENSITY PLOT.
        # get medians of each participant/PID combination per group (Intens x Modal)
        dfmed <- r$dfrt %>%
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
        validate(need(nrow(r$dfrt) > 0, print_nodata_msg()))

        ggdensityPlot <- ggplot(r$dfrt, aes(ReactionTime, color = Intens)) +
          geom_density() +
          scale_x_continuous(limits = c(-50, 800), breaks = seq(0, 800, by = 100)) +
          xlab("reaction time in ms") +
          theme_bw() +
          facet_grid(cols = vars(Modal))
        ggplotly(p = ggdensityPlot) %>% config(scrollZoom = TRUE)
      })
    } else if (r$subject == "synch") {
      print(paste("dfsynch filtered nrow:", nrow(r$dfsynch)))

      # SYNCH ABILITY VS INTENSITY PLOT -------
      output$synchViolinPlot <- renderPlotly({
        validate(need(nrow(r$dfsynch) > 0, print_nodata_msg()))

        r$dfsynch <- r$dfsynch[!is.na(r$dfsynch$ReactionTime), ]
        ggsynchViolinPlot <- ggplot(
          r$dfsynch,
          aes(Intens, ReactionTime, fill = Modal)
        ) +
          geom_violin() +
          facet_wrap(vars(MusicalAbility)) +
          geom_point(data = r$dfsynch, aes(Intens, ReactionTime, fill = Modal), alpha = 0.2, position = position_jitterdodge()) +
          xlab("Intensity") +
          ylab("Synch Offset (ms)") +
          theme_minimal() +
          theme(legend.title = element_blank(), plot.title = element_blank())

        ggplotly(p = ggsynchViolinPlot) %>% config(scrollZoom = TRUE)
      })

      # Synch Performance based on Musical Ability Plot
      output$synchAbilityByMusicalityPlot <- renderPlotly({
        validate(need(nrow(r$dfsynch) > 0, print_nodata_msg()))
        ggsynchMusicalAbilityPlot <- ggplot(r$dfsynch, aes(ReactionTime, color = MusicalAbility)) +
          geom_vline(xintercept = 0) +
          geom_density() +
          scale_x_continuous(limits = c(-500, 500), breaks = seq(-500, 500, by = 100)) +
          xlab("synch offset in ms") +
          theme_bw() +
          facet_grid(cols = vars(Modal))
        ggplotly(p = ggsynchMusicalAbilityPlot) %>% config(scrollZoom = TRUE)
      })

      output$GettingIntoSynchByMusicalityPlotPower <- renderPlotly({
        validate(need(nrow(r$dfsynch) > 0, print_nodata_msg()))
        GettingIntoSynchByMusicalityPlotPowerX <- ggplot(r$dfsynch, aes(x = runTrialNo, y = absSynchOffset)) +
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

    else if (r$subject == "EDAIBISerial") {
      # physio  PLOT -------
      # dfIBI <- dfphysio[dfphysio$IBI!=0,]
      # IBI<-dfIBI[,c("IBI")]
      # source("HRCalculations.R")
      output$physioIBIplot <- renderPlotly({
        validate(need(nrow(r$dfphysio) > 0, print_nodata_msg()))
        IBIplot <- ggplot(r$dfIBI, aes(x = TimeLine, y = IBI)) +
          geom_point() +
          ylab("inter-beat interval in ms") +
          xlab("time line in seconds") +
          geom_line() +
          theme_bw() +
          scale_y_continuous(breaks = seq(0, max(r$dfIBI$IBI), 200)) +
          scale_x_continuous(breaks = seq(0, max(r$dfIBI$TimeLine), 1)) +
          expand_limits(x = 0, y = 0) +
          geom_hline(yintercept = 300, color = "red") +
          geom_hline(yintercept = 2000, color = "green")
        ggplotly(p = IBIplot) %>% config(scrollZoom = TRUE)
      })

      # ##### HRV stuff -------
      if (nrow(r$dfIBI) < 1000) {
        return()
      }
      r$tsIBI <- as.data.frame(cumsum(c(0, r$dfIBI[2:nrow(r$dfIBI), ]$IBI / 1000)))
      names(r$tsIBI) <- c("beats")
      #
      # #need to write data back to file as I couldn't figure out how to simply inject it into the data structure, the file should be safe to delete after this
      # beatAscii <- write.table(r$tsIBI$beats, file = "", sep = ",", qmethod = "double", row.names = FALSE, col.names = FALSE)
      #
      # #create data structure

      r$hrv.data <- CreateHRVData()
      #
      # #load the beat data
      r$hrv.data <- LoadBeatString(r$hrv.data, r$tsIBI$beats)
      #
      # #make a non-interpolated plot of the heart rate
      r$hrv.data <- BuildNIHR(r$hrv.data)
      r$hrv.data <- FilterNIHR(r$hrv.data)
      r$hrv.data <- InterpolateNIHR(r$hrv.data, freqhr = 4)
      #
      # PlotNIHR(hrv.data, main = "niHR",Tags = "all")
      #
      # #Create a time analysis, the values here are the same as the default
      r$hrv.data <- CreateTimeAnalysis(r$hrv.data, size = floor(max(r$dfphysio$TimeLine)) / 2, interval = 7.8125)
      #
      # #Do the frequency analysis
      r$hrv.data <- CreateFreqAnalysis(r$hrv.data)
      #
      #
      # #Creates a power bands plot to see the values of LF/HF etc. over time
      r$hrv.data <-
        CalculatePowerBand(r$hrv.data,
          indexFreqAnalysis = 1,
          size = 100, shift = 2, type = "fourier",
          ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
          LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4
        )

      # #create nonlinear analysis
      r$hrv.data <- CreateNonLinearAnalysis(r$hrv.data)
      r$hrv.data <- NonlinearityTests(r$hrv.data)
      r$hrv.data <- PoincarePlot(r$hrv.data, indexNonLinearAnalysis = 1, timeLag = 1, confidenceEstimation = TRUE, confidence = 0.9, doPlot = TRUE)
      poincareRecordplot <- recordPlot()
      dev.off()
      #
      #
      # #Put all the values into a single variable each for easier display  in a table
      SDNN <- round(r$hrv.data$TimeAnalysis[[1]]$SDNN, 1)
      pNN50 <- round(r$hrv.data$TimeAnalysis[[1]]$pNN50, 1)
      rMSSD <- round(r$hrv.data$TimeAnalysis[[1]]$rMSSD, 1)
      avgLF <- round(mean(r$hrv.data$FreqAnalysis[[1]]$LF), 1)
      avgHF <- round(mean(r$hrv.data$FreqAnalysis[[1]]$HF), 1)
      avgLFHF <- round(avgLF / avgHF, 1)
      SD1 <- round(r$hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1, 1)
      SD2 <- round(r$hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD2, 1)

      types <- c("Time domain", "", "", "Frequency domain", "", "", "Non-linear", "")
      measures <- c("SDNN", "pNN50", "rMSSD", "avg LF", "avg HF", "avg LF/HF", "SD1", "SD2")
      mvalues <- c(SDNN, pNN50, rMSSD, avgLF, avgHF, avgLFHF, SD1, SD2)
      r$dfHRV <- data.frame(cbind(types, measures, mvalues))
      r$dfHRV$types <- as.character(r$dfHRV$types)
      r$dfHRV$measures <- as.character(r$dfHRV$measures)
      output$HRVtable <- renderTable({
        validate(need(nrow(r$dfphysio) > 0, print_nodata_msg()))
        dfHRV
      })


      # ###################
      # #Plots the powerband calculations from above, ymax can be changed to change the y-max value on ULF VLF LF and HF graphs while ymaxratio changes the max y value on the LF/HF graph.
      powerBandPlotX <- PlotPowerBand(r$hrv.data, indexFreqAnalysis = 1, ymax = 1200, ymaxratio = 16)
      powerBandPlotXRec <- recordPlot()
      dev.off()
      output$powerBandPlot <- renderPlot({
        validate(need(nrow(r$dfphysio) > 0, print_nodata_msg()))
        print(powerBandPlotXRec)
      })




      output$poincarePlot <- renderPlot({
        validate(need(nrow(r$dfphysio) > 0, print_nodata_msg()))
        print(poincareRecordplot)
      })

      output$EDAplot <- renderPlotly({
        validate(need(nrow(r$dfphysio) > 0, print_nodata_msg()))
        EDAplotX <- ggplot(r$dfphysio, aes(x = TimeLine, y = EDAsmoothed)) +
          ylab("conductivity in...?") +
          xlab("time line in seconds") +
          geom_line() +
          theme_bw() +
          scale_y_continuous(breaks = seq(0, max(r$dfIBI$IBI), 200)) +
          scale_x_continuous(breaks = seq(0, max(r$dfIBI$TimeLine), 1)) +
          expand_limits(x = 0, y = 0) +
          facet_grid(rows = vars(TimeStamp))
        ggplotly(p = EDAplotX) %>% config(scrollZoom = TRUE)
      })

      output$EDAplotBW <- renderPlotly({
        validate(need(nrow(r$dfphysio) > 0, print_nodata_msg()))
        EDAplotbwX <- ggplot(r$dfphysio, aes(x = TimeLine, y = EDAsmoothedbw)) +
          ylab("conductivity in...?") +
          xlab("time line in seconds") +
          geom_line() +
          theme_bw() +
          scale_y_continuous(breaks = seq(0, max(r$dfIBI$IBI), 200)) +
          scale_x_continuous(breaks = seq(0, max(r$dfIBI$TimeLine), 1)) +
          expand_limits(x = 0, y = 0) +
          facet_grid(rows = vars(TimeStamp))
        ggplotly(p = EDAplotbwX) %>% config(scrollZoom = TRUE)
      })
    }
  }
}
