LoadBeatString <- function (HRVData, beatsaux, scale = 1, starttime=NULL, endtime=NULL, datetime = "1/1/1900 0:0:0", verbose = NULL) {
  #-------------------------------
  # Loads beats from a string
  #-------------------------------
  #	x -> string
  #-------------------------------
  
  #HRVData = HandleVerboseArgument(HRVData, verbose)
  
  #VerboseMessage(HRVData$Verbose, 
  #               "Loading beats positions from string..")
  
  
  if ( (!is.null(starttime) & is.null(endtime) ) | ( is.null(starttime) & !is.null(endtime) ) ) {
    stop("Bad specification of starttime and endtime")
  }
  
  if (!is.null(starttime) & !is.null(endtime)) {
    if ( (starttime<0) | (endtime<0) | (starttime > endtime) ) {
      stop("Bad specification of starttime and endtime")
    }
  }
  
  #VerboseMessage(HRVData$Verbose, paste("Scale:", scale))

  beats = beatsaux[!duplicated(beatsaux)]
  #if (length(beatsaux) != length(beats)) {
  #  VerboseMessage(HRVData$Verbose, 
  #                 paste("Removed", length(beatsaux) - length(beats),
  #                       "duplicated beats")
  #  )
  #}
  
  if (!is.null(starttime)) {
    lastbeattime = tail(beats,n=1)
    if (endtime > lastbeattime)  {
      stop("Endtime exceeds record length")
    }
    beats <- beats[beats>=starttime & beats<=endtime]
  #  VerboseMessage(HRVData$Verbose, paste("Init time:", starttime))
  #  VerboseMessage(HRVData$Verbose, paste("End time:", endtime))
  }
  
  datetimeaux = strptime(datetime, "%d/%m/%Y %H:%M:%S")
  if (is.na(datetimeaux)) {
    stop("Date/time format is dd/mm/yyyy HH:MM:SS")
  }
  #VerboseMessage(HRVData$Verbose,
  #               c(paste("Date: ", 
  #                       sprintf("%02d", datetimeaux$mday), "/", 
  #                       sprintf("%02d", 1 + datetimeaux$mon), "/", 
  #                       1900 + datetimeaux$year, "\n"),
  #                 paste("Time: ", 
  #                       sprintf("%02d", datetimeaux$hour), ":", 
  #                       sprintf("%02d", datetimeaux$min), ":", 
  #                       sprintf("%02d", datetimeaux$sec))
  #               )
  #)  
  
  HRVData$datetime = datetimeaux
  HRVData$Beat = data.frame(Time = beats * scale)
  #VerboseMessage(HRVData$Verbose,  
  #               paste("Number of beats:", length(HRVData$Beat$Time)))
  
  return(HRVData)
}
