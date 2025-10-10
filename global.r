library(RMySQL)
library(plyr)
library(ggplot2)

library(seewave)
library(zoo)
library(RHRV)
library("nonlinearTseries")


# source("HRVhelpers/CalculateCorrelationDimension.R")
source("modules/csv_upload_module.R", local = T)
source("LoadBeatString.R")
options(shiny.fullstacktrace=TRUE)
pdf(NULL)
options(shiny.maxRequestSize=100*1024^2)

my_data <- read.csv("credentials.csv", header = TRUE, sep = ",", colClasses = c("character", "character", "character", "character"))

lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)


mydb <- dbConnect(MySQL(),
  user = my_data[1, "username"],
  # rstudioapi::askForPassword("Database user"),
  password = my_data[1, "password"],
  # rstudioapi::askForPassword("Database password"),
  dbname = my_data[1, "dbname"],
  host = my_data[1, "host"]
)

# RetreiveUniqueColmnVals() Used to get unique values available for a column
# USAGE:
# dtest = RetreiveUniqueColmnVals("Email")
RetreiveUniqueColVals <- function(tablename, column) {
  queryString <- paste("SELECT DISTINCT", column, "FROM", tablename, sep = " ")
  res <- dbSendQuery(mydb, queryString)
  vals <- fetch(res, n = -1)
  dbClearResult(dbListResults(mydb)[[1]])
  return(unlist(vals)) # if there are several values, they arrive as a list, so unlist them on arrival.
}

rt_accounts <- RetreiveUniqueColVals("reactiontime", "Email")
synch_accounts <- RetreiveUniqueColVals("synch", "Email")
physio_accounts <- RetreiveUniqueColVals("EDAIBISerial", "Email")

all_accounts <- unique(c(rt_accounts, synch_accounts, physio_accounts))


# RetreiveDataSet() Used to query for a specific dataset.
# Setting colvalue to NULL retreives all data.
# USAGE:
# dtest = RetreiveDataSet("reactiontime","Email","mhel@create.aau.dk")
RetreiveDataSet <- function(tablename, column, colvalue) {
  queryString <- "SELECT *"
  queryString <- paste(queryString, "FROM", tablename, sep = " ")
  if (colvalue != "NA") {
    queryString <- paste(queryString, "WHERE", column, "= ", sep = " ")
    queryString <- paste(queryString, "\'", colvalue, "\'", sep = "")
  }
  print(queryString)
  res <- dbSendQuery(mydb, queryString)
  df <- fetch(res, n = -1)
  dbClearResult(dbListResults(mydb)[[1]])
  return(df)
}

# RefreshDataSet is a helper function called in the app for refreshing ReactionTime and Synch datasets.
# Setting colfilter to NULL retreives all data.
# USAGE:
# RefreshDataSets("mhel@create.aau.dk")
RefreshDataSets <- function(colfilter) {
  if (colfilter == "-1") {
    # -1 is the default value R Shiny uses on startup.
    return()
  }

  dfrt <<- RetreiveDataSet("reactiontime", "Email", colfilter)
  dfsynch <<- RetreiveDataSet("synch", "Email", colfilter)
  dfphysio <<- RetreiveDataSet("EDAIBISerial", "Email", colfilter)

  # REFRESH REACTION TIME DATASET
  if (nrow(dfrt) > 0) {
    dfrt$Intens <<- as.factor(dfrt$Intens)
    dfrt$Intens <<- factor(dfrt$Intens, levels = c("Low", "High"))
    dfrt$ReactionTimeRounded <<- round(dfrt$ReactionTime, digits = -1)
    dfrt$Modal <<- as.factor(dfrt$Modal)
    dfrt$RunningTrialNum
    dfrt <<- dfrt %>% arrange(TimeStamp) %>% select(Email,SessionID) %>% distinct() %>% mutate(runningTrialNum=1:n()) %>% merge(dfrt)
  }
  # REFRESH SYNCH DATASET
  if (nrow(dfsynch) > 0) {
    dfsynch$run <<- floor(dfsynch$TrialNo / 21)
    dfsynch$runTrialNo <<- ifelse(dfsynch$TrialNo > 20, dfsynch$TrialNo - 20, dfsynch$TrialNo)
    dfsynch$absSynchOffset <<- abs(dfsynch$ReactionTime)
    dfsynch$Intens <<- as.factor(dfsynch$Intens)
    dfsynch$Intens <<- factor(dfsynch$Intens, levels = c("Low", "High"))
    dfsynch$Modal <<- as.factor(dfsynch$Modal)
    dfsynch$MusicalAbility <<- as.factor(dfsynch$MusicalAbility)
  }
  # REFRESH physio DATASET
  if (nrow(dfphysio) > 9) {
    # The session terminates if rollmean() is called on dataframes with less than 10 rows.
    dfphysio$Millis <<- as.integer(dfphysio$Millis)
    dfphysio$EDA <<- as.integer(dfphysio$EDA)
    dfphysio$IBI <<- as.integer(dfphysio$IBI)
    dfphysio$RawPulse <<- as.integer(dfphysio$RawPulse)
    dfphysio$Pressure <<- as.integer(dfphysio$Pressure)
    dfphysio$Button <<- as.integer(dfphysio$Button)

    dfEDAStart <<- dfphysio[, c("TimeStamp", "Email", "PID", "Comment", "Millis")] %>%
      group_by(Email, TimeStamp) %>%
      slice(which.min(Millis))
    dfEDAStart <<- rename(dfEDAStart, EDAStartMillis = Millis)
    dfphysio <<- merge(dfphysio, dfEDAStart, by = c("TimeStamp", "PID", "Comment", "Email"))
    dfphysio$TimeLine <<- (dfphysio$Millis - dfphysio$EDAStartMillis) / 1000
    dfphysio$EDAsmoothed <<- c(rep(NA, 9), rollmean(dfphysio$EDA, 10))
    dfphysio$EDAsmoothedbw <<- bwfilter(dfphysio$EDA, f = 100, n = 5, to = 1)
    dfIBI <<- dfphysio %>% filter(IBI != 0)
    if (nrow(dfIBI) > 9) {
      dfIBI$TimeLine <<- cumsum(c(0, dfIBI[2:nrow(dfIBI), ]$IBI / 1000))
      dfIBIstart <- dfphysio[dfphysio$IBI != 0, c("TimeStamp", "Email", "PID", "Comment", "Millis")] %>%
        group_by(Email, TimeStamp) %>%
        slice(which.min(Millis))
      dfIBIstart <- rename(dfIBIstart, IBIStartMillis = Millis)

    dfphysio <<- merge(dfphysio, dfIBIstart, by = c("TimeStamp", "PID", "Comment", "Email"))
    dfphysio$start <<- dfphysio$Millis - dfphysio$IBIStartMillis
    }
  }
}

RefreshDataLocal <- function(n_dfrt, n_dfsynch, n_dfphysio, n_dfIBI) {
  r <- reactiveValues(
    dfrt = NULL,
    dfsynch = NULL,
    dfphysio = NULL,
    dfIBI = NULL
  )
  
  # REFRESH REACTION TIME DATASET
  if (nrow(n_dfrt) > 0) {
    r$dfrt = n_dfrt
    r$dfrt$Intens <- as.factor(r$dfrt$Intens)
    r$dfrt$Intens <- factor(r$dfrt$Intens, levels = c("Low", "High"))
    r$dfrt$ReactionTimeRounded <<- round(r$dfrt$ReactionTime, digits = -1)
    r$dfrt$Modal <- as.factor(r$dfrt$Modal)
    r$dfrt$RunningTrialNum
    r$dfrt <- r$dfrt %>% arrange(TimeStamp) %>% select(Email,SessionID) %>% distinct() %>% mutate(runningTrialNum=1:n()) %>% merge(dfrt)
  }
  # REFRESH SYNCH DATASET
  if (nrow(n_dfsynch) > 0) {
    r$dfsynch = n_dfsynch
    r$dfsynch$run <- floor(r$dfsynch$TrialNo / 21)
    r$dfsynch$runTrialNo <- ifelse(r$dfsynch$TrialNo > 20, r$dfsynch$TrialNo - 20, r$dfsynch$TrialNo)
    r$dfsynch$absSynchOffset <- abs(r$dfsynch$ReactionTime)
    r$dfsynch$Intens <- as.factor(r$dfsynch$Intens)
    r$dfsynch$Intens <- factor(r$dfsynch$Intens, levels = c("Low", "High"))
    r$dfsynch$Modal <- as.factor(r$dfsynch$Modal)
    r$dfsynch$MusicalAbility <- as.factor(r$dfsynch$MusicalAbility)
  }
  # REFRESH physio DATASET
  if (nrow(n_dfphysio) > 9) {
    r$dfphysio = n_dfphysio
    r$dfIBI = n_dfIBI
    # The session terminates if rollmean() is called on dataframes with less than 10 rows.
    r$dfphysio$Millis <- as.integer(r$dfphysio$Millis)
    r$dfphysio$EDA <- as.integer(r$dfphysio$EDA)
    r$dfphysio$IBI <- as.integer(r$dfphysio$IBI)
    r$dfphysio$RawPulse <- as.integer(r$dfphysio$RawPulse)
    r$dfphysio$Pressure <- as.integer(r$dfphysio$Pressure)
    r$dfphysio$Button <- as.integer(r$dfphysio$Button)
    
    # Some CSV files has wrong S..
    if (is.null(r$dfphysio$TimeStamp)) {
      r$dfphysio$TimeStamp = r$dfphysio$Timestamp
    }
    
    r$dfEDAStart <- r$dfphysio[, c("TimeStamp", "Email", "PID", "Comment", "Millis")] %>%
      group_by(Email, TimeStamp) %>%
      slice(which.min(Millis))
    r$dfEDAStart <- rename(r$dfEDAStart, EDAStartMillis = Millis)
    r$dfphysio <- merge(r$dfphysio, r$dfEDAStart, by = c("TimeStamp", "PID", "Comment", "Email"))
    r$dfphysio$TimeLine <- (r$dfphysio$Millis - r$dfphysio$EDAStartMillis) / 1000
    r$dfphysio$EDAsmoothed <- c(rep(NA, 9), rollmean(r$dfphysio$EDA, 10))
    r$dfphysio$EDAsmoothedbw <- bwfilter(r$dfphysio$EDA, f = 100, n = 5, to = 1)
    r$dfIBI <- r$dfphysio %>% filter(IBI != 0)
    if (nrow(n_dfIBI) > 9) {
      r$dfIBI$TimeLine <- cumsum(c(0, r$dfIBI[2:nrow(r$dfIBI), ]$IBI / 1000))
      r$dfIBIstart <- r$dfphysio[r$dfphysio$IBI != 0, c("TimeStamp", "Email", "PID", "Comment", "Millis")] %>%
        group_by(Email, TimeStamp) %>%
        slice(which.min(Millis))
      r$dfIBIstart <- rename(r$dfIBIstart, IBIStartMillis = Millis)
      
      r$dfphysio <- merge(r$dfphysio, r$dfIBIstart, by = c("TimeStamp", "PID", "Comment", "Email"))
      r$dfphysio$start <- r$dfphysio$Millis - r$dfphysio$IBIStartMillis
    }
  }
  return(r)
}


dfrt <- data.frame()
dfsynch <- data.frame()
dfphysio <- data.frame()
