library(RMySQL)
library(plyr)
library(ggplot2)

my_data <- read.csv("credentials.csv", header=TRUE,sep=",", colClasses=c("character","character","character","character"))

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


mydb = dbConnect(MySQL(),
                 user=my_data[1, "username"],
                 # rstudioapi::askForPassword("Database user"),
                 password=my_data[1, "password"],
                 # rstudioapi::askForPassword("Database password"),
                 dbname=my_data[1, "dbname"],
                 host=my_data[1, "host"])


# RETREIVE REACTION TIME DATA
rs = dbSendQuery(mydb, "SELECT * FROM v_reactiontime")
dfrt<- fetch(rs, n=-1);
dbClearResult(dbListResults(mydb)[[1]])

# REFACTOR COLUMNS
dfrt$Intens<-as.factor(dfrt$Intens)
dfrt$Intens<-factor(dfrt$Intens,levels = c("Low", "High"))
dfrt$ReactionTimeRounded = round(dfrt$ReactionTime, digits=-1)

FetchDatas <- function(conditionLists = list(), option = "*" , tablename="v_reactiontime")
{
  queryString = GenerateQuery(conditionLists, option, tablename)
  #print(dbGetQuery(mydb, queryString))
  return(dbGetQuery(mydb, queryString))
}

createframe <- function(dfname, mail, modality){
  dfname <- subset(contage, synchdataLED.Email == mail & synchdataLED.Modal == modality)
  return(dfname)
}


#contage = count(reactiondatatest, vars =c("reactiondatatest$ReactionTimeRounded"))
#varaupif = contage$reactiondatatest.ReactionTimeRounded
#varraupif = contage$freq
#x <- data.frame("reactiontimecalculated" = contage$reactiondatatest.ReactionTimeRounded, "frequence" = contage$freq)
#x
#varraupif
#max(contage$freq)

GenerateQuery <- function(conditionLists, option , tablename)
{
  queryString = paste("SELECT", option, sep = " ")
  queryString = paste(queryString, "FROM",tablename, sep = " ")
  
  if (length(conditionLists) == 0)
  {
    #print(queryString)
    return(queryString)
  }
  
  conditionLink = "OR"
  listLink = "AND"
  
  queryString = paste(queryString, "WHERE", sep = " ")
  
  for (i in 1:length(conditionLists)){
    queryString = paste(queryString, "(", sep = "")
    for (j in 1:length(conditionLists[[i]])){
      queryString = paste(queryString, conditionLists[[i]][[j]], sep = " ")
      if (j < length(conditionLists[[i]]))
      {
        queryString = paste(queryString, conditionLink, sep = " ")
      }
    }
    queryString = paste(queryString, ")", sep = "")
    if (i < length(conditionLists))
    {
      queryString = paste(queryString, listLink, sep = " ")
    }
  }
  #print(queryString)
  return(queryString)
}


GetField <- function(fieldName, fetchResult)
{
  return(fetchResult[[fieldName]])
}


CountField <- function(fieldName = "*", conditions = list())
{
  tempField <- paste("COUNT(DISTINCT ", fieldName, sep = "")
  tempField <- paste(tempField, ")", sep = "")
  return(GetField(tempField, FetchDatas(conditions, tempField)))
  }


GenerateSelectChoices <- function(default = "", text = "", fieldName, conditions = list(), tablename="v_reactiontime", extraInfo = list())
{
  tempList <- list()
  tempList[[default]] <- -1
  fieldList <- GetField(fieldName, FetchDatas(conditions,tablename = tablename, paste("DISTINCT", fieldName)))
  extraTextString = ""
  
  if(length(fieldList) == 0)
    return(tempList)
  
  for(i in 1:length(fieldList))
  {
    if(length(extraInfo) != 0)
    {
      extraTextString = ""
      extraText <- list()
      for(j in 1:length(extraInfo))
      {
        #print(j)
        tempContitions <- conditions
        tempContitions[[length(tempContitions) + 1]] <- paste(toString(fieldName), " = ", fieldList[[i]], sep = "")
        extraText[[j]] <- GetField(extraInfo[[j]], FetchDatas(tempContitions, paste("DISTINCT", extraInfo[[j]])))[[1]]
        #print("test")
      }
      
      
      for(j in 1:length(extraText))
      {
        extraTextString <- paste(extraTextString, toString(extraText[[j]]), sep = "")
      }
    }
    
    resultString <- ""
    
    if (is.numeric(fieldList[[i]]))
    {
      resultString <- paste(text, fieldList[[i]], sep = " ")
    }
    else
    {
      resultString <- fieldList[[i]]
    }
    
    if(extraTextString != "")
    {
      extraTextString <- paste("(", extraTextString, ")", sep = "")
      resultString <- paste(resultString, extraTextString, sep = " ")
    }
    
    tempList[[resultString]] <- fieldList[[i]]
  }
  return(tempList)
}
synchdataLED = FetchDatas(option = "*", tablename = "synch")

synchdataLED$ReactionTimeRounded = round(as.numeric(synchdataLED$ReactionTime), digits=-1)
contage = count(synchdataLED, vars =c("synchdataLED$ReactionTimeRounded","synchdataLED$Email","synchdataLED$TimeStamp","synchdataLED$Modal","synchdataLED$MusicalAbility","synchdataLED$Comment"))

reactiondatatest = FetchDatas(option = "TimeStamp, Email, TrialNo, ReactionTime, Modal", tablename = "v_reactiontime")
reactiondatatest$ReactionTimeRounded = round(as.numeric(reactiondatatest$ReactionTime), digits=-1)

