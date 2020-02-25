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

# RetreiveUniqueColmnVals() Used to get unique values available for a column
# USAGE:
#dtest = RetreiveUniqueColmnVals("Email")
RetreiveUniqueColVals <- function(tablename, column) {
  queryString = paste("SELECT DISTINCT",column,"FROM",tablename,sep=" ")
  res = dbSendQuery(mydb, queryString)
  vals = fetch(res, n=-1)
  dbClearResult(dbListResults(mydb)[[1]])
  return(unlist(vals)) # if there are several values, they arrive as a list, so unlist them on arrival.
}

rt_accounts = RetreiveUniqueColVals("reactiontime","Email")
synch_accounts = RetreiveUniqueColVals("synch","Email")

all_accounts = unique(c(rt_accounts,synch_accounts))


# RetreiveDataSet() Used to query for a specific dataset. 
# Setting colvalue to NULL retreives all data.
# USAGE:
#dtest = RetreiveDataSet("reactiontime","Email","mhel@create.aau.dk")
RetreiveDataSet <- function(tablename, column, colvalue) {
  queryString = "SELECT *"
  queryString = paste(queryString, "FROM",tablename, sep = " ")
  if (colvalue != "NA") {
    queryString = paste(queryString, "WHERE",column,"= ",sep=" ")
    queryString = paste(queryString,"\'",colvalue,"\'",sep="")
  }
  print(queryString)
  res = dbSendQuery(mydb, queryString)
  df = fetch(res, n=-1)
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
  # REFRESH REACTION TIME DATASET
  dfrt<<- RetreiveDataSet("reactiontime","Email",colfilter)
  dfrt$Intens<<-as.factor(dfrt$Intens)
  dfrt$Intens<<-factor(dfrt$Intens,levels = c("Low", "High"))
  dfrt$ReactionTimeRounded <<- round(dfrt$ReactionTime, digits=-1)
  dfrt$Modal <<- as.factor(dfrt$Modal)
  
  # REFRESH SYNCH DATASET
  dfsynch <<- RetreiveDataSet("synch","Email",colfilter)
  dfsynch$Intens<<-as.factor(dfsynch$Intens)
  dfsynch$Intens<<-factor(dfsynch$Intens,levels = c("Low", "High"))
  dfsynch$Modal<<-as.factor(dfsynch$Modal)
  dfsynch$MusicalAbility<<-as.factor(dfsynch$MusicalAbility)  
}


dfrt <- data.frame()
dfsynch <- data.frame()