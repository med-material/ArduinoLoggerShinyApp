library('RHRV')
# library(here)
# source("CreateTimeAnalysisByEpisodes.R")


#import IBI data in milliseconds (three digits e.g 569, no decimal) from file

#The line below is not needed when we already have the IBI ready
 dfIBI<-read.csv(file = "IBI clean.txt") #insert .txt file name here

#Calculate the time each heart beat happened  
tsIBI<-as.data.frame(cumsum(c(0, dfIBI[2:nrow(dfIBI),]/1000)))
names(tsIBI)<-c('beats')

#need to write data back to file as I couldn't figure out how to simply inject it into the data structure, the file should be safe to delete after this
write.table(tsIBI$beats, file = "TSibi.txt", sep = ",", qmethod = "double", row.names = FALSE, col.names = FALSE)

#create data structure
hrv.data  = CreateHRVData()

#load the beat data
hrv.data = LoadBeatAscii(hrv.data, "TSibi.txt")

#make a non-interpolated plot of the heart rate
hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)

PlotNIHR(hrv.data, main = "niHR",Tags = "all")
  
#Create a time analysis, the values here are the same as the default  
hrv.data = CreateTimeAnalysis(hrv.data, size = 200, interval = 7.8125)
  
#Do the frequency analysis
hrv.data = CreateFreqAnalysis(hrv.data)


#Creates a power bands plot to see the values of LF/HF etc. over time
hrv.data =
    CalculatePowerBand(hrv.data , indexFreqAnalysis = 1,
                       size = 100, shift = 2, type = "fourier",
                       ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                       LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )

#create nonlinear analysis
hrv.data=CreateNonLinearAnalysis(hrv.data)
hrv.data=NonlinearityTests(hrv.data)
hrv.data=PoincarePlot(hrv.data,indexNonLinearAnalysis = 1,timeLag = 1,confidenceEstimation = TRUE,confidence = 0.9, doPlot = TRUE)
hd = hrv.data
hd = CreateNonLinearAnalysis(hd)
hd = PoincarePlot(hd, doPlot = T)
poincareRecordplot<<-recordPlot()
dev.off()


#Put all the values into a single variable each for easier display  
SDNN <<- round(hrv.data$TimeAnalysis[[1]]$SDNN,1)
pNN50 <<- round(hrv.data$TimeAnalysis[[1]]$pNN50,1)
rMSSD <<- round(hrv.data$TimeAnalysis[[1]]$rMSSD,1)
avgLF <<- round(mean(hrv.data$FreqAnalysis[[1]]$LF),1)
avgHF <<- round(mean(hrv.data$FreqAnalysis[[1]]$HF),1)
avgLFHF <<- round(avgLF/avgHF,1)
SD1<<-round(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1,1)
SD2<<-round(hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD2,1)

types<-c("Time domain","","","Frequency domain","","","Non-linear","")
measures<-c("SDNN","pNN50","rMSSD","avg LF","avg HF","avg LF/HF","SD1","SD2")
mvalues<-c(SDNN,pNN50,rMSSD,avgLF,avgHF,avgLFHF,SD1,SD2)
dfHRV<<-data.frame(cbind(types,measures,mvalues))



#Plots the powerband calculations from above, ymax can be changed to change the y-max value on ULF VLF LF and HF graphs while ymaxratio changes the max y value on the LF/HF graph.
powerBandPlot<<-PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 1200, ymaxratio = 16)

# see here for more details on size of windows, shifts etc.
# https://cran.r-project.org/web/packages/RHRV/vignettes/RHRV-quickstart.html