#install_github("GRAPLE/GrapleR")
library("bitops")
library("RCurl")
library("jsonlite")
library("GRAPLEr")

graplerURL<-"http://graple.acis.ufl.edu"
print(GrapleCheckService(graplerURL))

#Experiment 1
expRootDir<-"c:/ExpRoot/Exp1"
setwd(expRootDir)
expId1<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleGetExperimentResults(graplerURL, expId1)
GrapleAbortExperiment(graplerURL, expId1)

#Experiment 2 - w/ Filter
expRootDir<-"c:/ExpRoot/Exp2"
filterName <- "Filter1.R"
setwd(expRootDir)
expId2<-GrapleRunExperiment(graplerURL, expRootDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId2)
GrapleGetExperimentResults(graplerURL, expId2)

#Experiment 3
simDir3="c:/ExpRoot/Exp3"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=5
setwd(simDir3)
expId3 <- GrapleRunExperimentSweep(graplerURL, simDir3, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
GrapleCheckExperimentCompletion(graplerURL, expId3)
GrapleGetExperimentResults(graplerURL, expId3)

#Experiment 4 - w/ Filter
simDir4="c:/ExpRoot/Exp4"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=10
filterName = "Filter1.R"
setwd(simDir4)
expId4<-GrapleRunExperimentSweep(graplerURL, simDir4, driverFileName, parameterName, startValue, endValue, numberOfIncrements, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId4)
GrapleGetExperimentResults(graplerURL, expId4)

#Experiment 5
simDir5="c:/ExpRoot/Exp5"
setwd(simDir5)
expId5 <- GrapleRunExperimentJob(graplerURL, simDir5)
GrapleCheckExperimentCompletion(graplerURL, expId5)
GrapleGetExperimentJobResults(graplerURL, expId5)

#Experiment 6 - w/ Filter
simDir6="C:/ExpRoot/Exp6"
filterName = "Filter1.R"
setwd(simDir6)
expId6<-GrapleRunExperimentJob(graplerURL, simDir6, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId6)
GrapleGetExperimentJobResults(graplerURL, expId6)

#Experiment 7
expRootDir<-"c:/ExpRoot/Exp1"
setwd(expRootDir)
expId1<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleAbortExperiment(graplerURL, expId1)
