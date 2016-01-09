#install_github("GRAPLE/GrapleR")
library("bitops")
library("RCurl")
library('GLMr')
library("glmtools")
library("GrapleR")

graplerURL<-"http://graple.acis.ufl.edu"
#graplerURL<-"http://graple-service.cloudapp.net"

#Experiment 1
expRootDir<-"c:/Workspace/SimRoot/Exp1"
setwd(expRootDir)

expId1<-GraplePostProcessRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleGetExperimentResults(graplerURL, expId1)

#Experiment 2
expRootDir<-"c:/Workspace/SimRoot/Exp2"
filterName <- "Filter1.R"
setwd(expRootDir)

expId2<-GraplePostProcessRunExperiment(graplerURL, expRootDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId2)
GrapleGetExperimentResults(graplerURL, expId2)

#Experiment 3
simDir="c:/Workspace/SimRoot/Exp3"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=10
setwd(simDir)
expId3 <- GrapleRunExperimentSweep(graplerURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
GrapleCheckExperimentCompletion(graplerURL, expId3)
GrapleGetExperimentResults(graplerURL, expId3)
#GrapleGetSimResult(graplerURL, expId, 3)

#Experiment 4
simDir="c:/Workspace/SimRoot/Exp4"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=10
filterName = "Filter1.R"
setwd(simDir)
expId4<-GrapleRunExperimentSweep(graplerURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId4)
GrapleGetExperimentResults(graplerURL, expId4)
#GrapleGetSimResult(graplerURL, expId, 3)

#Experiment 5
simDir="c:/Workspace/SimRoot/Exp5"
JobFileName="sweepexp.tar.gz"
setwd(simDir)
expId5 <- GrapleRunExperimentJob(graplerURL, simDir, JobFileName)
GrapleCheckExperimentCompletion(graplerURL, expId5)
GrapleGetExperimentResults(graplerURL, expId5)
#GrapleGetSimResult(graplerURL, expId, 5)

#Experiment 6
simDir="C:/Workspace/SimRoot/Exp6"
JobFileName="sweepexp.tar.gz"
filterName = "Filter1.R"
setwd(simDir)
expId6<-GrapleRunExperimentJob(graplerURL, simDir, JobFileName, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId6)
GrapleGetExperimentResults(graplerURL, expId6)
#GrapleGetSimResult(graplerURL, expId, 5)
