#install_github("GRAPLE/GrapleR")
library("bitops")
library("RCurl")
library('GLMr')
library("glmtools")
library("GrapleR")
library("jsonlite")

#graplerURL<-"http://graple.acis.ufl.edu"
graplerURL<-"http://graple-service.cloudapp.net"

#Experiment 1
#expRootDir<-"C:/Workspace/SimRoot1/Exp1/mySim"
expRootDir<-"c:/Workspace/SimRoot/Exp1"
setwd(expRootDir)

expId1<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleGetExperimentResults(graplerURL, expId1)

#Experiment 2
expRootDir<-"c:/Workspace/SimRoot/Exp2"
filterName <- "Filter1.R"
setwd(expRootDir)

expId2<-GrapleRunExperiment(graplerURL, expRootDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId2)
GrapleGetExperimentResults(graplerURL, expId2)

#Experiment 3
simDir="c:/Workspace/SimRoot1/Exp3"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=5
setwd(simDir)
expId3 <- GrapleRunExperimentSweep(graplerURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
GrapleCheckExperimentCompletion(graplerURL, expId3)
GrapleGetExperimentResults(graplerURL, expId3)

#Experiment 4
simDir="c:/Workspace/SimRoot1/Exp4"
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

#Experiment 5
simDir="c:/Workspace/SimRoot1/Exp5"
setwd(simDir)
expId5 <- GrapleRunExperimentJob(graplerURL, simDir)
GrapleCheckExperimentCompletion(graplerURL, expId5)
GrapleGetExperimentJobResults(graplerURL, expId5)

#Experiment 6
simDir="C:/Workspace/SimRoot/Exp6"
filterName = "Filter1.R"
setwd(simDir)
expId6<-GrapleRunExperimentJob(graplerURL, simDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId6)
GrapleGetExperimentJobResults(graplerURL, expId6)
