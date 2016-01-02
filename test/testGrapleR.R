#install_github("GRAPLE/GrapleR")
library("bitops")
library("RCurl")
library('GLMr')
library("glmtools")
library("GrapleR")


graplerURL<-"http://128.227.150.20:5000"
expRootDir<-"c:/Workspace/SimRoot/Sims"
setwd(expRootDir)

#print(GrapleCheckService(graplerURL))

#Experiment -1
expId1<-GraplePostProcessRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleGetPostProcessExperimentResults(graplerURL, expId1)

expRootDir<-"c:/Workspace/SimRoot/SimsWithFilter"
filterName <- "Filter1.R"
setwd(expRootDir)

#Experiment 0
expId2<-GraplePostProcessRunExperiment(graplerURL, expRootDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId2)
GrapleGetPostProcessExperimentResults(graplerURL, expId2)

#Experiment 1
expId<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId)
GrapleGetExperimentResults(graplerURL, expId)

#Experiment 2
simDir="c:/Workspace/SimRoot/Sims/mySim"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=10
expId<-GrapleRunExperimentSweep(graplerURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
GrapleCheckExperimentCompletion(graplerURL, expId)
GrapleGetExperimentResults(graplerURL, expId)
GrapleGetSimResult(graplerURL, expId, 3)

#Experiemnt 3
simDir="C:/Workspace/SimRoot"
JobFileName="sweepexp.tar.gz"
expId<-GrapleRunExperimentJob(graplerURL, simDir, JobFileName)
GrapleCheckExperimentCompletion(graplerURL, expId)
GrapleGetExperimentJobResults(graplerURL, expId)
GrapleGetSimResult(graplerURL, expId, 5)
