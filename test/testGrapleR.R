#install_github("GRAPLE/GrapleR")
library("bitops")
library("RCurl")
library('GLMr')
library("glmtools")
library("GrapleR")


graplerURL<-"http://graple-service.cloudapp.net:80"
expRootDir<-"c:/Workspace/SimRoot/Sims"
filterName <- "RunSimulation.R"
setwd(expRootDir)

#print(GrapleCheckService(graplerURL))

#Experiment 0
expId<-GraplePostProcessRunExperiment(graplerURL, expRootDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId)
GrapleGetPostProcessExperimentResults(graplerURL, expId)

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
