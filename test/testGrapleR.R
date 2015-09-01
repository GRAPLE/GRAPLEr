#install_github("kcratie/GrapleR")
library("bitops")
library("RCurl")
library('GLMr')
library("glmtools")
library("GrapleR")

graplerURL<-"http://graple-service.cloudapp.net:80"
expRootDir<-"c:/users/kcratie/Workspace/SimRoot/Sims"
setwd(expRootDir)

print(GrapleCheckService(graplerURL))

#Experiment 1
expId<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId)
GrapleGetExperimentResults(graplerURL, expId)



#Experiment 2
simDir="c:/users/kcratie/Workspace/SimRoot/Sims/mySim"
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
simDir="C:/users/kcratie/Workspace/SimRoot"
JobFileName="SweepExperiment.zip"
expId<-GrapleRunExperimentJob(graplerURL, simDir, JobFileName)
GrapleCheckExperimentCompletion(graplerURL, expId)
GrapleGetExperimentResults(graplerURL, expId)
GrapleGetSimResult(graplerURL, expId, 5)
