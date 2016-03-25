#install.packages("devtools")
#devtools::install_github("klutometis/roxygen")
#devtools::install_github("GRAPLE/GRAPLEr")
library("bitops")
library("RCurl")
library("jsonlite")
library("GRAPLEr")

graplerURL<-"http://10.244.37.64:5000"
#graplerURL<-"http://graple.acis.ufl.edu"
print(GrapleCheckService(graplerURL))

#Returns the list of post-processing scripts available on the server
print(GrapleListFilters(graplerURL))

print(GrapleCheckVersionCompatibility(graplerURL))

#Experiment 1: Your own handcrafted simulations
expRootDir<-"c:/ExpRoot/Exp1"
setwd(expRootDir)
expId1<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleGetExperimentResults(graplerURL, expId1)

#Experiment 2 - Your own handcrafted simulations w/ post processing R-Filter(experimental feature)
expRootDir<-"c:/ExpRoot/Exp2"
filterName <- "Filter2.R"
setwd(expRootDir)
expId2<-GrapleRunExperiment(graplerURL, expRootDir, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId2)
GrapleGetExperimentResults(graplerURL, expId2)

#Experiment 3: An increment type sweep experiment.
#Paramters are passed directly to funtion on invocation.
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

#Experiment 4: An increment type sweep experiment w/ post processing R-Filter(experimental feature)
simDir4="c:/ExpRoot/Exp4"
driverFileName="met_hourly.csv"
parameterName="AirTemp"
startValue=-2
endValue=2
numberOfIncrements=10
filterName = "Filter2.R"
setwd(simDir4)
expId4<-GrapleRunExperimentSweep(graplerURL, simDir4, driverFileName, parameterName, startValue, endValue, numberOfIncrements, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId4)
GrapleGetExperimentResults(graplerURL, expId4)

#Experiment 5: A sweep experiment using using various distributions for generating ranges.
#Paramters specified via an input file.
simDir5="c:/ExpRoot/Exp5"
setwd(simDir5)
expId5 <- GrapleRunExperimentJob(graplerURL, simDir5)
GrapleCheckExperimentCompletion(graplerURL, expId5)
GrapleGetExperimentJobResults(graplerURL, expId5)

#Experiment 6: A sweep experiment using using various distributions for generating ranges
#w/ post processing R-Filter(experimental feature)
simDir6="C:/ExpRoot/Exp6"
filterName = "Filter2.R"
setwd(simDir6)
expId6<-GrapleRunExperimentJob(graplerURL, simDir6, filterName)
GrapleCheckExperimentCompletion(graplerURL, expId6)
GrapleGetExperimentJobResults(graplerURL, expId6)

#Experiment 7: Aborting an active or failed experiment
expRootDir<-"c:/ExpRoot/Exp1"
setwd(expRootDir)
expId1<-GrapleRunExperiment(graplerURL, expRootDir)
GrapleCheckExperimentCompletion(graplerURL, expId1)
GrapleAbortExperiment(graplerURL, expId1)
