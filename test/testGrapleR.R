library(httr)
library(RCurl)
library(jsonlite)
library(GRAPLEr)

graple <- Graple()
tempdr <- tempdir()
#The result of the method call is stored in the object itself
graple <- GrapleCheckService(graple)
graple@StatusMsg

#Experiment1
grapleExp1Obj <- Graple(ExpRootDir="C:/ExpRoot/Exp1", ResultsDir="C:/Workspace/Results/Exp1", TempDir = tempdr)
grapleExp1Obj <- GrapleRunExperiment(grapleExp1Obj);grapleExp1Obj@StatusMsg
grapleExp1Obj <- GrapleCheckExperimentCompletion(grapleExp1Obj);grapleExp1Obj@StatusMsg
grapleExp1Obj <- GrapleGetExperimentResults(grapleExp1Obj);grapleExp1Obj@StatusMsg

#Experiment2
filterName <- "Filter1.R"
grapleExp2Obj <- Graple(ExpRootDir="C:/ExpRoot/Exp2", ResultsDir="C:/Workspace/Results/Exp2", TempDir = tempdr)
grapleExp2Obj <- GrapleRunExperiment(grapleExp2Obj, filterName);grapleExp2Obj@StatusMsg
grapleExp2Obj <- GrapleCheckExperimentCompletion(grapleExp2Obj);grapleExp2Obj@StatusMsg
grapleExp2Obj <- GrapleGetExperimentResults(grapleExp2Obj);grapleExp2Obj@StatusMsg

#Experiment3
grapleExp3Obj <- Graple(ExpRootDir="C:/ExpRoot/Exp3", ResultsDir="C:/Workspace/Results/Exp3", TempDir = tempdr)
grapleExp3Obj <- GrapleRunSweepExperiment(grapleExp3Obj);grapleExp3Obj@StatusMsg
grapleExp3Obj <- GrapleCheckExperimentCompletion(grapleExp3Obj);grapleExp3Obj@StatusMsg
grapleExp3Obj <- GrapleGetExperimentJobResults(grapleExp3Obj);grapleExp3Obj@StatusMsg

#Experiment4
grapleExp4Obj <- Graple(ExpRootDir="C:/ExpRoot/Exp4", ResultsDir="C:/Workspace/Results/Exp4", TempDir = tempdr)
grapleExp4Obj <- GrapleRunSweepExperiment(grapleExp4Obj);grapleExp4Obj@StatusMsg
grapleExp4Obj <- GrapleCheckExperimentCompletion(grapleExp4Obj);grapleExp4Obj@StatusMsg
grapleExp4Obj <- GrapleGetExperimentJobResults(grapleExp4Obj);grapleExp4Obj@StatusMsg
