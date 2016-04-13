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
grapleExp1Obj <- graple
grapleExp1Obj <- setTempDir(grapleExp1Obj, tempdr)
grapleExp1Obj <- setSubmissionURL(grapleExp1Obj, "http://10.244.37.64:5000")
grapleExp1Obj <- setExpName(grapleExp1Obj, "Experiment1")
grapleExp1Obj <- setExperimentDir(grapleExp1Obj, "C:/ExpRoot/Exp1")
grapleExp1Obj <- setResultsDir(grapleExp1Obj, "C:/Workspace/Results/Exp1")
grapleExp1Obj <- GrapleRunExperiment(grapleExp1Obj);grapleExp1Obj@StatusMsg
grapleExp1Obj <- GrapleCheckExperimentCompletion(grapleExp1Obj);grapleExp1Obj@StatusMsg
grapleExp1Obj <- GrapleGetExperimentResults(grapleExp1Obj);grapleExp1Obj@StatusMsg


#Experiment2
filterName <- "ExtractVariables.R"
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
filterName <- "Filter1.R"
grapleExp4Obj <- Graple(ExpRootDir="C:/ExpRoot/Exp4", ResultsDir="C:/Workspace/Results/Exp4", TempDir = tempdr)
grapleExp4Obj <- GrapleRunSweepExperiment(grapleExp4Obj, filterName);grapleExp4Obj@StatusMsg
grapleExp4Obj <- GrapleCheckExperimentCompletion(grapleExp4Obj);grapleExp4Obj@StatusMsg
grapleExp4Obj <- GrapleGetExperimentJobResults(grapleExp4Obj);grapleExp4Obj@StatusMsg
