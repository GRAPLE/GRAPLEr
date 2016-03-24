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
grapleExpObj1 <- Graple(ExpRootDir="C:/Users/kcratie/workspace/ExpRoot/Exp1", ResultsDir="C:/Users/kcratie/workspace/Results/Exp1", TempDir = tempdr)
grapleExpObj1@ExpName="Experiment1"
grapleExpObj1 <- GrapleRunExperiment(grapleExpObj1);grapleExpObj1@StatusMsg
grapleExpObj1 <- GrapleCheckExperimentCompletion(grapleExpObj1);
while (grapleExpObj1@StatusMsg != 'completed') {
  Sys.sleep(5);
  grapleExpObj1 <- GrapleCheckExperimentCompletion(grapleExpObj1);  
  print(grapleExpObj1@StatusMsg);
}
grapleExpObj1 <- GrapleGetExperimentResults(grapleExpObj1);grapleExpObj1@StatusMsg

#Experiment2
filterName <- "Filter1.R"
grapleExpObj2 <- Graple(ExpRootDir="C:/ExpRoot/Exp2", ResultsDir="C:/Workspace/Results/Exp2", TempDir = tempdr)
grapleExpObj2 <- GrapleRunExperiment(grapleExpObj2, filterName);grapleExpObj2@StatusMsg
grapleExpObj2 <- GrapleCheckExperimentCompletion(grapleExpObj2);grapleExpObj2@StatusMsg
grapleExpObj2 <- GrapleGetExperimentResults(grapleExpObj2);grapleExpObj2@StatusMsg

#Experiment3
grapleExp3Obj <- Graple(ExpRootDir="C:/Users/kcratie/workspace/ExpRoot/Exp3", ResultsDir="C:/Users/kcratie/workspace/Results/Exp3", TempDir = tempdr)
grapleExp3Obj <- GrapleRunSweepExperiment(grapleExp3Obj);grapleExp3Obj@StatusMsg
grapleExp3Obj <- GrapleCheckExperimentCompletion(grapleExp3Obj);grapleExp3Obj@StatusMsg
while (grapleExp3Obj@StatusMsg != 'completed') {
  Sys.sleep(5);
  grapleExp3Obj <- GrapleCheckExperimentCompletion(grapleExp3Obj);  
  print(grapleExp3Obj@StatusMsg);
}
grapleExp3Obj <- GrapleGetExperimentJobResults(grapleExp3Obj);grapleExp3Obj@StatusMsg

#Experiment4
grapleExp4Obj <- Graple(ExpRootDir="C:/ExpRoot/Exp4", ResultsDir="C:/Workspace/Results/Exp4", TempDir = tempdr)
grapleExp4Obj <- GrapleRunSweepExperiment(grapleExp4Obj);grapleExp4Obj@StatusMsg
grapleExp4Obj <- GrapleCheckExperimentCompletion(grapleExp4Obj);grapleExp4Obj@StatusMsg
grapleExp4Obj <- GrapleGetExperimentJobResults(grapleExp4Obj);grapleExp4Obj@StatusMsg
