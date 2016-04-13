library(httr)
library(RCurl)
library(jsonlite)
library(GRAPLEr)

graple <- Graple()
#The result of the method call is stored in the object itself
graple <- GrapleCheckService(graple)
print(graple@StatusMsg)

#Batch Experiment
#
#Start a new experiment and setup parameters
grapleExp1 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp1", ResultsDir="D:/GRAPLE/Results/Exp1", TempDir = tempdir())
grapleExp1@ExpName="BatchExperiment1"
grapleExp1 <- setSubmissionURL(grapleExp1, "http://graple.acis.ufl.edu")

#Run the experiment
grapleExp1 <- GrapleRunExperiment(grapleExp1);
print(grapleExp1@StatusMsg)

#check on status and wait until it is ready
grapleExp1 <- GrapleCheckExperimentCompletion(grapleExp1)
while (grapleExp1@StatusMsg != 'completed') {
  Sys.sleep(5);
  grapleExp1 <- GrapleCheckExperimentCompletion(grapleExp1) 
  print(grapleExp1@StatusMsg);
}

#get the experiment results. Extracted to results dir you specified
grapleExp1 <- GrapleGetExperimentResults(grapleExp1);
print(grapleExp1@StatusMsg)

#
##Batch Experiment w/ Filter
#
filterName <- "Filter1.R"
grapleExp2 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp2", ResultsDir="D:/GRAPLE/Results/Exp2", TempDir = tempdr)
grapleExp2 <- GrapleRunExperiment(grapleExp2, filterName);
grapleExp2 <- GrapleCheckExperimentCompletion(grapleExp2);
grapleExp2 <- GrapleGetExperimentResults(grapleExp2);

#
#Multiple Sweep Experiments at once
#
grapleExp3 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp3", ResultsDir="D:/GRAPLE/Results/Exp3", TempDir = tempdir())
grapleExp4 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp4", ResultsDir="D:/GRAPLE/Results/Exp4", TempDir = tempdir())
grapleExp3@ExpName="SweepExperiment3"
grapleExp4@ExpName="SweepExperiment4"
grapleExp3 <- GrapleRunSweepExperiment(grapleExp3)
print(grapleExp3@StatusMsg)
grapleExp4 <- GrapleRunSweepExperiment(grapleExp4)
print(grapleExp4@StatusMsg)

grapleExp3 <- GrapleCheckExperimentCompletion(grapleExp3)
grapleExp4 <- GrapleCheckExperimentCompletion(grapleExp4)
while (grapleExp3@StatusMsg != 'completed' && grapleExp4@StatusMsg != 'completed') {
  Sys.sleep(10);
  grapleExp3 <- GrapleCheckExperimentCompletion(grapleExp3)
  print(paste(grapleExp3@ExpName, grapleExp3@StatusMsg, sep=":"))

  grapleExp4 <- GrapleCheckExperimentCompletion(grapleExp4)
  print(paste(grapleExp3@ExpName, grapleExp4@StatusMsg, sep = ":"))
}

grapleExp3 <- GrapleGetExperimentJobResults(grapleExp3)
print(grapleExp3@StatusMsg)
grapleExp4 <- GrapleGetExperimentJobResults(grapleExp4)
print(grapleExp4@StatusMsg)
