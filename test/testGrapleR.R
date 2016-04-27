library(httr)
library(RCurl)
library(jsonlite)
library(GRAPLEr)

# Default SubmissionURL is set to http://graple.acis.ufl.edu, use setSubmissionURL method to change submissionURL
graple <- Graple()

#The result of the method call is stored in the object itself
graple <- GrapleCheckService(graple)
print(graple@StatusMsg)
graple <- GrapleListPostProcessFilters(graple)
print(graple@StatusMsg)
graple <- GrapleChkVersionCompatibility(graple)
print(graple@StatusMsg)

#Batch Experiment
#
#Start a new experiment and setup parameters
grapleExp1 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp1", ResultsDir="D:/GRAPLE/Results/Exp1", TempDir = tempdir())
grapleExp1 <- setExpName(grapleExp1, "BatchExperiment1")

# Change SubmissionURL
#grapleExp1 <- setSubmissionURL(grapleExp1, "http://10.244.37.4:5000")

# Change directories
#grapleExp1 <- setExperimentDir(grapleExp1, "C:/ExpRoot/Exp1")
#grapleExp1 <- setResultsDir(grapleExp1, "C:/ExpRoot/Results/Exp1")
#grapleExp1 <- setTempDir(grapleExp1, "C:/TempDir")
#grapleExp1 <- setSecurityKey(grapleExp1, 'C:/TempDir/APIKey.txt')

#Run the experiment
grapleExp1 <- GrapleRunExperiment(grapleExp1);
print(grapleExp1@StatusMsg)

#check on status and wait until it is ready
grapleExp1 <- GrapleCheckExperimentCompletion(grapleExp1)
while (grapleExp1@StatusMsg != '100.0% complete') {
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
filterName <- "ExtractVariables.R"
grapleExp2 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp2", ResultsDir="D:/GRAPLE/Results/Exp2", TempDir = tempdr)
grapleExp2 <- GrapleRunExperiment(grapleExp2, filterName);
grapleExp2 <- GrapleCheckExperimentCompletion(grapleExp2);
grapleExp2 <- GrapleGetExperimentResults(grapleExp2);

#
#Multiple Sweep Experiments at once
#
grapleExp3 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp3", ResultsDir="D:/GRAPLE/Results/Exp3", TempDir = tempdir())
grapleExp4 <- Graple(ExpRootDir="D:/GRAPLE/ExpRoot/Exp4", ResultsDir="D:/GRAPLE/Results/Exp4", TempDir = tempdir())
grapleExp3 <- setExpName(grapleExp3, "SweepExperiment3")
grapleExp4 <- setExpName(grapleExp4, "SweepExperiment4")
grapleExp3 <- GrapleRunSweepExperiment(grapleExp3)
print(grapleExp3@StatusMsg)
grapleExp4 <- GrapleRunSweepExperiment(grapleExp4)
print(grapleExp4@StatusMsg)

grapleExp3 <- GrapleCheckExperimentCompletion(grapleExp3)
grapleExp4 <- GrapleCheckExperimentCompletion(grapleExp4)
while (grapleExp3@StatusMsg != '100.0% complete' && grapleExp4@StatusMsg != '100.0% complete') {
  Sys.sleep(10);
  grapleExp3 <- GrapleCheckExperimentCompletion(grapleExp3)
  print(paste(grapleExp3@ExpName, grapleExp3@StatusMsg, sep=":"))

  grapleExp4 <- GrapleCheckExperimentCompletion(grapleExp4)
  print(paste(grapleExp4@ExpName, grapleExp4@StatusMsg, sep = ":"))
}

grapleExp3 <- GrapleGetExperimentJobResults(grapleExp3)
print(grapleExp3@StatusMsg)
grapleExp4 <- GrapleGetExperimentJobResults(grapleExp4)
print(grapleExp4@StatusMsg)
