
validate_url <- function(url){
  valid_url <- FALSE

  valid_url = tryCatch({
    http_error(url)
  }, error = function(e) {
    valid_url <- FALSE
  })
  return(valid_url)
}

check_graple <- function(object) {
  errors <- character()
  if(length(object@GWSURL) > 0)
  {
    valid_url <- validate_url(object@GWSURL)

    if (!valid_url) {
      msg <- paste("Invalid url")
      errors <- c(errors, msg)
    }
  }

  if(length(object@ExpRootDir) > 0)
  {
    if(!dir.exists(input_dir))
    {
      msg <- paste("Experiment root directory does not exist")
      errors <- c(errors, msg)
    }
  }

  if(length(object@ResultsDir) > 0)
  {
    if(!dir.exists(results_dir))
    {
      msg <- paste("Results directory does not exist")
      errors <- c(errors, msg)
    }
  }

  if(!check_subdirectory(object))
  {
    msg <- paste("Exp Root Dir/Result directory is a sub-directory of Result/Input")
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

check_subdirectory <- function(object){
  input_dir <- object@ExpRootDir
  results_dir <- object@ResultsDir
  valid_directories <- FALSE
  if(((length(input_dir) > 0 && length(results_dir) > 0) && (length(grep(input_dir,results_dir)) || length(grep(results_dir, input_dir)))>0))
    valid_directories <- FALSE
  else
    valid_directories <- TRUE
  return(valid_directories)
}

getResultsDirName <- function(object){
  if(length(object@ExpName) > 0)
    return(object@ExpName)
  else
    return(object@JobID)
}

Graple <- setClass("Graple", slots = c(GWSURL = "character", ExpRootDir="character", ResultsDir="character", JobID="character",
                                       StatusCode="numeric", StatusMsg="character", ExpName="character", TempDir="character",
                                       Retention ="numeric"), prototype = list(GWSURL="http://graple.acis.ufl.edu",
                                                                                                       TempDir=tempdir()), validity = check_graple)

setGeneric(name="setTempDir",
           def=function(grapleObject,path)
           {
             standardGeneric("setTempDir")
           }
)

setGeneric(name="setSubmissionURL",
           def=function(grapleObject,url)
           {
             standardGeneric("setSubmissionURL")
           }
)

setGeneric(name="setExpName",
           def=function(grapleObject,expName)
           {
             standardGeneric("setExpName")
           }
)

setGeneric(name="setExperimentDir",
           def=function(grapleObject,path)
           {
             standardGeneric("setExperimentDir")
           }
)

setGeneric(name="setResultsDir",
           def=function(grapleObject,path)
           {
             standardGeneric("setResultsDir")
           }
)

setGeneric(name="GrapleCheckService",
           def=function(grapleObject)
           {
             standardGeneric("GrapleCheckService")
           }
)

setGeneric(name="GrapleRunExperiment",
           def=function(grapleObject, filterName)
           {
             standardGeneric("GrapleRunExperiment")
           }
)

setGeneric(name="GrapleCheckExperimentCompletion",
           def=function(grapleObject)
           {
             standardGeneric("GrapleCheckExperimentCompletion")
           }
)

setGeneric(name="GrapleGetExperimentResults",
           def=function(grapleObject)
           {
             standardGeneric("GrapleGetExperimentResults")
           }
)

setGeneric(name="GrapleRunSweepExperiment",
           def=function(grapleObject, filterName)
           {
             standardGeneric("GrapleRunSweepExperiment")
           }
)

setGeneric(name="GrapleGetExperimentJobResults",
           def=function(grapleObject)
           {
             standardGeneric("GrapleGetExperimentJobResults")
           }
)

setGeneric(name="GrapleAbortExperiment",
           def=function(grapleObject)
           {
             standardGeneric("GrapleAbortExperiment")
           }
)

setMethod(f="setTempDir",
          signature="Graple",
          definition=function(grapleObject,path)
          {
            if(dir.exists(path)){
              grapleObject@TempDir <- path
              grapleObject@StatusCode <- 1
              grapleObject@StatusMsg <- "Temporary directory set to directory provided"
            }
            else{
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Temporary directory provided does not exist"
            }
            return(grapleObject)
          }
)

setMethod(f="setSubmissionURL",
          signature="Graple",
          definition=function(grapleObject,url)
          {
            grapleObject@GWSURL <- url
            return(grapleObject)
          }
)

setMethod(f="setExpName",
          signature="Graple",
          definition=function(grapleObject,expName)
          {
            if(length(grapleObject@ExpName) > 0)
            {
              grapleObject@ExpName <- expName
              grapleObject@StatusCode <- 1
              grapleObject@StatusMsg <- "Experiment name set to the name provided"
            }
            else
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Invalid Experiment Name"
            }
            return(grapleObject)
          }
)

setMethod(f="setExperimentDir",
          signature="Graple",
          definition=function(grapleObject,path)
          {
            if(length(path) > 0)
            {
              grapleObject@ExpRootDir <- path
              if(!dir.exists(grapleObject@ExpRootDir))
              {
                grapleObject@ExpRootDir <- character(0)
                grapleObject@StatusCode <- -1
                grapleObject@StatusMsg <- "Experiment root directory provided does not exist"
              }
              else if(length(grapleObject@ResultsDir) > 0 && !check_subdirectory(grapleObject))
              {
                grapleObject@ExpRootDir <- character(0)
                grapleObject@StatusCode <- -1
                grapleObject@StatusMsg <- "Exp Root Dir/Result directory is a sub-directory of Result/Input"
              }
              else
              {
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- "Experiment root directory has been set to directory provided"
              }
            }
            return(grapleObject)
          }
)

setMethod(f="setResultsDir",
          signature="Graple",
          definition=function(grapleObject,path)
          {
            if(length(path) > 0)
            {
              grapleObject@ResultsDir <- path
              if(!dir.exists(grapleObject@ResultsDir))
              {
                grapleObject@StatusCode <- -1
                grapleObject@StatusMsg <- "Results directory provided does not exist"
              }
              else if(length(grapleObject@ExpRootDir) > 0 && !check_subdirectory(grapleObject))
              {
                grapleObject@StatusCode <- -1
                grapleObject@StatusMsg <- "Exp Root Dir/Result directory is a sub-directory of Result/Input"
              }
              else
              {
                grapleObject@ResultsDir <- path
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- "Experiment root directory has been set to directory provided"
              }
            }
            return(grapleObject)
          }
)

setMethod(f="GrapleCheckService",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl<-paste(grapleObject@GWSURL, "service_status", sep="/")
            status<- getURL(qurl)
            grapleObject@StatusCode <- 1
            grapleObject@StatusMsg <- paste(toString(fromJSON(status)[1]),toString(fromJSON(status)[2]))
            return(grapleObject)
          }
)

setMethod(f="GrapleRunExperiment",
          signature="Graple",
          definition=function(grapleObject, filterName)
          {
            if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Temporary directory provided does not exist"
            }
            else if(length(grapleObject@ExpRootDir)<=0 || !dir.exists(grapleObject@ExpRootDir)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory provided does not exist"
            }
            else
            {
              td<-getwd()
              setwd(grapleObject@TempDir)
              if(file.exists("sim.tar.gz")) file.remove("sim.tar.gz")
              tarfile = file.path(getwd(), "sim.tar.gz")
              setwd(grapleObject@ExpRootDir)
              tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")

              if(missing(filterName)){
                qurl <- paste(grapleObject@GWSURL, "GrapleRun", sep="/")
              }
              else{
                qurl <- paste(grapleObject@GWSURL, "GrapleRun", filterName, sep="/")
              }
              expid = postForm(qurl, files=fileUpload(tarfile))
              grapleObject@JobID <- toString(fromJSON(expid)[1])
              if(nchar(grapleObject@JobID) == 40){
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- "The simulation was submitted successfully"
              }

              if (file.exists(tarfile)) file.remove(tarfile)
              setwd(td)

            }
            return (grapleObject)
          }
)

setMethod(f="GrapleCheckExperimentCompletion",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl <- paste(grapleObject@GWSURL, "GrapleRunStatus", grapleObject@JobID, sep="/")
            status <- getURL(qurl)
            grapleObject@StatusCode <- 1
            grapleObject@StatusMsg <- toString(fromJSON(status)[1])
            return (grapleObject)
          }
)

setMethod(f="GrapleGetExperimentResults",
          signature="Graple",
          definition=function(grapleObject)
          {
            if(length(grapleObject@ResultsDir)<=0 || !dir.exists(grapleObject@ResultsDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Results directory provided does not exist"
            }
            else if(length(grapleObject@JobID) <= 0)
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "No JobID, Experiment Job ID not provided"
            }
            else if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Temporary directory provided does not exist"
            }
            else if(getResultsDirName(grapleObject) %in% list.files(grapleObject@ResultsDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- paste("Directory with name as ExpName/JobID found in results dir,
                                              please delete and try again", sep ="")
            }
            else{
              td<-setwd(grapleObject@ResultsDir)
              qurl<-paste(grapleObject@GWSURL, "GrapleRunResults", grapleObject@JobID, sep="/")
              status<- getURL(qurl)
              if(toString(fromJSON(status)[2]) == "success"){
                qurl <- paste(grapleObject@GWSURL, grapleObject@JobID, "Results", "output.tar.gz", sep="/")
                resultfile <- file.path(grapleObject@TempDir,  "results.tar.gz")
                download.file(qurl, resultfile)
                setwd(grapleObject@TempDir)
                resultPath <- paste(grapleObject@ResultsDir, getResultsDirName(grapleObject), sep="/")
                file.copy("results.tar.gz", resultPath)
                file.remove("results.tar.gz")
                setwd(resultPath)
                untar("results.tar.gz")
                file.remove("results.tar.gz")
                files <- list.files(".", pattern = "\\.bz2\\.tar$")
                lapply(files, function(x){untar(x); file.remove(x)})
                setwd(td)
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste('The results have been downloaded to ', resultPath, sep ="")
              }
            }
            return(grapleObject)
          }
)

setMethod(f="GrapleRunSweepExperiment",
          signature="Graple",
          definition=function(grapleObject, filterName)
          {
            if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Temporary directory provided does not exist"
            }
            else if(length(grapleObject@ExpRootDir)<=0 || !dir.exists(grapleObject@ExpRootDir)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory provided does not exist"
            }
            else{
              td<-getwd()
              setwd(grapleObject@TempDir)
              if(file.exists("sim.tar.gz")) file.remove("sweepexp.tar.gz")
              tarfile = file.path(getwd(), "sweepexp.tar.gz")
              setwd(grapleObject@ExpRootDir)
              tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")
              if(missing(filterName)){
                qurl <- paste(grapleObject@GWSURL, "GrapleRunMetSample", sep="/")
              }
              else{
                qurl <- paste(grapleObject@GWSURL, "GrapleRunMetSample", filterName, sep="/")
              }

              status <- postForm(qurl, files=fileUpload(tarfile))
              grapleObject@JobID <- toString(fromJSON(status)[2])
              if(nchar(grapleObject@JobID) == 40){
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- "The simulation was submitted successfully"
              }

              if (file.exists(tarfile)) file.remove(tarfile)
              setwd(td)
            }
            return (grapleObject)
          }
)

setMethod(f="GrapleGetExperimentJobResults",
          signature="Graple",
          definition=function(grapleObject)
          {
            if(length(grapleObject@ResultsDir)<=0 || !dir.exists(grapleObject@ResultsDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Results directory provided does not exist"
            }
            else if(length(grapleObject@JobID) <= 0)
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "No JobID, Experiment Job ID not provided"
            }
            else if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Temporary directory provided does not exist"
            }
            else if(getResultsDirName(grapleObject) %in% list.files(grapleObject@ResultsDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- paste("Directory with name as ExpName/JobID found in results dir,
                                              please delete and try again", sep ="")
            }
            else{
              td<-getwd()
              qurl <- paste(grapleObject@GWSURL, "GrapleRunResultsMetSample", grapleObject@JobID, sep="/")
              status<- getURL(qurl)

              if(toString(fromJSON(status)[2]) == "success"){
                qurl <- paste(grapleObject@GWSURL, grapleObject@JobID, "Results", "output.tar.gz", sep="/")
                resultfile <- file.path(grapleObject@TempDir,  "results.tar.gz")
                download.file(qurl, resultfile)
                setwd(grapleObject@TempDir)
                resultPath <- paste(grapleObject@ResultsDir, getResultsDirName(grapleObject), sep="/")
                file.copy("results.tar.gz", resultPath)
                file.remove("results.tar.gz")
                setwd(resultPath)
                untar("results.tar.gz")
                file.remove("results.tar.gz")
                files <- list.files(".", pattern = "\\.bz2\\.tar$")
                lapply(files, function(x){untar(x); file.remove(x)})
                setwd(td)
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste('The results have been downloaded to ', resultPath, sep = "")
              }
            }
            return(grapleObject)
          }
)

setMethod(f="GrapleAbortExperiment",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl <- paste(grapleObject@GWSURL, "GrapleAbort", grapleObject@JobID, sep="/")
            status<- getURL(qurl)
            return (fromJSON(status))
          }
)
