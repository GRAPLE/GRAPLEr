
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("GRAPLEr has been developed with support from a supplement the PRAGMA award (NSF OCI-1234983) by
  Ken Subratie, Saumitra Aditya, Satish Mahesula, Renato J. Figueiredo, Cayelan C. Carey and Paul C. Hanson.
  For more information, please visit graple.org")
}

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
    if(!dir.exists(object@ExpRootDir))
    {
      msg <- paste("Experiment root directory does not exist")
      errors <- c(errors, msg)
    }
  }

  if(length(object@ResultsDir) > 0)
  {
    if(!dir.exists(object@ResultsDir))
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

filesPresent <- function(object){
  if(length(list.files(path = object@ExpRootDir, recursive = FALSE)) != length(list.dirs(path = object@ExpRootDir, recursive = FALSE)))
    return(TRUE)
  else
    return(FALSE)
}

filePresent <- function(dirPath, fileName){
  filesList <- list.files(path = dirPath, recursive = FALSE)
  if(fileName %in% filesList)
    return(TRUE)
  else
    return(FALSE)
}

validate_json <- function(jsonFilePath)
{
  valid_JSON <- TRUE
  distribution_type <- ''
  distribution_types <- list('uniform', 'binomial', 'normal', 'poisson')

  jsonFile <- fromJSON(jsonFilePath, simplifyVector = FALSE)

  num_iterations <- jsonFile$num_iterations

  if(is.null(num_iterations))
    distribution_type <- 'linear'
  else
    distribution_type <- 'non-linear'

  if(distribution_type == 'linear')  {
    steps = 1
    for (expFile in 1:length(jsonFile$ExpFiles)) {
      for (vb in 1:length(jsonFile$ExpFiles[[expFile]]$variables[[1]])) {
        distribution = jsonFile$ExpFiles[[expFile]]$variables[[1]][[vb]]$distribution
        if(is.null(distribution) || distribution != 'linear'){
          valid_JSON <- FALSE
          break
        }
        else if (distribution == 'linear'){
          var_steps = jsonFile$ExpFiles[[expFile]]$variables[[1]][[vb]]$steps
          if(is.null(steps)){
            valid_JSON <- FALSE
            break
          }
          else{
            steps = steps * (var_steps + 1)
          }
        }
      }
    }
    if(steps >= 100000)
      valid_JSON <- FALSE
  }
  else if(distribution_type == 'non-linear'){
    for (expFile in 1:length(jsonFile$ExpFiles)) {
      for (vb in 1:length(jsonFile$ExpFiles[[expFile]]$variables[[1]])) {
        distribution = jsonFile$ExpFiles[[expFile]]$variables[[1]][[vb]]$distribution
        if(is.null(distribution) || !(distribution %in% distribution_types)){
          valid_JSON <- FALSE
          break
        }
      }
    }
  }
  else
  {
    valid_JSON <- FALSE
  }
  return(list(valid_JSON, distribution_type))
}

Graple <- setClass("Graple", slots = c(GWSURL = "character", ExpRootDir="character", ResultsDir="character", JobID="character",
                                       StatusCode="numeric", StatusMsg="character", ExpName="character", TempDir="character", SecurityKey="character",
                                       Retention ="numeric", Client_Version_ID="character"), prototype = list(GWSURL="http://graple.acis.ufl.edu",
                                       TempDir=tempdir(), Client_Version_ID = toString(packageVersion("GRAPLEr"))), validity = check_graple)

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

setGeneric(name="setSecurityKey",
           def=function(grapleObject,path)
           {
             standardGeneric("setSecurityKey")
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

setGeneric(name="GrapleEndExperiment",
           def=function(grapleObject)
           {
             standardGeneric("GrapleEndExperiment")
           }
)

setGeneric(name="GrapleChkVersionCompatibility",
           def=function(grapleObject)
           {
             standardGeneric("GrapleChkVersionCompatibility")
           }
)

setGeneric(name="GrapleListPostProcessFilters",
           def=function(grapleObject)
           {
             standardGeneric("GrapleListPostProcessFilters")
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
            if(length(expName) > 0)
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

setMethod(f="setSecurityKey",
          signature="Graple",
          definition=function(grapleObject,path)
          {
            if(length(path) > 0)
            {
              if(!file.exists(path))
              {
                grapleObject@StatusCode <- -1
                grapleObject@StatusMsg <- "File provided does not exist"
              }
              else
              {
                ##write the logic for reading file content and set the security key
                grapleObject@SecurityKey <- ''
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- "Security Key has been successfully set"
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
            if(length(grapleObject@ExpRootDir)<=0 || !dir.exists(grapleObject@ExpRootDir)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory provided does not exist"
            }
            else if(filesPresent(grapleObject)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory should contain only directories(no files) for this experiment"
            }
            else if(!missing(filterName) && !dir.exists(paste(grapleObject@ExpRootDir, "FilterParams", sep="/"))){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment Root Directory should consist of FilterParams Directory"
            }
            else if(!missing(filterName) && dir.exists(paste(grapleObject@ExpRootDir, "FilterParams", sep="/")) && !filePresent(paste(grapleObject@ExpRootDir, "FilterParams", sep = "/"), "FilterParams.json")){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "The Filter Params directory does not contain FilterParams.json"
            }
            else
            {
              if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir)){
                grapleObject@TempDir <- tempdir()
              }
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
                filterName <- paste(sub("\\..*", "", filterName), '.R', sep="")
                qurl <- paste(grapleObject@GWSURL, "GrapleRun", filterName, sep="/")
              }
              expid = postForm(qurl, files=fileUpload(tarfile))
              grapleObject@JobID <- toString(fromJSON(expid)[1])
              if(nchar(grapleObject@JobID) == 40){
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste("The simulation was submitted successfully, JobID: ", grapleObject@JobID, sep = '')
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
            else if(getResultsDirName(grapleObject) %in% list.files(grapleObject@ResultsDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- paste("Directory with name as ExpName/JobID found in results dir,please delete and try again", sep ="")
            }
            else{
              if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir)){
                grapleObject@TempDir <- tempdir()
              }
              td<-setwd(grapleObject@ResultsDir)
              qurl<-paste(grapleObject@GWSURL, "GrapleRunResults", grapleObject@JobID, sep="/")
              status<- getURL(qurl)
              if(toString(fromJSON(status)[2]) == "success"){
                qurl <- paste(grapleObject@GWSURL, grapleObject@JobID, "Results", "output.tar.gz", sep="/")
                resultfile <- file.path(grapleObject@TempDir,  "results.tar.gz")
                download.file(qurl, resultfile)
                setwd(grapleObject@TempDir)
                resultPath <- paste(grapleObject@ResultsDir, getResultsDirName(grapleObject), sep="/")
                dir.create(resultPath)
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
            if(length(grapleObject@ExpRootDir)<=0 || !dir.exists(grapleObject@ExpRootDir)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory provided does not exist"
            }
            else if(!filePresent(grapleObject@ExpRootDir, "job_desc.json")){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "A job description file should be present with name job_desc.json in the ExpRootDir"
            }
            else if(!as.logical(validate_json(paste(grapleObject@ExpRootDir, "job_desc.json", sep="/"))[1])){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Invalid job_desc file"
            }
            else if(length(list.dirs(path = grapleObject@ExpRootDir, recursive = FALSE)) > 0 && missing(filterName)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory should not contain any directories for this experiment"
            }
            else if(length(list.dirs(path = grapleObject@ExpRootDir, recursive = FALSE)) > 1 && !missing(filterName)){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory should contain only files and FilterParams Directory"
            }
            else if(length(list.dirs(path = grapleObject@ExpRootDir, recursive = FALSE)) == 1 && !missing(filterName) && list.dirs(path = grapleObject@ExpRootDir, recursive = FALSE) != "FilterParams"){
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- "Experiment root directory should contain directory with name FilterParams"
            }
            else{
              if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir)){
                grapleObject@TempDir <- tempdir()
              }
              td<-getwd()
              setwd(grapleObject@TempDir)
              if(file.exists("sim.tar.gz")) file.remove("sweepexp.tar.gz")
              tarfile = file.path(getwd(), "sweepexp.tar.gz")
              setwd(grapleObject@ExpRootDir)
              tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")
              distribution_type <- validate_json(paste(grapleObject@ExpRootDir, "job_desc.json", sep="/"))[2]
              if(missing(filterName)){
                if(distribution_type == 'non-linear')
                  qurl <- paste(grapleObject@GWSURL, "GrapleRunMetSample", sep="/")
                else
                  qurl <- paste(grapleObject@GWSURL, "GrapleRunLinearSweep", sep="/")
              }
              else{
                filterName <- paste(sub("\\..*", "", filterName), '.R', sep="")
                if(distribution_type == 'non-linear')
                  qurl <- paste(grapleObject@GWSURL, "GrapleRunMetSample", filterName, sep="/")
                else
                  qurl <- paste(grapleObject@GWSURL, "GrapleRunLinearSweep", filterName, sep="/")
              }

              status <- postForm(qurl, files=fileUpload(tarfile))
              grapleObject@JobID <- toString(fromJSON(status)[2])
              if(nchar(grapleObject@JobID) == 40){
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste("The simulation was submitted successfully, JobID: ", grapleObject@JobID, sep = '')
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
            else if(getResultsDirName(grapleObject) %in% list.files(grapleObject@ResultsDir))
            {
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- paste("Directory with name as ExpName/JobID found in results dir,please delete and try again", sep ="")
            }
            else{
              if(length(grapleObject@TempDir)<=0 || !dir.exists(grapleObject@TempDir)){
                grapleObject@TempDir <- tempdir()
              }
              td<-getwd()
              qurl <- paste(grapleObject@GWSURL, "GrapleRunResultsMetSample", grapleObject@JobID, sep="/")
              status<- getURL(qurl)

              if(toString(fromJSON(status)[2]) == "success"){
                qurl <- paste(grapleObject@GWSURL, grapleObject@JobID, "Results", "output.tar.gz", sep="/")
                resultfile <- file.path(grapleObject@TempDir,  "results.tar.gz")
                download.file(qurl, resultfile)
                setwd(grapleObject@TempDir)
                resultPath <- paste(grapleObject@ResultsDir, getResultsDirName(grapleObject), sep="/")
                dir.create(resultPath)
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

setMethod(f="GrapleEndExperiment",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl <- paste(grapleObject@GWSURL, "GrapleEnd", grapleObject@JobID, sep="/")
            status<- getURL(qurl)
            return (fromJSON(status))
          }
)

setMethod(f="GrapleChkVersionCompatibility",
          signature="Graple",
          definition=function(grapleObject)
          {
            if(!length(grapleObject@Client_Version_ID) > 0){
              grapleObject@Client_Version_ID <- packageVersion("GRAPLEr")
            }
            qurl <- paste(grapleObject@GWSURL, "GrapleGetVersion", sep="/")
            status <- getURL(qurl)
            compatibleGRAPLEVersions <- fromJSON(status)
            if(grapleObject@Client_Version_ID %in% compatibleGRAPLEVersions){
              grapleObject@StatusCode <- 1
              grapleObject@StatusMsg <- 'GWS and GRAPLEr versions are compatible'
            }
            else{
              grapleObject@StatusCode <- -1
              grapleObject@StatusMsg <- paste('GWS and GRAPLEr versions are not compatible, compatible versions are :', compatibleGRAPLEVersions, sep="")
            }
            return(grapleObject)
          }
)

setMethod(f="GrapleListPostProcessFilters",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl <- paste(grapleObject@GWSURL, "GrapleListFilters", sep="/")
            status<- getURL(qurl)
            grapleObject@StatusCode <- 1
            grapleObject@StatusMsg <- paste('The list of post process filters available are :', toString(fromJSON(status)), sep = "")
            return(grapleObject)
          }
)
