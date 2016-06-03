
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("GRAPLEr has been developed with support from a supplement the PRAGMA award (NSF OCI-1234983) by
  Ken Subratie, Saumitra Aditya, Satish Mahesula, Renato J. Figueiredo, Cayelan C. Carey and Paul C. Hanson.
  For more information, please visit graple.org")
}

#' Validates whether a given url exists
#'
#' @param url
#' @return A boolean value indicating whether url exists
#' @examples
#' validate_url('http://graple.acis.ufl.edu')
validate_url <- function(url){
  invalid_url = tryCatch({
    http_error(url)
  }, error = function(e) {
    invalid_url <- TRUE
  })
  return(!invalid_url)
}

#' Checks if the grapleObject is valid
#' by checking if the directories provided exist and valid
#' @param object A Graple Object
#' @return errors if any of the validation fails
#' @examples
#' check_graple(grapleExp1)
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

#' Checks if the input/results directory is a sub-directory of
#' results/input directory
#' @param object A Graple Object
#' @return A boolean value indicating whether the directory
#' structure is valid
#' @examples
#' check_subdirectory(grapleExp1)
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

#' Gets the name for results directory
#' @param object A Graple Object
#' @return ExpName if provided, else the JobID
#' @examples
#' getResultsDirName(grapleExp1)
getResultsDirName <- function(object){
  if(length(object@ExpName) > 0)
    return(object@ExpName)
  else if(length(object@JobID) > 0)
    return(object@JobID)
  else
    return('')
}

#' Checks if there are any files present in ExpRoot  Directory
#' @param object A Graple Object
#' @return A boolean value indicating whether files are present or not
#' @examples
#' filesPresent(grapleExp1)
filesPresent <- function(object){
  if(length(list.files(path = object@ExpRootDir, recursive = FALSE)) != length(list.dirs(path = object@ExpRootDir, recursive = FALSE)))
    return(TRUE)
  else
    return(FALSE)
}

#' checks if a given file exists in a directory
#' @param dirPath A Directory path
#' @param fileName File name
#' @return A boolean value indicating whether a file is present or not
#' @examples
#' filePresent('C:/ExpRoot/Exp1/mySim', 'met_hourly.csv')
filePresent <- function(dirPath, fileName){
  filesList <- list.files(path = dirPath, recursive = FALSE)
  if(fileName %in% filesList)
    return(TRUE)
  else
    return(FALSE)
}

#' Checks if a json file is valid according to GRAPLEr constraints
#' @param jsonFilePath JSON File path
#' @return A boolean value indicating whether the json file is valid
#' @examples
#' validate_json('C:/ExpRoot/Exp3/job_desc.json')
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

  if(is.null(jsonFile$ExpFiles))
    return(list(valid_JSON, distribution_type))

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

#' An S4 class to represent a graple object.
#'
#' @slot GWSURL            A SubmissionURL for the experiment having a default value of http://graple.acis.ufl.edu
#' @slot ExpRootDir        Experiment Root Directory path
#' @slot ResultsDir        Directory path for storing the results
#' @slot JobID             Unique identifier for the experiment
#' @slot StatusCode        Integer value indicating the status of an operation
#' @slot StatusMsg         A brief text message indicating the status of an operation
#' @slot ExpName           A name for the experiment
#' @slot TempDir           Temporary Directory path for temporary storage of files
#' @slot SecurityKey       API Security Key to authenticate a user
#' @slot Retention         A user provided request to the GRAPLEr cluster on duration for which experiment results should be retained
#' @slot Client_Version_ID The version of GRAPLEr package being used
Graple <- setClass("Graple", slots = c(GWSURL = "character", ExpRootDir="character", ResultsDir="character", JobID="character", Email="character", APIKey="character",
                                       StatusCode="numeric", StatusMsg="character", ExpName="character", TempDir="character", SecurityKey="character",
                                       Retention ="numeric", Client_Version_ID="character"), prototype = list(GWSURL="http://graple.acis.ufl.edu", Email='', APIKey="0",
                                       TempDir=tempdir(), Retention = 10, Client_Version_ID = toString(packageVersion("GRAPLEr"))), validity = check_graple)

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
           def=function(grapleObject, filterName, simsPerJob)
           {
             standardGeneric("GrapleRunSweepExperiment")
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

#' Set the Temporary Directory to given directory path for the Graple Object
#' @param grapleObject A Graple Object
#' @param path Path to a directory to be set as Temporary directory for the experiment
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' setTempDir(grapleExp1, 'C:/TempDir')
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

#' Sets the submission URL in the Graple object
#' @param grapleObject A Graple Object
#' @param url A Submission URL
#' @return The Graple object with updated submissionURL
#' @examples
#' setSubmissionURL(grapleExp1, 'http://graple.acis.ufl.edu')
setMethod(f="setSubmissionURL",
          signature="Graple",
          definition=function(grapleObject,url)
          {
            grapleObject@GWSURL <- url
            return(grapleObject)
          }
)

#' Sets the name of the experiment in the Graple object
#' @param grapleObject A Graple Object
#' @param expName A Graple Object
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' setExpName(grapleObject, 'SweepExperiment')
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

#' Set the Experiment root directory in the Graple object
#' @param grapleObject A Graple Object
#' @param path Directory path for the experiment root directory
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' setExperimentDir(grapleObject, 'C:/ExpRoot/Exp1')
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

#' Sets the results directory for the experiment in the Graple object
#' @param grapleObject A Graple Object
#' @param path Directory path for the results to be downloaded
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' setResultsDir(grapleObject, 'C:/ExpRoot/Results/Exp1')
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

#' Sets the security key in the grapleObject
#' @param grapleObject A Graple Object
#' @param path Path to the a text file containing the security key
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' setSecurityKey(grapleExp1, 'C:/ExpRoot/KeyFiles/myKey.txt')
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

#' Checks if the graple service is up and running on the submission URL provided
#' @param grapleObject A Graple Object
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' GrapleCheckService(grapleExp1)
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

#' Used to run a batch experiment with mutiple simulations in the root directory
#' also, an optional filter can be run on the results generated
#' @param grapleObject A Graple Object
#' @param filterName An optional post-process filter name
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' GrapleRunExperiment(grapleExp1, 'ExtractVariables')
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

              params = list()
              params['retention'] = grapleObject@Retention
              params['expname'] = getResultsDirName(grapleObject)
              params['email'] = grapleObject@Email
              params['apikey'] = grapleObject@APIKey
              if(!missing(filterName))
                params['filter'] = filterName
              qurl <- paste(grapleObject@GWSURL, "GrapleRun", sep="/")
              postresp = postForm(qurl, .params = params, files=fileUpload(tarfile))
              response = fromJSON(postresp)
              
              grapleObject@JobID <- ''
              grapleObject@StatusCode <- -1
              if(nchar(response$errors) > 0) {
                grapleObject@StatusMsg <- response$errors
              } else if(nchar(response$uid) == 40) {
                grapleObject@JobID <- toString(response$uid)
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste("The simulation was submitted successfully, JobID: ", grapleObject@JobID, sep = '')
              } else {
                grapleObject@StatusMsg <- "Unknown error"
              }
              if(nchar(response$warnings) > 0) 
                  grapleObject@StatusMsg <- paste(grapleObject@StatusMsg, "\nWARNING:", response$warnings)

              if (file.exists(tarfile)) file.remove(tarfile)
              setwd(td)

            }
            return (grapleObject)
          }
)

#' Checks the status of an experiment
#' @param object A Graple Object
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' GrapleCheckExperimentCompletion(grapleExp1)
setMethod(f="GrapleCheckExperimentCompletion",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl <- paste(grapleObject@GWSURL, "GrapleRunStatus", grapleObject@JobID, sep="/")
            grapleObject@StatusCode <- -1
            status <- fromJSON(getForm(qurl, apikey=grapleObject@APIKey))
            
            if(nchar(status$errors) > 0)
              grapleObject@StatusMsg <- toString(status$errors)
            else {
              grapleObject@StatusCode <- 1
              grapleObject@StatusMsg <- toString(status$curr_status)
            }
            return (grapleObject)
          }
)

#' Downloads the results to the results directory path with exp name/job id as the directory name
#' @param grapleObject A Graple Object
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' GrapleGetExperimentResults(grapleExp1)
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
              td<-getwd()
              qurl<-paste(grapleObject@GWSURL, "GrapleRunResults", grapleObject@JobID, sep="/")
              grapleObject@StatusCode <- -1
              getresp <- getForm(qurl, apikey=grapleObject@APIKey)
              status <- fromJSON(getresp)
              if(nchar(status$errors) > 0)
                  grapleObject@StatusMsg <- status$errors
              else if(status$status == "success"){
                qurl <- paste(grapleObject@GWSURL, status$output_url, sep="")
                resultfile <- file.path(grapleObject@TempDir, "results.tar.gz")
                download.file(qurl, resultfile)
                setwd(grapleObject@TempDir)
                resultPath <- paste(grapleObject@ResultsDir, getResultsDirName(grapleObject), sep="/")
                dir.create(resultPath)
                file.copy("results.tar.gz", resultPath)
                file.remove("results.tar.gz")
                setwd(resultPath)
                untar("results.tar.gz")
                file.remove("results.tar.gz")
                setwd(td)
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste('The results have been downloaded to ', resultPath, sep ="")
              }
            }
            return(grapleObject)
          }
)

#' Used to run a sweep experiment with a json file providing job description
#' also, an optional filter can be run on the results generated
#' @param grapleObject A Graple Object
#' @param filterName An optional post-process filter name
#' @param simsPerJob Optionally set the number of simulations per worker job
#' @return The status message is updated on Graple object and the Graple object is returned
#' @examples
#' GrapleRunSweepExperiment(grapleExp1)
#' GrapleRunSweepExperiment(grapleExp1, 'ExtractVariables')
#' GrapleRunSweepExperiment(grapleExp1, 'ExtractVariables', 20)
setMethod(f="GrapleRunSweepExperiment",
          signature="Graple",
          definition=function(grapleObject, filterName, simsPerJob)
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
            else if(length(list.dirs(path = grapleObject@ExpRootDir, recursive = FALSE)) == 1 && !missing(filterName) && list.dirs(path = grapleObject@ExpRootDir, recursive = FALSE, full.names = FALSE)[1] != "FilterParams"){
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
              if(distribution_type == 'non-linear')
                qurl <- paste(grapleObject@GWSURL, "GrapleRunMetSample", sep="/")
              else
                qurl <- paste(grapleObject@GWSURL, "GrapleRunLinearSweep", sep="/")
              params = list()
              params['retention'] = grapleObject@Retention
              params['expname'] = getResultsDirName(grapleObject)
              params['email'] = grapleObject@Email
              params['apikey'] = grapleObject@APIKey
              if(!missing(filterName))
                params['filter'] = filterName
              if(!missing(simsPerJob))
                params['gen_per_job'] = simsPerJob

              grapleObject@JobID <- ''
              grapleObject@StatusCode <- -1
              subresp <- postForm(qurl, .params = params, files=fileUpload(tarfile))
              response <- fromJSON(subresp)

              if(nchar(response$errors) > 0) {
                grapleObject@StatusMsg <- response$errors
              } else if(nchar(response$uid) == 40) {
                grapleObject@JobID <- toString(response$uid)
                grapleObject@StatusCode <- 1
                grapleObject@StatusMsg <- paste("The simulation was submitted successfully, JobID: ", grapleObject@JobID, sep = '')
              } else {
                grapleObject@StatusMsg <- "Unknown error"
              }
              if(nchar(response$warnings) > 0) 
                  grapleObject@StatusMsg <- paste(grapleObject@StatusMsg, "\nWARNING:", response$warnings)

              if (file.exists(tarfile)) file.remove(tarfile)
              setwd(td)
            }
            return (grapleObject)
          }
)

#' A method to abort an experiment
#' @param grapleObject A Graple Object
#' @return A text message with the status of the operation
#' @examples
#' GrapleEndExperiment(grapleExp1)
setMethod(f="GrapleEndExperiment",
          signature="Graple",
          definition=function(grapleObject)
          {
            qurl <- paste(grapleObject@GWSURL, "GrapleEnd", grapleObject@JobID, sep="/")
            status<- getForm(qurl, apikey=grapleObject@APIKey)
            return (fromJSON(status))
          }
)

#' Checks if the GRAPLEr pacakge being used is compatible with GWS Service being used
#' @param grapleObject A Graple Object
#' @return A status msg indicating whether Web service and GRAPLEr are compatible
#' and the Graple object is returned
#' @examples
#' GrapleChkVersionCompatibility(grapleExp1)
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

#' Provides a list of post processing operations/ filters available
#' @param grapleObject A Graple Object
#' @return Adds the list of filters to StatusMsg and returns the Graple object
#' @examples
#' GrapleListPostProcessFilters(grapleExp1)
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
