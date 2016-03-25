
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("GRAPLEr has been developed with support from a supplement the the PRAGMA award (NSF OCI-1234983). For more information, please visit graple.org")
}

#' @title Get the GRAPLEr Service Status
#' @description
#' This function allows you to check the staus of the GRAPLEr web service.
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @return a string describing service status
#' @keywords Graple ServiceStatus
#' @export
#' @examples
#' \dontrun{
#' GrapleCheckService()
#' }
GrapleCheckService<-function(submissionURL)
{
  qurl<-paste(submissionURL, "service_status", sep="/")
  status<- getURL(qurl)
  return(fromJSON(status))
}

#' @title Sends the experiment (multiple simulations) to be run on GWS
#' This function allows you to run graple with an optional post-process filtering of results
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param ExperimentDir the experiment root folder
#' @param FilterName the name of the post-processing filter script
#' @keywords Graple RunExperiment
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' expRootDir<-"./Workspace/ExpRoot"
#' filterName<-"Filter1.R"
#' GrapleRunExperiment(graplerURL, expRootDir, filterName)
#' }
GrapleRunExperiment<-function(submissionURL, ExperimentDir, FilterName)
{
  td<-getwd()
  setwd(ExperimentDir)
  simdirs <- dir(".")
  if("Results" %in% simdirs) {
      print("Results found in experiment directory!")
      print("Please delete and run the experiment")
      return()
  }
  tarfile = file.path(ExperimentDir, "sim.tar.gz")
  tar(tarfile, simdirs, compression="gz", compression_level = 6, tar="internal")

  if(missing(FilterName)){
    qurl <- paste(submissionURL, "GrapleRun", sep="/")
  }
  else{
    qurl <- paste(submissionURL, "GrapleRun", FilterName, sep="/")
  }

  expid = postForm(qurl, files=fileUpload(tarfile))

  if (file.exists(tarfile)) file.remove(tarfile)
  setwd(td)
  return (substr(expid[1], start=13, stop=52))
}

#' @title Check the GRAPLEr Experiment Status
#' @description
#' This function allows you to check the staus of the GRAPLEr web service.
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a string describing experiment status
#' @keywords Graple CheckExperimentCompletion
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleCheckExperimentCompletion(graplerURL, expId)
#' }
GrapleCheckExperimentCompletion <- function(submissionURL, experimentId)
{
  qurl <- paste(submissionURL, "GrapleRunStatus", experimentId, sep="/")
  status<- getURL(qurl)
  return (fromJSON(status))
}

#' @title Gets the GRAPLEr Experiment Results
#' @description
#' This function allows you to retrieve the complete results
#' of an experiment.
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a string describing the fully qualified result file name
#' @keywords Graple GetExperimentResults
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleGetExperimentResults(graplerURL, expId)
#' }
GrapleGetExperimentResults <- function(submissionURL, experimentId)
{
  td<-getwd()
  if("Results" %in% dir()) {
      print("Results found in experiment directory!")
      print("Please delete and get the results")
      return()
  }
  qurl <- paste(submissionURL, "GrapleRunResults", experimentId, sep="/")
  status<- getURL(qurl)

  qurl <- paste(submissionURL, experimentId, "Results", "output.tar.gz", sep="/")
  resultfile <- file.path(getwd(),  "results.tar.gz")
  download.file(qurl, resultfile)
  dir.create("Results")
  file.copy("results.tar.gz", "Results/")
  file.remove("results.tar.gz")
  setwd("Results")
  untar("results.tar.gz")
  file.remove("results.tar.gz")
  files <- list.files(".")
  lapply(files, function(x){untar(x); file.remove(x)})
  setwd(td)
  return(resultfile)
}

#' @title Creates an experiment based on sweep parameters
#' @description
#' This function allows you to run an experiment using a single simulation.
#' The sweep parameters are used to generate the other sims.
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param simDir the simulation folder containing the driver file
#' @param driverFileName the driver file name
#' @param parameterName the column name from the driver file to be swept
#' @param startValue numeric start offset
#' @param endValue numeric end offset
#' @param numberOfIncrements number of increments between start and end
#' @param filterName the name of the post-process filter (optional parameter)
#' @return the experiment ID
#' @keywords Graple RunExperimentSweep
#' @export
#' @examples
#' \dontrun{
#' simDir="./Workspace/ExpRoot/Exp0"
#' driverFileName="met_hourly.csv"
#' parameterName="AirTemp"
#' startValue=-2
#' endValue=2
#' numberOfIncrements=10
#' filterName="Filter1.R" (optional parameter)
#' expId<-GrapleRunExperimentSweep(graplerURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
#' }
GrapleRunExperimentSweep <- function(submissionURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements, filterName)
{
  td<-getwd()
  unlink("../tempGRAPLE", recursive = TRUE)
  dir.create("../tempGRAPLE")
  setwd("../tempGRAPLE")
  tarfile = file.path(simDir, "sim.tar.gz")
  setwd(simDir)
  if("Results" %in% dir()) {
	  print("Results found in experiment directory!")
	  print("Please delete and run the experiment")
	  unlink("../tempGRAPLE", recursive = TRUE)
	  return()
  }
  tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")

  qurl <- paste(submissionURL, "GrapleRunMetOffset", sep="/")

  status <- postForm(qurl, files=fileUpload(tarfile))
  print(status)
  expid <- substr(status[1], start=13, stop=52)

  if (file.exists(tarfile)) file.remove(tarfile)
  unlink("../tempGRAPLE", recursive = TRUE)
  if(missing(filterName)){
    params <- paste(expid, driverFileName, parameterName, startValue, endValue, numberOfIncrements, sep="*")
  }else{
    params <- paste(expid, driverFileName, parameterName, startValue, endValue, numberOfIncrements, filterName, sep="*")
  }
  qurl <- paste(submissionURL, "TriggerSimulation", params, sep="/")
  print(qurl)
  status = postForm(qurl, t="none")
  print(paste0("Status:", status))
  #if(status <> "Success") print("Failed to start experiment")
  setwd(td)
  return (expid)
}

#' @title Creates an experiment based on a configuration file
#' @description
#' This function allows you to run an experiment using a single simulation.
#' Create an archive that contains:
#' 1. Met (csv) file.
#' 2. Driver (nml) file
#' 3. job_desc.csv file (the name has to be job_desc.csv)
#' 4. (optional) A FilterParams Directory, which consists of a "FilterParams.txt" file
#'
#' The sweep parameters in the job description file are used to generate the other sims.
#' job_desc.csv is a comma separated value file in the following format:
#' base_file, driver_filename.csv
#' iterations, number_of_iterations
#' Parametername1, add, uniform, -1, 1
#' Paramtername2, sub, binomial, 10, 0.5

#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param simDir the simulation folder containing the driver file
#' @param FilterName the name of post-process filter
#' @return the experiment ID
#' @keywords Graple RunExperimentJob
#' @export
#' @examples
#' \dontrun{
#' simDir="./Workspace/ExpRoot/Exp0"
#' FilterName="Filter1.R"
#' expId<-GrapleRunExperimentJob(graplerURL, simDir, FilterName)
#' }
GrapleRunExperimentJob <- function(submissionURL, simDir, FilterName)
{
  td<-getwd()
  unlink("../tempGRAPLE", recursive = TRUE)
  dir.create("../tempGRAPLE")
  setwd("../tempGRAPLE")
  tarfile = file.path(getwd(), "sweepexp.tar.gz")
  setwd(simDir)
  if("Results" %in% dir()) {
      print("Results found in experiment directory!")
      print("Please delete and run the experiment")
      unlink("../tempGRAPLE", recursive = TRUE)
      return()
  }
  tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")
  if(missing(FilterName)){
    qurl <- paste(submissionURL, "GrapleRunMetSample", sep="/")
  }
  else{
    qurl <- paste(submissionURL, "GrapleRunMetSample", FilterName, sep="/")
  }

  status <- postForm(qurl, files=fileUpload(tarfile))
  if (file.exists(tarfile)) file.remove(tarfile)
  unlink("../tempGRAPLE", recursive = TRUE)
  expid <- substr(status[1], start=57, stop=96)
  setwd(td)
  return (expid)
}

#' @title Gets the GRAPLEr Experiment Job Results
#' @description
#' This function allows you to retrieve the complete results
#' of an sweep job style experiment.
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a string describing the fully qualified result file name
#' @keywords Graple GetExperimentJobResults
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleGetExperimentJobResults(graplerURL, expId)
#' }
GrapleGetExperimentJobResults <- function(submissionURL, experimentId)
{
  td<-getwd()
  if("Results" %in% dir()) {
    print("Results found in experiment directory!")
    print("Please delete and run the experiment")
    return()
  }
  qurl <- paste(submissionURL, "GrapleRunResultsMetSample", experimentId, sep="/")
  status<- getURL(qurl)

  qurl <- paste(submissionURL, experimentId, "Results", "output.tar.gz", sep="/")
  resultfile <- file.path(getwd(),  "results.tar.gz")
  download.file(qurl, resultfile)
  dir.create("Results")
  file.copy("results.tar.gz", "Results/")
  file.remove("results.tar.gz")
  setwd("Results")
  untar("results.tar.gz")
  file.remove("results.tar.gz")
  files <- list.files(".", pattern = "\\.bz2\\.tar$")
  lapply(files, function(x){untar(x); file.remove(x)})
  setwd(td)
  return(resultfile)
}

#' @title Aborts an existing GRAPLE experiment
#' @description
#' This function allows you to terminate a previously submitted experiment using
#' its experiment identifier. This is useful for aborting stalled or failed experiments.
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a status string
#' @keywords Graple AbortExperiment
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleAbortExperiment(graplerURL, expId)
#' }
GrapleAbortExperiment <- function(submissionURL, experimentId)
{
  qurl <- paste(submissionURL, "GrapleAbort", experimentId, sep="/")
  status<- getURL(qurl)
  return (fromJSON(status))
}

#' @title Retrieves the list of post process operation scripts
#' @description
#' This function allows you to retrieve list of all the post-process operation scripts
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @return a comma seperated string of file names
#' @keywords Graple ListFilters
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' GrapleListFilters(graplerURL)
#' }
GrapleListFilters <- function(submissionURL)
{
  qurl <- paste(submissionURL, "GrapleListFilters", sep="/")
  status <- getURL(qurl)
  return (toString(fromJSON(status)))
}

#' @title Checks version compatibility between R package and Graple web service
#' @description
#' This function allows you to check version compatibility between R package and
#' Graple Web Service
#' @param submissionURL URL:Port of the GRAPLEr web service
#' @return true if versions are compatible else false
#' @keywords Graple CheckVersionCompatibility
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://graple-service.cloudapp.net"
#' GrapleCheckVersionCompatibility(graplerURL)
#' }
GrapleCheckVersionCompatibility <- function(submissionURL)
{
  compatibleVersions <- c(packageVersion("GRAPLEr"), "1.0.2")
  qurl <- paste(submissionURL, "GrapleGetVersion", sep="/")
  status <- getURL(qurl)
  serviceVersion <- fromJSON(status)
  if(serviceVersion %in% compatibleVersions)
    return(TRUE)
  else
    return(TRUE)
}

