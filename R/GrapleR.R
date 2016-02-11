
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science.")
}

#' @title Get the Graple Service Status
#' @description
#' This function allows you to check the staus of the GrapeR service.
#' @param submissionURL URL:Port of the GrapeR service
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
  return(status)
}

#' @title Sends the experiment (multiple simulations) to be run on GrapleR
#' This function allows you to run graple with an optional post-process filtering of results
#' @param submissionURL URL:Port of the GrapleR service
#' @param ExperimentDir the experiment root folder
#' @param FilterName the name of the post-processing filter script
#' @keywords Graple RunExperiment
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://128.227.150.20:80"
#' expRootDir<-"./Workspace/SimRoot"
#' filterName<-"Filter1.R"
#' GrapleRunExperiment(graplerURL, expRootDir, filterName)
#' }
GrapleRunExperiment<-function(submissionURL, ExperimentDir, FilterName)
{
  td<-getwd()
  setwd(ExperimentDir)
  simdirs <- dir(".")
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

#' @title Check the Graple Experiment Status
#' @description
#' This function allows you to check the staus of the GrapleR service.
#' @param submissionURL URL:Port of the GrapleR service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a string describing experiment status
#' @keywords Graple CheckExperimentCompletion
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://128.227.150.20:80"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleCheckExperimentCompletion(graplerURL, expId)
#' }
GrapleCheckExperimentCompletion <- function(submissionURL, experimentId)
{
  qurl <- paste(submissionURL, "GrapleRunStatus", experimentId, sep="/")
  status<- getURL(qurl)
  return (status)
}

#' @title Gets the Graple Experiment Results
#' @description
#' This function allows you to retrieve the complete results
#' of an experiment.
#' @param submissionURL URL:Port of the GrapleR service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a string describing the fully qualified result file name
#' @keywords Graple GetExperimentResults
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://128.227.150.20:80"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleGetExperimentResults(graplerURL, expId)
#' }
GrapleGetExperimentResults <- function(submissionURL, experimentId)
{
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
  return(resultfile)
}

#' @title Creates an experiment based on sweep parameters
#' @description
#' This function allows you to run an experiment using a single simulation.
#' The sweep parameters are used to generate the other sims.
#' @param submissionURL URL:Port of the GrapeR service
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
#' simDir="C:/Workspace/SimRoot/Sim0"
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
#' 1. data csv file.
#' 2. .nml file
#' 3. job_desc.csv file (the name has to be job_desc.csv)
#' 4. (optional) A FilterParams Directory, which consists of a "FilterParams.txt" file
#'
#' The sweep parameters in the job description file are used to generate the other sims.
#' job_desc.csv is a comma separated value file in the following format:
#' base_file, driver_filename.csv
#' iterations, number_of_iterations
#' Parametername1, add, uniform, -1, 1
#' Paramtername2, sub, binomial, 10, 0.5

#' @param submissionURL URL:Port of the GrapeR service
#' @param simDir the simulation folder containing the driver file
#' @param FilterName the name of post-process filter
#' @return the experiment ID
#' @keywords Graple RunExperimentJob
#' @export
#' @examples
#' \dontrun{
#' simDir="C:/Workspace/SimRoot"
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
  print(status)
  expid <- substr(status[1], start=56, stop=95)
  setwd(td)
  return (expid)
}

#' @title Gets the Graple Experiment Job Results
#' @description
#' This function allows you to retrieve the complete results
#' of an sweep job style experiment.
#' @param submissionURL URL:Port of the GrapeR service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @return a string describing the fully qualified result file name
#' @keywords Graple GetExperimentJobResults
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://128.227.150.20:80"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' GrapleGetExperimentJobResults(graplerURL, expId)
#' }
GrapleGetExperimentJobResults <- function(submissionURL, experimentId)
{
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
  return(resultfile)
}
