
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
#' This function allows you to run graple.
#' @param submissionURL URL:Port of the GrapeR service
#' @param ExperimentDir the experiment root folder
#' @keywords Graple RunExperiment
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://128.227.150.20:80"
#' expRootDir<-"./Workspace/SimRoot"
#' GrapleRunExperiment(graplerURL, expRootDir)
#' }
GrapleRunExperiment<-function(submissionURL, ExperimentDir)
{
  td<-getwd()
  setwd(ExperimentDir)
  simdirs <- dir(".")
  tarfile = file.path(ExperimentDir, "sim.tar.gz")
  tar(tarfile, simdirs, compression="gz", compression_level = 6, tar="internal")

  qurl <- paste(submissionURL, "GrapleRun", sep="/")
  expid = postForm(qurl, files=fileUpload(tarfile))

  if (file.exists(tarfile)) file.remove(tarfile)
  setwd(td)
  return (substr(expid[1], start=13, stop=52))


}

#' @title Check the Graple Experiment Status
#' @description
#' This function allows you to check the staus of the GrapeR service.
#' @param submissionURL URL:Port of the GrapeR service
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
#' @param submissionURL URL:Port of the GrapeR service
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
  untar("results.tar.gz")
  return(resultfile)

}

#' @title Gets a Graple Simulation Results
#' @description
#' This function allows you to retrieve the results of a
#' specific simulation in an experiment.
#' @param submissionURL URL:Port of the GrapeR service
#' @param experimentId Experiment ID returned from GrapleRunExperiment
#' or GrapleRunExperimentSweep
#' @param simId a valid simulation number from the sweep range
#' @return a string describing the fully qualified result file name
#' @keywords Graple GetSimResult
#' @export
#' @examples
#' \dontrun{
#' graplerURL<-"http://128.227.150.20:80"
#' expId<-"7YWMJYAYAR7Y3TNTAKC5801KMN7JHQW8NYBDMKUR"
#' simId<-2
#' GrapleGetSimResult(graplerURL, expId, simNum)
#' }
GrapleGetSimResult <- function(submissionURL, experimentId, simId)
{
  params<-paste(experimentId, simId, "output.nc", sep="*")
  qurl <- paste(submissionURL, "download_file", params, sep="/")
  outpURL<- getURL(qurl)

  outpURL<-substr(outpURL, 21, nchar(outpURL)-3)

  fileURL<-paste(submissionURL, outpURL, sep="/" )

  resultfile <- file.path(getwd(),  paste0(simId,"output.nc"))

  download.file(fileURL, resultfile)

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
#' expId<-GrapleRunExperimentSweep(graplerURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
#' }
GrapleRunExperimentSweep <- function(submissionURL, simDir, driverFileName, parameterName, startValue, endValue, numberOfIncrements)
{
  td<-getwd()
  setwd(simDir)

  tarfile = file.path(simDir, "sim.tar.gz")
  tar(tarfile, ".", compression="gz", compression_level = 6, tar="internal")

  qurl <- paste(submissionURL, "GrapleRunMetOffset", sep="/")

  status <- postForm(qurl, files=fileUpload(tarfile))
  print(status)
  expid <- substr(status[1], start=13, stop=52)

  if (file.exists(tarfile)) file.remove(tarfile)
  params <- paste(expid, driverFileName, parameterName, startValue, endValue, numberOfIncrements, sep="*")
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
#'
#' The sweep parameters in the job description file are used to generate the other sims.
#' job_desc.csv is a comma separated value file in the following format:
#' base_file, driver_filename.csv
#' iterations, number_of_iterations
#' Parametername1, add, uniform, -1, 1
#' Paramtername2, sub, binomial, 10, 0.5

#' @param submissionURL URL:Port of the GrapeR service
#' @param simDir the simulation folder containing the driver file
#' @param JobFileName the archive file name containing the simulations and sweep parameters
#' @return the experiment ID
#' @keywords Graple RunExperimentJob
#' @export
#' @examples
#' \dontrun{
#' simDir="C:/Workspace/SimRoot"
#' JobFileName="SweepExperiment.tar.gz"
#' expId<-GrapleRunExperimentJob(graplerURL, simDir, JobFileName)
#' }
GrapleRunExperimentJob <- function(submissionURL, simDir, JobFileName)
{
  td<-getwd()
  setwd(simDir)
  qurl <- paste(submissionURL, "GrapleRunMetSample", sep="/")
  status <- postForm(qurl, files=fileUpload(JobFileName))
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
  untar("results.tar.gz")
  return(resultfile)

}
