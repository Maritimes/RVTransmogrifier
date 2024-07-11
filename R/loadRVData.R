#' @title loadRVData
#' @description This function loads all of the tables from the RVSurveyData package into a single 
#' list.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a list of the ~18 data frames from the RVSurveyData package
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
loadRVData<- function(...){
  argsFn <- as.list(environment())
  argsFn[["RVSurveyData"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  
  # tbls <- utils::data(package = "RVSurveyData")$results[,"Item"]
  dataEnv<-new.env()
  utils::data(package="RVSurveyData", list = RVSurveyData::RVSurveyDataTables, envir = dataEnv)
  thedata <- as.list(dataEnv)
  return(invisible(thedata))
}