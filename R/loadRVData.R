#' @title loadRVData
#' @description A survey is a defined by a combination of a range of months, a selection of strata, 
#' and a tow 'type'. This function ensures that all those values are combined correctly to get the 
#' valid data for a survey.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param force.extract the default is \code{FALSE}.
#' @param ... other arguments passed to methods (i.e. 'keep_nullsets', debug' and 'quiet')
#' @returns a list of dataframes which have been filtered to only include data related to the 
#' specified survey and years
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr left_join select mutate
#' @importFrom stats setNames
#' @export
loadRVData <- function(cxn=NULL, force.extract = FALSE, ...){

  args <- list(...)
  newE <- new.env()
  if (is.null(args$debug)) args$debug <- FALSE
  if (is.null(args$keep_nullsets)) args$keep_nullsets <- TRUE
  
  missingTables <- coreTables[!file.exists(file.path(get_pesd_rvt_dir(), paste0("GROUNDFISH.", coreTables, ".RData")))]
  
  if (length(missingTables) == 0 & force.extract ==F){
    # no missing tables, load everything
    invisible(Mar.utils::get_data_tables(schema = "GROUNDFISH", tables = coreTables, force.extract = force.extract,
                                         cxn = cxn, data.dir = get_pesd_rvt_dir(), extract_user= args$extract_user, extract_computer = args$extract_computer,env = newE))
  }else{
    if (force.extract){
      # replace everything, load everything
      message("Initial data extraction/processing - this will take a minute (subsequent loads will be WAY faster)")
      invisible(Mar.utils::get_data_tables(schema = "GROUNDFISH", tables = rawTables, force.extract = force.extract,
                                           cxn = cxn, data.dir = get_pesd_rvt_dir(), extract_user= args$extract_user, extract_computer = args$extract_computer,env = newE))
    }else{
      # identify the stuff we have that can just be loaded
      message("Extracting and processing missing tables - this will take a minute (subsequent loads will be WAY faster)")
      allProcessTables <- c("GSINF") #GSSPECIES_NEW
      
      missingProcessTables <- intersect(missingTables, allProcessTables)
      haveTables <- setdiff(coreTables, missingProcessTables)
      
      # load the tables we have
      invisible(Mar.utils::get_data_tables(schema = "GROUNDFISH", tables = haveTables, force.extract = force.extract,
                                           cxn = cxn, data.dir = get_pesd_rvt_dir(), extract_user= args$extract_user, extract_computer = args$extract_computer,env = newE))
    }
    if("GSINF" %in% missingTables) invisible(Mar.utils::get_data_tables(schema = "GROUNDFISH", tables = c("GSINF","GSSTRATA"), force.extract = force.extract,
                                                                               cxn = cxn, data.dir = get_pesd_rvt_dir(), extract_user= args$extract_user, extract_computer = args$extract_computer,env = newE))
    # if ("GSSPECIES_NEW" %in% missingTables)  invisible(Mar.utils::get_data_tables(schema = "GROUNDFISH", tables = c("GSSPECIES_CHANGES","GSSPECIES_ANDES","GSSPECIES", "GSSPEC"), force.extract = force.extract,
    #                                                                                      cxn = cxn, data.dir = get_pesd_rvt_dir(), extract_user= args$extract_user, extract_computer = args$extract_computer,env = newE))
    #general processing for specific tables
    if("GSINF" %in% missingTables | force.extract) {
      newE$GSINF <- addDDCoords(newE$GSINF)
      newE$GSINF$SLAT <- newE$GSINF$SLONG <- newE$GSINF$ELAT <- newE$GSINF$ELONG  <- NULL
      gearDets <- data.frame(
        GEAR = c(9, 3, 15),
        WINGSPREAD_FT = c(41, 35, 41)
      )
      newE$GSINF <- newE$GSINF |> 
        left_join(gearDets, by="GEAR") |> 
        left_join(newE$GSSTRATUM |> 
                    select(STRAT, AREA_KM2 = AREA)|> 
                    mutate(AREA_KM2 = sqNMToSqKm(AREA_KM2)),
                  by="STRAT")
    }

  lapply(names(newE), function(x) {
    Mar.utils::save_encrypted(
      list = x,
      file = file.path(get_pesd_rvt_dir(), paste0("GROUNDFISH.", x, ".RData")),
      envir = newE
    )
  })
  }

curDate <- as.Date(Sys.time())
dataDate <- as.Date(max(newE$GSINF$SDATE, na.rm = T))
if (as.integer(difftime(curDate, dataDate))>270){
  message("Your most recent data is >9 months old - there's probably new data available.\nConsider updating your data by adding `force.extract=T`. " )
}

df_names <- ls(envir = newE)[sapply(ls(envir = newE), function(x) is.data.frame(get(x, envir = newE, inherits = FALSE)))]
tblList <- stats::setNames(lapply(df_names, get, envir = newE, inherits = FALSE), df_names)

#only need to propagate changes if we make changes...
if (!all(sapply(list(args$survey, args$years, args$months,
                     args$missions, args$strata, args$types, args$areas,
                     args$code, args$aphiaid, args$taxa), is.null))){
  tblList <- propagateChanges(tblList = tblList, keep_nullsets=args$keep_nullsets,
                              survey = args$survey, years = args$years, months = args$months,  
                              missions = args$missions, strata = args$strata, types=args$types, areas= args$areas,
                              code = args$code, aphiaid = args$aphiaid, taxa = args$taxa, debug = args$debug)
}
return (tblList)
}