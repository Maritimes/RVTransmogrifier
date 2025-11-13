#' @title propagateChanges
#' @description This function is used to ensure that filtering of a single RV data object cascades
#' to all of the other known RV data objects. For example, if GSSPECIES is filtered to only
#' "haddock", and \code{propagateChanges} is run, then all of the various RV tables will be filtered to
#' the point where they relate directly to haddock. All remaining GSINF records would be sets that
#' caught haddock, all GSCAT records would be limited to haddock, all GSDET records would be limited
#' to haddock, etc. Filtering is not limited to species, but any value that exists in any field in
#' any of the tables used by this package (i.e."GSINF","GSCAT","GSMISSIONS","GSXTYPE","GSSTRATUM",
#' "GSWARPOUT","GSSPECIES_NEW","GSDET").
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param keep_nullsets the default is \code{FALSE}. This is used to control whether or not the 
#' presence/absence of species changes the returned set locations. If \code{FALSE}, the only 
#' returned records will be directly linkable to the user-filtered records. If \code{TRUE}, set 
#' locations will still be returned, even if a particular species was not caught there.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are:
#' \itemize{
#' \item \code{4X} - Type 1; Spring (i.e. months 1:4); 2008+; specific strata 
#' \item \code{GEORGES} - Type 1; Spring (i.e. months 1:4); strata 5Z*
#' \item \code{SPRING} - Type 1; Spring (i.e. months 1:4); pre-2008; specific strata 
#' \item \code{4VSW}  - Type 1; Spring (i.e. months 1:4); 4VSW strata;  
#' \item \code{SUMMER} - Type 1; Summer (i.e. months 5:8); strata 440:495 - the "standard" strata
#' \item \code{SUMMER_ALL} - Type 1; Summer (i.e. months 5:8); strata 434:559+5Z*
#' \item \code{FALL} - Type 1; Fall (i.e. months 9:12)
#' }
#' @param years the default is \code{NULL}. A vector of 1 or more years can be passed to this function, and the data 
#' will be filtered to only that/those years  (via GSMISSIONS$YEAR).
#' @param months the default is \code{NULL}. A vector of 1 or more months can be passed to this function, and the data 
#' will be filtered to only that/those months (via GSINF$SDATE).
#' @param missions the default is \code{NULL}. A vector of 1 or more missions can be passed to this function, and the 
#' data will be filtered to only that/those missions (via GSMISSIONS$MISSION)
#' @param strata the default is \code{NULL}. A vector of 1 or more strata can be passed to this function, and the 
#' data will be filtered to only that/those strata (via GSSTRATUM$STRAT)
#' @param types the default is \code{NULL}. A vector of 1 or more set "types" can be passed to this function, and the 
#' data will be filtered to only that/those set types (via GSINF$TYPE)
#' @param areas the default is \code{NULL}. A vector of 1 or more areas can be passed to this function, and the 
#' data will be filtered to only that/those set types (via GSINF$AREA)
#' @param code the default is \code{NULL}. If data should be limited to a particular species, enter
#' the species code here
#' @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid,
#' enter the aphiaid here.
#' @param taxa the default is \code{NULL}. Any value found in any of "SPEC", "KINGDOM",
#' "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})
#' @param debug the default is \code{FALSE}.
#' @returns a list of filtered versions of the dataframes passed as \code{tblList}. If the
#' filtering fails, a value of -1 will be returned. For example, if data is filtered for a year
#' where data was not collected, a strata that doesn't exist, or a species that was not observed
#' would all result in a value of -1 being returned.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
propagateChanges<-function(tblList = NULL, keep_nullsets=T, 
                           survey = NULL, years = NULL, months = NULL, 
                           missions = NULL,strata = NULL, types = 1, areas = NULL,
                           code=NULL,aphiaid = NULL, taxa= NULL, debug=F){
  if (!is.null(code)|!is.null(aphiaid)|!is.null(taxa)) tblList <- filterSpecies(tblList = tblList, code = code, aphiaid = aphiaid, taxa = taxa, debug=debug)
  if (!is.null(missions)) missions <- toupper(missions)
  if (!is.null(survey)) survey <- toupper(survey)
  if (!is.null(taxa)) taxa <- toupper(taxa)
  
  user_months <- months
  user_strata <- strata
  user_years <- years

  ### do some filtering
  if (!is.null(survey) && survey %in% c("SUMMER")){
    months <- c(5,6,7,8)
  } else if(!is.null(survey) && survey %in% c("FALL")){
    months <- c(9,10,11,12)
  }else if(!is.null(survey) && survey %in% c("4VSW", "GEORGES", "SPRING")){
    months <- c(1,2,3,4)
    if (survey == "4VSW"){
      strata = c('396','397', '398', '399', '400', 
                 '401', '402', '403', '404', '405', '406', '407', 
                 '408', '409', '410', '411')
    }else if(survey %in% c("GEORGES", "SPRING")){
      years <- c(1978:lubridate::year(Sys.Date()))
      strata <-  tblList$GSSTRATUM[!(tblList$GSSTRATUM$STRAT %in% c('477','480','481','551','552','553','554','555','556','557',
                                                                    '516','517','518','519','520','521','522')),"STRAT"]
    } 
  }
  
  if (!is.null(user_months) && !is.null(survey)) {
    survey_months <- months
    if (length(intersect(user_months, survey_months)) == 0) {
      warning("User-provided months (", paste(user_months, collapse=", "),") do not overlap with survey '", survey, "' months (",paste(survey_months, collapse=", "), "). Using user-provided values.")
    }
  }
  
  if (!is.null(user_strata) && !is.null(survey)) {
    survey_strata <- strata
    if (length(intersect(user_strata, survey_strata)) == 0) {
      warning("User-provided strata (", paste(user_strata, collapse=", "),") do not overlap with survey '", survey, "' strata (",paste(survey_strata, collapse=", "), "). Using user-provided values.")
    }
  }
  
  if (!is.null(user_months)) months <- user_months
  if (!is.null(user_strata)) strata <- user_strata
  if (!is.null(user_years)) years <- user_years
  
  
  if (!is.null(years)) tblList$GSMISSIONS<- tblList$GSMISSIONS[tblList$GSMISSIONS$YEAR %in% years,]
  if (!is.null(missions)) tblList$GSMISSIONS<- tblList$GSMISSIONS[tblList$GSMISSIONS$MISSION %in% missions,]
  if (!is.null(months)) tblList$GSINF <- tblList$GSINF[lubridate::month(tblList$GSINF$SDATE) %in% months,]
  if (!is.null(types)) tblList$GSINF<- tblList$GSINF[tblList$GSINF$TYPE %in% types,]
  if (!is.null(strata)) tblList$GSSTRATUM  <- tblList$GSSTRATUM[tblList$GSSTRATUM$STRAT %in% strata,]
  if (!is.null(areas)) tblList$GSINF  <- tblList$GSINF[tblList$GSINF$AREA %in% areas,]
  
  LOOPAGAIN <- T
  while (LOOPAGAIN){
    precnt = sum(sapply(tblList, NROW))
    
    tblList$GSINF      <- merge(tblList$GSINF,        unique(tblList$GSCAT[,c("MISSION","SETNO")]), all.x=keep_nullsets )
    tblList$GSINF      <- merge(tblList$GSINF,        unique(tblList$GSMISSIONS[,"MISSION",drop=F]))
    tblList$GSINF      <- merge(tblList$GSINF,        unique(tblList$GSSTRATUM[,"STRAT",drop=F]))
    tblList$GSXTYPE    <- merge(tblList$GSXTYPE,      unique(tblList$GSINF[,"TYPE",drop=F]),by.x="XTYPE", by.y="TYPE")
    tblList$GSSTRATUM  <- merge(tblList$GSSTRATUM,    unique(tblList$GSINF[,"STRAT",drop=F]))
    tblList$GSMISSIONS <- merge(tblList$GSMISSIONS,   unique(tblList$GSINF[,"MISSION",drop=F]))
    tblList$GSWARPOUT  <- merge(tblList$GSWARPOUT,    unique(tblList$GSINF[,c("MISSION","SETNO")]))
    tblList$GSCAT      <- merge(tblList$GSCAT,        unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=keep_nullsets)
    tblList$GSCAT      <- merge(tblList$GSCAT,        unique(tblList$GSMISSIONS[,"MISSION",drop=F]), by="MISSION")
    tblList$GSDET      <- merge(tblList$GSDET,        unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=keep_nullsets)
    if(!all(c("TAXA_", "TAXARANK_") %in% names(tblList$GSCAT))){
      #this will only be used when no species filtering has been done.  As soon as species filtering 
      #is done, taxa_ and taxarank_ will exist
      tblList$GSCAT        <- merge(tblList$GSCAT,        unique(tblList$GSSPECIES_NEW[,"CODE",drop=F]), by.x="SPEC", by.y  ="CODE")
      tblList$GSDET        <- merge(tblList$GSDET,        unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")]))
      tblList$GSDET        <- merge(tblList$GSDET,        unique(tblList$GSDET[,c("MISSION","SETNO", "SPEC")]))
      tblList$GSSPECIES_NEW  <- merge(tblList$GSSPECIES_NEW,  unique(tblList$GSCAT[,"SPEC",drop=F]), by.x="CODE", by.y="SPEC")
    }else{
      tblList$GSCAT        <- merge(tblList$GSCAT,        unique(tblList$GSSPECIES_NEW[,c("TAXA_", "TAXARANK_")]))
      tblList$GSDET        <- merge(tblList$GSDET,        unique(tblList$GSCAT[,c("MISSION","SETNO", "TAXA_", "TAXARANK_")]))
      tblList$GSDET     <- merge(tblList$GSDET,     unique(tblList$GSDET[,c("MISSION","SETNO", "TAXA_", "TAXARANK_")]))
      tblList$GSSPECIES_NEW  <- merge(tblList$GSSPECIES_NEW,  unique(tblList$GSCAT[,c("TAXA_", "TAXARANK_")]))
    }
    
    postcnt = sum(sapply(tblList, NROW))
    
    if(nrow(tblList$GSCAT)==0 | nrow(tblList$GSINF)==0){
      warning("Filtered out all catches and/or sets")
      return(tblList)
    }
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
    } else{
      if (debug) print(sapply(tblList, NROW))
    }
  }
  
  return(tblList)
}
