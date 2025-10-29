#' @title propagateChanges
#' @description This function is used to ensure that filtering of a single RV data object cascades
#' to all of the other known RV data objects. For example, if GSSPECIES is filtered to only
#' "haddock", and \code{propagateChanges} is run, then all of the various RV tables will be filtered to
#' the point where they relate directly to haddock. All remaining GSINF records would be sets that
#' caught haddock, all GSCAT records would be limited to haddock, all GSDET records would be limited
#' to haddock, etc. Filtering is not limited to species, but any value that exists in any field in
#' any of the tables present in RVSurveyData package.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param keep_nullsets the default is \code{FALSE}. This is used to control whether or not the 
#' presence/absence of species changes the returned set locations. If \code{FALSE}, the only 
#' returned records will be directly linkable to the user-filtered records. If \code{TRUE}, set 
#' locations will still be returned, even if a particular species was not caught there.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a list of filtered versions of the dataframes passed as \code{tblList}. If the
#' filtering fails, a value of -1 will be returned. For example, if data is filtered for a year
#' where data was not collected, a strata that doesn't exist, or a species that was not observed
#' would all result in a value of -1 being returned.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
propagateChanges<-function(tblList = NULL, ...){
  
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  recdTables <- names(tblList)
  essentialTables <- c("GSINF","GSCAT","GSMISSIONS","GSXTYPE","GSSTRATUM","GSWARPOUT","GSSPECIES_NEW","dataDETS","dataLF")
  if (length(setdiff(essentialTables,recdTables))>0) stop("Missing the following tables from your tblList object: ",paste(setdiff(essentialTables,recdTables), collapse=", "))
  tblList      <- filterSpecies(tblList = tblList, ...)
  LOOPAGAIN <- T
  if (all(c("TAXA_", "TAXARANK_") %in% names(tblList$GSSPECIES_NEW) & !all(c("TAXA_", "TAXARANK_") %in% names(tblList$GSCAT)))){
    if(args$debug) message("\tdetected TAXA_ and TAXARANK_ fields in GSSPECIES_NEW - adding to all tables")
    # use opportunity to also filter GSCAT dataLF and datDETS by the requested taxa

    tblList$GSCAT    <- merge(tblList$GSCAT, tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_")], by.x="SPEC", by.y="CODE")
    tblList$dataDETS <- merge(tblList$dataDETS, tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_")], by.x="SPEC", by.y="CODE")
    tblList$dataLF   <- merge(tblList$dataLF, tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_")], by.x="SPEC", by.y="CODE")
    
    if(length(unique(tblList$GSSPECIES_NEW$TAXA_))==1 & length(unique(tblList$GSSPECIES_NEW$SCI_NAME))>1 & !args$taxaAgg){
      if(args$debug) message("\tAGGREGATING POSSIBLE BUT  NOT REQUESTED")
    } else if (length(unique(tblList$GSSPECIES_NEW$TAXA_))==1 & length(unique(tblList$GSSPECIES_NEW$SCI_NAME))>1 & args$taxaAgg){
      if(args$debug) message("\tAGGREGATING POSSIBLE AND REQUESTED")
      message("")
      #If there's one taxa and multiple sci_names, we need to agg - ie remove taxa fields
      tblList$GSCAT$SPEC <- tblList$dataLF$SPEC <- tblList$dataDETS$SPEC <- NULL
      tblList$GSCAT$APHIA_ID <- tblList$dataLF$APHIA_ID <- tblList$dataDETS$APHIA_ID <- NULL
      tblList$GSCAT$SCI_NAME <- tblList$dataLF$SCI_NAME <- tblList$dataDETS$SCI_NAME <- NULL
      tblList$GSCAT$TOTNO_RAW <- tblList$GSCAT$TOTWGT_RAW <- NULL
      if (args$useBins){
        args$useBins <- F
        message("\tuseBins has been set to FALSE (since taxa are being combined).  All lengths will be to 1 cm")
      }
      tblList$GSCAT <- tblList$GSCAT %>%
        dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT))) %>%
        dplyr::summarise(TOTNO=sum(TOTNO),
                         TOTWGT=sum(TOTWGT),
                         .groups = "keep")%>%
        as.data.frame()
      
      tblList$dataLF <- tblList$dataLF %>%
        dplyr::group_by(dplyr::across(c(-CLEN))) %>%
        dplyr::summarise(CLEN=sum(CLEN),
                         .groups = "keep")%>%
        as.data.frame()
    }else{
      if (args$debug)message("\tAGGREGATING UNNECESSARY")
    }
    
  }
  
  while (LOOPAGAIN){
    precnt = sum(sapply(tblList, NROW))
    tblList$GSINF      <- merge(tblList$GSINF,        unique(tblList$GSCAT[,c("MISSION","SETNO")]), all.x=args$keep_nullsets )
    tblList$GSINF      <- merge(tblList$GSINF,        unique(tblList$GSMISSIONS[,"MISSION",drop=F]))
    tblList$GSXTYPE    <- merge(tblList$GSXTYPE,      unique(tblList$GSINF[,"TYPE",drop=F]),by.x="XTYPE", by.y="TYPE")
    tblList$GSSTRATUM  <- merge(tblList$GSSTRATUM,    unique(tblList$GSINF[,"STRAT",drop=F]))
    tblList$GSMISSIONS <- merge(tblList$GSMISSIONS,   unique(tblList$GSINF[,"MISSION",drop=F]))
    tblList$GSWARPOUT  <- merge(tblList$GSWARPOUT,    unique(tblList$GSINF[,c("MISSION","SETNO")]))
    tblList$GSCAT      <- merge(tblList$GSCAT,        unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=args$keep_nullsets)
    tblList$GSCAT      <- merge(tblList$GSCAT,        unique(tblList$GSMISSIONS[,"MISSION",drop=F]), by="MISSION")
    tblList$dataDETS   <- merge(tblList$dataDETS,     unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=args$keep_nullsets)
    tblList$dataLF     <- merge(tblList$dataLF,       unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=args$keep_nullsets)
    # tblList$GSCURNT    <- merge(tblList$GSCURNT,      unique(tblList$GSINF[,"CURNT",drop=F]))
    # tblList$GSFORCE    <- merge(tblList$GSFORCE,      unique(tblList$GSINF[,"FORCE",drop=F]))
    # tblList$GSAUX      <- merge(tblList$GSAUX,        unique(tblList$GSINF[,"AUX",drop=F]))
    # tblList$GSGEAR     <- merge(tblList$GSGEAR,       unique(tblList$GSINF[,"GEAR",drop=F]))
    # tblList$GSMATURITY <- merge(tblList$GSMATURITY, unique(tblList$dataDETS[,"FMAT",drop=F]), by.x="CODE", by.y="FMAT")
    # tblList$GSSEX      <- merge(tblList$GSSEX,      unique(tblList$dataDETS[,"FSEX",drop=F]), by.x="CODE", by.y="FSEX")
    # tblList$GSHOWOBT   <- tblList$GSHOWOBT[which(tblList$GSHOWOBT$HOWOBT %in% c(unique(tblList$GSINF$HOWD),unique(tblList$GSINF$HOWS))) ,]
    # if(!args$taxaAgg){
    
    if(!all(c("TAXA_", "TAXARANK_") %in% names(tblList$GSCAT))){
      #this will only be used when no species filtering has been done.  As soon as species filtering 
      #is done, taxa_ and taxarank_ will exist
      tblList$GSCAT      <- merge(tblList$GSCAT,      unique(tblList$GSSPECIES_NEW[,"CODE",drop=F]), by.x="SPEC", by.y  ="CODE")
      tblList$dataDETS   <- merge(tblList$dataDETS,   unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")]))
      tblList$dataLF     <- merge(tblList$dataLF,     unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")]))
      tblList$dataLF     <- merge(tblList$dataLF,     unique(tblList$dataDETS[,c("MISSION","SETNO", "SPEC")]))
      tblList$dataDETS   <- merge(tblList$dataDETS,   unique(tblList$dataLF[,c("MISSION","SETNO", "SPEC")]))
      tblList$GSSPECIES_NEW  <- merge(tblList$GSSPECIES_NEW,  unique(tblList$GSCAT[,"SPEC",drop=F]), by.x="CODE", by.y="SPEC")
    }else{
      tblList$GSCAT      <- merge(tblList$GSCAT,      unique(tblList$GSSPECIES_NEW[,c("TAXA_", "TAXARANK_")]))
      tblList$dataDETS   <- merge(tblList$dataDETS,   unique(tblList$GSCAT[,c("MISSION","SETNO","TAXA_", "TAXARANK_")])) 
      tblList$dataLF     <- merge(tblList$dataLF,     unique(tblList$GSCAT[,c("MISSION","SETNO", "TAXA_", "TAXARANK_")]))
      tblList$dataLF     <- merge(tblList$dataLF,     unique(tblList$dataDETS[,c("MISSION","SETNO", "TAXA_", "TAXARANK_")]))
      tblList$dataDETS   <- merge(tblList$dataDETS,   unique(tblList$dataLF[,c("MISSION","SETNO", "TAXA_", "TAXARANK_")]))
      tblList$GSSPECIES_NEW  <- merge(tblList$GSSPECIES_NEW,  unique(tblList$GSCAT[,c("TAXA_", "TAXARANK_")]))
    }
    
    postcnt = sum(sapply(tblList, NROW))
    
    if(nrow(tblList$GSCAT)==0 | nrow(tblList$GSINF)==0){
      # message("Filtered out all catches and/or sets")
      # return(-1)
      stop("Filtered out all catches and/or sets")
    }
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
    } else{
      if (args$debug) print(sapply(tblList, NROW))
    }
  }
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}
