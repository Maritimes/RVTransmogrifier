#' @title easyFlatten
#' @description Merge GSINF, GSCAT, and GSSTRATUM tables into a single flat data frame for analysis. Handles both species-level (SPEC) and taxa-level (TAXA_) data.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, and GSSTRATUM.
#' @param keep_nullsets the default is \code{TRUE}. If TRUE, retains set information even when no catch occurred.
#' @return A merged data frame containing set, catch, and stratum information with NA values filled appropriately.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr filter rename
#' @export
easyFlatten <- function(tblList = NULL, keep_nullsets=T){
  theFields<-c("MISSION", "SETNO","SIZE_CLASS","CALWT", "SAMPWGT", "TOTWGT", "TOTNO")
  if ("SPEC" %in% names(tblList$GSCAT)){
    tblList$GSCAT$SPEC[is.na(tblList$GSCAT$SPEC)] <- unique(tblList$GSCAT$SPEC[!is.na(tblList$GSCAT$SPEC)])
    theFields <- c(theFields,"SPEC")
  } else if ("TAXA_" %in% names(tblList$GSCAT)){
    tblList$GSCAT$TAXA_[is.na(tblList$GSCAT$TAXA_)] <- unique(tblList$GSCAT$TAXA_[!is.na(tblList$GSCAT$TAXA_)])
    tblList$GSCAT$TAXARANK_[is.na(tblList$GSCAT$TAXARANK_)] <- unique(tblList$GSCAT$TAXARANK_[!is.na(tblList$GSCAT$TAXARANK_)])
    theFields <- c(theFields,"TAXA_")
  }
  
  this <- merge(tblList$GSINF[,c("MISSION","SETNO","STRAT","SDATE","DIST", "TYPE","GEAR","DEPTH", "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE",  "BOTTOM_SALINITY", "SLAT_DD","SLONG_DD","ELAT_DD","ELONG_DD","AREA","AREA_KM2","WINGSPREAD_FT" )], 
                tblList$GSCAT[, theFields], by=c("MISSION", "SETNO"), all.x=keep_nullsets)
  this[is.na(this$DIST), "DIST"] <- 1.75
  this[is.na(this$TOTNO), "TOTNO"] <- 0
  this[is.na(this$TOTWGT), "TOTWGT"] <- 0
  
  return(this)
}