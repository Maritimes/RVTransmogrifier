### @title aggregateByTaxa
## @description This function .
## @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
## should have filtering applied to them.
## @param code the default is \code{NULL}. If data should be limited to a particular species, enter 
## the species code here.
## @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid, 
## enter the aphiaid here.
## @param taxa the default is \code{NULL}. Any value found in any of "SCI_NAME", "KINGDOM", 
## "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})

## @param ... other arguments passed to methods (e.g. 'debug' and 'quiet')
## @returns #' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
## @export
aggregateByTaxa <- function(tblList = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  # #All data arranged by code, so no need to aggregate if code was specified
  if(!is.null(args$code))return(tblList)
  GSCAT_agg  <-  tblList$GSCAT
  dataLF_agg <-  tblList$dataLF
  #remove the spp code and collapse on the taxa_ and taxarank_
  GSCAT_agg   <- merge(GSCAT_agg, tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_","APHIA_ID", "SPEC")], by.x="SPEC", by.y="CODE")
  dataLF_agg  <- merge(dataLF_agg, tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_","APHIA_ID", "SPEC")], by.x="SPEC", by.y="CODE")
  GSCAT_agg$SPEC <- dataLF_agg$SPEC <- NULL
  if(!is.null(args$aphiaid)) GSCAT_agg$TAXA_ <- GSCAT_agg$TAXARANK_ <- dataLF_agg$TAXA_ <- dataLF_agg$TAXARANK_ <- NULL
  if(!is.null(args$taxa)) GSCAT_agg$APHIA_ID <- dataLF_agg$APHIA_ID <- GSCAT_agg$SPEC.y <- dataLF_agg$SPEC.y <- NULL

  GSCAT_agg <- GSCAT_agg %>%
    dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT))) %>%
    dplyr::summarise(TOTNO=sum(TOTNO),
                     TOTWGT=sum(TOTWGT),
                     .groups = "keep")%>%
    as.data.frame()

  dataLF_agg <- dataLF_agg %>%
    dplyr::group_by(dplyr::across(c(-CLEN))) %>%
    dplyr::summarise(CLEN=sum(CLEN),
                     .groups = "keep")%>%
    as.data.frame()

  tblList$GSCAT_agg <- GSCAT_agg
  tblList$dataLF_agg <- dataLF_agg
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}
