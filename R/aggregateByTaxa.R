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
  
  
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}
# 
# args <- list(...)
# debug <- ifelse(is.null(args$debug), F, args$debug) 
# quiet <- ifelse(is.null(args$quiet), F, args$quiet)
# #All data arranged by code, so no need to aggregate if code was specified
# if(!is.null(code))return(tblList)
# GSCAT_agg  <-  tblList$GSCAT
# dataLF_agg <-  tblList$dataLF
# #remove the spp code and collapse on the taxa_ and taxarank_
# GSCAT_agg   <- merge(GSCAT_agg, tblList$GSSPECIES[,c("CODE","TAXA_", "TAXARANK_","APHIAID", "SCIENTIFICNAME")], by.x="SPEC", by.y="CODE")
# dataLF_agg  <- merge(dataLF_agg, tblList$GSSPECIES[,c("CODE","TAXA_", "TAXARANK_","APHIAID", "SCIENTIFICNAME")], by.x="SPEC", by.y="CODE")
# GSCAT_agg$SPEC <- dataLF_agg$SPEC <- NULL
# if(!is.null(aphiaid)) GSCAT_agg$TAXA_ <- GSCAT_agg$TAXARANK_ <- dataLF_agg$TAXA_ <- dataLF_agg$TAXARANK_ <- NULL
# if(!is.null(taxa)) GSCAT_agg$APHIAID <- dataLF_agg$APHIAID <- GSCAT_agg$SCIENTIFICNAME <- dataLF_agg$SCIENTIFICNAME <- NULL
# 
# GSCAT_agg <- GSCAT_agg %>%
#   dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT))) %>%
#   dplyr::summarise(TOTNO=sum(TOTNO),
#                    TOTWGT=sum(TOTWGT),
#                    .groups = "keep")%>%
#   as.data.frame()
# 
# dataLF_agg <- dataLF_agg %>%
#   dplyr::group_by(dplyr::across(c(-CLEN))) %>%
#   dplyr::summarise(CLEN=sum(CLEN),
#                    .groups = "keep")%>%
#   as.data.frame()
# 
# tblList$GSCAT_agg <- GSCAT_agg
# tblList$dataLF_agg <- dataLF_agg
# return(tblList)
# }

# aggrInfo <- data.frame(TAXA= character(), RANK=character())
# for (t in 1:length(taxa)){
#   rankCheck <- which(apply(catches_agg, 2, function(b) any(grepl(taxa[t], b))))
#   if (length(rankCheck)==0){
#     if (!quiet) message('Invalid taxa requested ("',taxa[t],'")')
#     taxa <- taxa[!taxa %in% taxa[t]]
#     next()
#   }
#   rankCheck <- stack(rankCheck)
#   rankCheck$ind <- as.character(rankCheck$ind)
#   if (nrow(rankCheck)==1){
#     aggrInfo[t,"TAXA"] <- taxa[t]
#     aggrInfo[t,"RANK"] <- rankCheck$ind
#   }else {
#     rankCheck <- rankCheck[which.max(rankCheck$values),]
#     if (nrow(rankCheck)>1){
#       stop("Can't differentiate between multiple taxomomic levels with the same number of records (",paste(rankCheck$ind, collapse=","),")")
#     }
#     if (!quiet) message('Multiple potential matches for "',taxa[t],'" - defaulting to the usage with the most records ("',rankCheck$ind,'")')
#     aggrInfo[t,"TAXA"] <- taxa[t]
#     aggrInfo[t,"RANK"] <- rankCheck$ind
#   }
# }