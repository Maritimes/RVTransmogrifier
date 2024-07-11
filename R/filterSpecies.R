### @title filterSpecies
## @description This function facilitates filtering by species, and can filter by "code", "aphiaid", 
## or "taxa". Only one of these filter types may be used at a time.  "code" is first priority, and 
## if multiple filter types are sent, only "code" will be applied.  If "code" is not sent, "aphiaid"
## will take priority over "taxa".
## @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
## should have filtering applied to them.
## @param code the default is \code{NULL}. If data should be limited to a particular species, enter 
## the species code here
## @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid, 
## enter the aphiaid here.
## @param taxa the default is \code{NULL}. Any value found in any of "SPEC", "KINGDOM", 
## "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})
## @param keep_nullsets the default is \code{TRUE}.
## @returns a list of filtered versions of the dataframes passed as \code{tblList}. If the
## filtering fails, a value of -1 will be returned. For example, if data is filtered for a year
## where data was not collected, a strata that doesn't exist, or a species that was not observed
## would all result in a value of -1 being returned.
## @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
## @export
## @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
filterSpecies <- function(tblList = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  #if nothing is to be filtered, skip.
  # if (all(is.null(args$code), is.null(args$aphiaid), is.null(args$taxa))) return(tblList)
  if (!is.null(args$aphiaid)) {
    if (!is.null(args$taxa)){
      message("Only one filter type  (i.e. code, taxa, aphiaid) can be done at once. Only 'aphiaid' will be used.")
    }
    req_Spp <- tblList$GSSPECIES_NEW[which(tblList$GSSPECIES_NEW$APHIA_ID %in% args$aphiaid),]
    req_Spp[,"TAXA_"]<- req_Spp[, "SPEC"]
    req_Spp[,"TAXARANK_"]<- req_Spp[, "RANK"]
    if (length(args$aphiaid[!(args$aphiaid %in% tblList$GSSPECIES_NEW$APHIA_ID)])>0){
      message("No species with the following aphiaids could be found: ",paste0(args$aphiaid[!(args$aphiaid %in% tblList$GSSPECIES_NEW$APHIA_ID)], collapse = ", "))
    }
  } else if (!is.null(args$taxa))    {
    #taxa search is more complicated - must check all columns for matching strings, so I do them one at a time, and build a resultset
    req_Spp <- tblList$GSSPECIES_NEW[FALSE,]
    req_Spp$TAXA_ <- character()
    req_Spp$TAXARANK_ <- character()
    req_Spp_Code <- req_Spp[FALSE,] 
    req_Spp_AphiaID <- req_Spp[FALSE,]
    req_Spp_Taxa <- req_Spp[FALSE,]
    #Taxa filter must capture all spec within the taxa, as well as the rank of the taxa (e.g. "FAMILY")
    for (t in 1:length(args$taxa)){
      these <- tblList$GSSPECIES_NEW[which(apply(tblList$GSSPECIES_NEW[,c("SPEC","KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS")], 1, function(r) any(r %in% args$taxa[t]))), ]
      if (nrow(these)==0){
        message("No species with a taxa of ",args$taxa[t]," were found. Taxa are names, like 'GADUS'.  \nCheck the RVSurveyData::GSSPECIES_NEW for an exhaustive selection of available taxa names for this data set")
        next
      }
      these$TAXA_ <- args$taxa[t]
      rankCheck <- which(apply(these[,!names(these) %in% c("COMMENTS","TAXA_")], 2, function(b) any(grepl(args$taxa[t], b))))
      rankCheck <- utils::stack(rankCheck)
      rankCheck$ind <- as.character(rankCheck$ind)
      if (nrow(rankCheck)==1){
        these$TAXARANK_ <- rankCheck$ind
      }else {
        rankCheck <- rankCheck[which.max(rankCheck$values),]
        if (nrow(rankCheck)>1){
          stop("Can't differentiate between multiple taxomomic levels with the same number of records (",paste(rankCheck$ind, collapse=","),")")
        }
        #message('Multiple potential matches for "',taxa[t],'" - defaulting to the usage with the most records ("',rankCheck$ind,'")')
        these$TAXARANK_  <- rankCheck$ind
      }
      req_Spp_Taxa <- rbind.data.frame(req_Spp_Taxa, these)
    }
    req_Spp<-req_Spp_Taxa
  }else if (!is.null(args$code)){
    req_Spp <- tblList$GSSPECIES_NEW[which(tblList$GSSPECIES_NEW$CODE %in% args$code),]
    req_Spp[,"TAXA_"]<- req_Spp[, "SPEC"]
    req_Spp[,"TAXARANK_"]<- req_Spp[, "RANK"]
    if (length(args$code[!(args$code %in% tblList$GSSPECIES_NEW$CODE)])>0){
      message("No species with the following codes could be found: ",paste0(args$code[!(args$code %in% tblList$GSSPECIES_NEW$CODE)], collapse = ", "))
    }
  }else{
    #if no code/aphia/taxa sent, still need to add taxa and taxarank to the data for other functions
    req_Spp <- tblList$GSSPECIES_NEW
    req_Spp[,"TAXA_"]<- req_Spp[, "SPEC"]
    req_Spp[,"TAXARANK_"]<- req_Spp[, "RANK"]
  }
  if (nrow(req_Spp) > 0 & nrow(req_Spp) < nrow(tblList$GSSPECIES_NEW)){
    tblList$GSSPECIES_NEW <- req_Spp
    if(args$debug)message("\tlimited species table")
    # tblList <- propagateChanges(tblList, ...)
  } else if (nrow(req_Spp)==0){
    message("Species filter resulted in zero species remaining - cancelling")
    return(-1)
  } else if (nrow(req_Spp) == nrow(tblList$GSSPECIES_NEW)){
    #   message("Species filter would not remove any species - cancelling")
    tblList$GSSPECIES_NEW <- req_Spp
    return(tblList)
  }
  if (!is.null(args$taxa) && nrow(req_Spp_Taxa)>0){
    if(args$taxaAgg) tblList <- aggregateByTaxa(tblList=tblList, ...)
  }
  # tblList <- propagateChanges(tblList, ...)
  if (inherits(tblList,"numeric")){
    stop("Filter resulted in 0 records.")
  }
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}