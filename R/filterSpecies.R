#' @title filterSpecies
#' @description This function facilitates filtering by species, and can filter by "code", "aphiaid",
#' or "taxa". Only one of these filter types may be used at a time.  "code" is first priority, and
#' if multiple filter types are sent, only "code" will be applied.  If "code" is not sent, "aphiaid"
#' will take priority over "taxa".
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
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
#' @export
filterSpecies <- function(tblList = NULL, code = NULL, aphiaid = NULL, taxa = NULL, debug = FALSE){

  if (!is.null(aphiaid)) {
    if (!is.null(taxa)){
      message("Only one filter type  (i.e. code, taxa, aphiaid) can be done at once. Only 'aphiaid' will be used.")
    }
    req_Spp <- tblList$GSSPECIES_NEW[which(tblList$GSSPECIES_NEW$APHIA_ID %in% aphiaid),]
    req_Spp$TAXA_<- req_Spp$SPEC
    req_Spp$TAXARANK_<- req_Spp$RANK
    if (length(aphiaid[!(aphiaid %in% tblList$GSSPECIES_NEW$APHIA_ID)])>0){
      message("No species with the following aphiaids could be found: ",paste0(aphiaid[!(aphiaid %in% tblList$GSSPECIES_NEW$APHIA_ID)], collapse = ", "))
    }
  } else if (!is.null(taxa))    {
    #taxa search is more complicated - must check all columns for matching strings, so I do them one at a time, and build a resultset
    req_Spp <- tblList$GSSPECIES_NEW[FALSE,]
    req_Spp$TAXA_ <- character()
    req_Spp$TAXARANK_ <- character()
    req_Spp_Code <- req_Spp[FALSE,] 
    req_Spp_AphiaID <- req_Spp[FALSE,]
    req_Spp_Taxa <- req_Spp[FALSE,]
    #Taxa filter must capture all spec within the taxa, as well as the rank of the taxa (e.g. "FAMILY")
    for (t in 1:length(taxa)){
      these <- tblList$GSSPECIES_NEW[which(apply(tblList$GSSPECIES_NEW[,c("SPEC","KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS")], 1, function(r) any(r %in% taxa[t]))), ]
      if (nrow(these)==0){
        message("No species with a taxa of ",taxa[t]," were found. Taxa are names, like 'GADUS'.  \nCheck the GSSPECIES_NEW for an exhaustive selection of available taxa names for this data set")
        next
      }
      these$TAXA_ <- taxa[t]
      rankCheck <- which(apply(these[,!names(these) %in% c("COMMENTS","TAXA_")], 2, function(b) any(grepl(taxa[t], b))))
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
  }else if (!is.null(code)){
    req_Spp <- tblList$GSSPECIES_NEW[which(tblList$GSSPECIES_NEW$CODE %in% code),]
    req_Spp$TAXA_<- req_Spp$SPEC
    req_Spp$TAXARANK_<- req_Spp$RANK
    if (length(code[!(code %in% tblList$GSSPECIES_NEW$CODE)])>0){
      message("No species with the following codes could be found: ",paste0(code[!(code %in% tblList$GSSPECIES_NEW$CODE)], collapse = ", "))
    }

  }else{
    #if no code/aphia/taxa sent, still need to add taxa and taxarank to the data for other functions
    req_Spp <- tblList$GSSPECIES_NEW
    req_Spp$TAXA_<- req_Spp$SPEC
    req_Spp$TAXARANK_<- req_Spp$RANK
  }
  
  if (nrow(req_Spp) > 0){
    if (nrow(req_Spp) < nrow(tblList$GSSPECIES_NEW) && debug) message("\tLimited species table")
    if (nrow(req_Spp) == nrow(tblList$GSSPECIES_NEW) && debug)message("\tSpecies filter did not remove any species")
  }else{
    warning("Species filter resulted in zero species remaining - cancelling")
    return(tblList)
  }
  tblList$GSSPECIES_NEW <- req_Spp
  if(!all(c("TAXA_", "TAXARANK_") %in% names(tblList$GSCAT))){
    tblList$GSCAT    <- merge(tblList$GSCAT, distinct(tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_")]), by.x="SPEC", by.y="CODE")
    tblList$GSDET      <- merge(tblList$GSDET, distinct(tblList$GSSPECIES_NEW[,c("CODE","TAXA_", "TAXARANK_")]), by.x="SPEC", by.y="CODE")
  }
  return(tblList)
}