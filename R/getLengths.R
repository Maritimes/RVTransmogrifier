getLengths <- function(tblList=NULL, useBins =F, bySex =F, debug=F){
  nw <- stratify(tblList)$stratified_bySet[,c("MISSION", "SPEC", "SETNO", "STRAT", "DIST","SIZE_CLASS","SAMPWGT","TOTWGT", "TOTWGT_RAW", "TOTNO")]
  message("Do I need to do anything with sampwgt/totwgt ratios here prior to dropping size class?")
  dets<- tblList$GSDET[,c("MISSION", "SPEC", "SETNO","SIZE_CLASS","FLEN", "FSEX", "CLEN")]

  if(!bySex) dets <- dets  |> 
    dplyr::group_by(MISSION, SPEC, SETNO,SIZE_CLASS,FLEN) |> 
    dplyr::summarise(CLEN = sum(CLEN), .groups = "keep") |> 
    as.data.frame()

  # 
  # agelen <- merge(nw,
  #                 , by=c("MISSION", "SETNO","SPEC", "SIZE_CLASS"), all.x=T)
  # agelen$SIZE_CLASS <- NULL
  
  if (useBins){
    sppLgrp = tblList$GSSPECIES_NEW$LGRP
  }else{
    sppLgrp = 1
  }
  browser()
}