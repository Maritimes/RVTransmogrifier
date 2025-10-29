
stratify <- function(tblList=NULL, towDist_NM = 1.75, debug=F){

  gearDets <- '"GEAR","GEAR_DESC","WINGSPREAD_M","WINGSPREAD_FT"
  9,"W2A",12.5,41
  3,"Y36",10.7,35
  15,"US4",12.6,41'
  gearDets <- utils::read.table(text = gearDets, header = TRUE, sep = ",")
  #message("Maybe area should be a parameter so we can limit the data?")
  theData <- merge(tblList$GSINF[,c("MISSION", "SETNO", "STRAT", "DIST","GEAR")],tblList$GSCAT, all.x = T, by = c("MISSION", "SETNO"))
  if ("SPEC" %in% names(theData)){
    # message("What's going on?!!")
    message(unique(tblList$GSMISSIONS$YEAR))
    theDataMMMs<<- theData
    theData$SPEC[is.na(theData$SPEC)] <- unique(theData$SPEC[!is.na(theData$SPEC)])
  } else if ("TAXA_" %in% names(theData)){
    theData$TAXA_[is.na(theData$TAXA_)] <- unique(theData$TAXA_[!is.na(theData$TAXA_)])
    theData$TAXARANK_[is.na(theData$TAXARANK_)] <- unique(theData$TAXARANK_[!is.na(theData$TAXARANK_)])
  }

  theData <- merge(theData,gearDets[,c("GEAR", "WINGSPREAD_FT")], all.x = T, by = "GEAR")
  theData <- merge(theData,tblList$GSSTRATUM[,c("STRAT", "AREA")], all.x = T, by = "STRAT")
  colnames(theData)[colnames(theData)=="AREA"] <- "AREA_NM"
  theData[is.na(theData$DIST), "DIST"] <- 1.75
  theData[is.na(theData$TOTNO), "TOTNO"] <- 0
  theData[is.na(theData$TOTWGT), "TOTWGT"] <- 0
  
  theData$AREA_KM <- sqNMToSqKm(field = theData$AREA_NM)
  theData$TOTWGT_sqkm <- valPerSqKm(theData$TOTWGT, towDist_NM = theData$DIST, netWidth_ft = theData$WINGSPREAD_FT)
  theData$BIOMASS_set<-theData$TOTWGT_sqkm*theData$AREA_KM
  theData$TOTNO_sqkm  <- valPerSqKm(theData$TOTNO, towDist_NM = theData$DIST, netWidth_ft = theData$WINGSPREAD_FT)
  theData$ABUNDANCE_set<-theData$TOTNO_sqkm*theData$AREA_KM

  species_col <- if ("SPEC" %in% names(theData)) "SPEC" else if ("TAXA_" %in% names(theData)) "TAXA_" else NULL
  
  theData <- theData |>
    # dplyr::group_by(MISSION, STRAT, AREA_KM) |>
    dplyr::group_by(.data[[species_col]], STRAT, AREA_KM) |>
    dplyr::summarise(COUNT= length(MISSION),
              TOTWGT_sum = round(sum(TOTWGT),5),
              TOTNO_sum = round(sum(TOTNO),5),
              TOTWGT_mean = round(mean(TOTWGT),5),
              TOTNO_mean = round(mean(TOTNO),5),
              TOTWGT_SE = round(Mar.utils::st_err(TOTWGT),5),
              TOTNO_SE = round(Mar.utils::st_err(TOTNO),5),
              TOTWGT_sqkm_strat_mean = round(mean(TOTWGT_sqkm),5),
              TOTNO_sqkm_strat_mean = round(mean(TOTNO_sqkm),5),
              BIOMASS_SE= round(Mar.utils::st_err(BIOMASS_set),5),
              ABUNDANCE_SE= round(Mar.utils::st_err(ABUNDANCE_set),5),
              .groups = "keep") |>
    dplyr::mutate(BIOMASS = round(TOTWGT_sqkm_strat_mean * AREA_KM,5),
           ABUNDANCE = round(TOTNO_sqkm_strat_mean * AREA_KM,5)) |>
    as.data.frame()
  
  return(theData)
}




