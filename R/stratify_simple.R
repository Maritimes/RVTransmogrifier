
stratify_simple <- function(theData=NULL, towDist_NM = 1.75, debug=F){
  #this guy just takes a dataframe and is standalone (vs `stratify()`, which takes a list and relies on util functions)
  if (!"AREA" %in% names(theData)) stop(sprintf("Column AREA not found in data frame (must be NM2)"))
  if (!"STRAT" %in% names(theData))     stop(sprintf("Column STRAT not found in data frame"))
  if (!"DIST" %in% names(theData))     stop(sprintf("Column DIST not found in data frame"))
  if (!"GEAR" %in% names(theData)) stop(sprintf("Column GEAR not found in data frame"))
  if (!"SPEC" %in% names(theData)) stop(sprintf("Column SPEC not found in data frame"))
  if (!"TOTNO" %in% names(theData)) stop(sprintf("Column TOTNO not found in data frame"))
  if (!"TOTWGT" %in% names(theData)) stop(sprintf("Column TOTWGT not found in data frame"))
  
  sqNMToSqKm <- function(field=NULL){
    field <- round(field*3.4299,4)
    return(field)
  }
  
  st_err <- function (x = NULL, na.rm = FALSE) {
    if (na.rm) 
      x <- stats::na.omit(x)
    if (length(x) < 2) 
      return(NA)
    stats::sd(x)/sqrt(length(x))
  }
  
  valPerSqKm <- function(theData = NULL, towDist_NM = 1.75, netWidth_ft = 41){
    #theData must include NAs (e.g. nullsets so that the means will include them)
    theData[is.na(theData)] <- 0
    ft2m = 0.3048
    m2km = 1/1000
    nmi2mi = 1.1507794
    mi2ft = 5280
    sakm2 = (netWidth_ft * ft2m * m2km ) * ( towDist_NM * nmi2mi * mi2ft * ft2m * m2km )
    res = theData/sakm2
    res = round(res,4)
    return(res)
  }
  
  
  gearDets <- '"GEAR","GEAR_DESC","WINGSPREAD_M","WINGSPREAD_FT"
  9,"W2A",12.5,41
  3,"Y36",10.7,35
  15,"US4",12.6,41'
  gearDets <- utils::read.table(text = gearDets, header = TRUE, sep = ",")

  theData <- merge(theData,gearDets[,c("GEAR", "WINGSPREAD_FT")], all.x = T, by = "GEAR")
  theData$SPEC[is.na(theData$SPEC)] <- unique(theData$SPEC[!is.na(theData$SPEC)])
  theData[is.na(theData$DIST), "DIST"] <- 1.75
  theData[is.na(theData$TOTNO), "TOTNO"] <- 0
  theData[is.na(theData$TOTWGT), "TOTWGT"] <- 0
  
  theData$AREA_KM <- sqNMToSqKm(field = theData$AREA)
  theData$TOTWGT_sqkm <- valPerSqKm(theData$TOTWGT, towDist_NM = theData$DIST, netWidth_ft = theData$WINGSPREAD_FT)
  theData$BIOMASS_set<-theData$TOTWGT_sqkm*theData$AREA_KM
  theData$TOTNO_sqkm  <- valPerSqKm(theData$TOTNO, towDist_NM = theData$DIST, netWidth_ft = theData$WINGSPREAD_FT)
  theData$ABUNDANCE_set<-theData$TOTNO_sqkm*theData$AREA_KM
  
  species_col <- if ("SPEC" %in% names(theData)) "SPEC" else if ("TAXA_" %in% names(theData)) "TAXA_" else NULL
  
  theData <- theData |>
    # dplyr::group_by(MISSION, STRAT, AREA_KM) |>
    dplyr::group_by(SPEC, STRAT, AREA_KM) |>
    dplyr::summarise(COUNT= length(MISSION),
                     TOTWGT_sum = round(sum(TOTWGT),5),
                     TOTNO_sum = round(sum(TOTNO),5),
                     TOTWGT_mean = round(mean(TOTWGT),5),
                     TOTNO_mean = round(mean(TOTNO),5),
                     TOTWGT_SE = round(st_err(TOTWGT),5),
                     TOTNO_SE = round(st_err(TOTNO),5),
                     TOTWGT_sqkm_strat_mean = round(mean(TOTWGT_sqkm),5),
                     TOTNO_sqkm_strat_mean = round(mean(TOTNO_sqkm),5),
                     BIOMASS_SE= round(st_err(BIOMASS_set),5),
                     ABUNDANCE_SE= round(st_err(ABUNDANCE_set),5),
                     .groups = "keep") |>
    dplyr::mutate(BIOMASS = round(TOTWGT_sqkm_strat_mean * AREA_KM,5),
                  ABUNDANCE = round(TOTNO_sqkm_strat_mean * AREA_KM,5)) |>
    as.data.frame()
  
  return(theData)
}




