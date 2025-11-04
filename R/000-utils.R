utils::globalVariables(c('TOTNO', 'TOTWGT', 'CLEN', 'MISSION', 'VESEL', 'CRUNO', 'YEAR', 'SEASON', 'SDATE', 'SURFACE_TEMPERATURE', 
                         'BOTTOM_TEMPERATURE', 'BOTTOM_SALINITY', 'SETNO', 'TIME', 'STRAT', 'SLAT', 'SLONG', 'ELAT', 'ELONG', 'DUR', 
                         'DIST', 'SPEED', 'DEPTH_M', 'SURF_TEMP', 'BOTT_TEMP', 'BOTT_SAL', 'GEARDESC', 'SPEC', 'TOTWGT', 'TOTNO', 
                         'FSEX', 'FLEN', 'CLEN', 'FWT', 'MATURITY', 'SEX', 'AGE', 'SPECIMEN_ID', 'CODE', 'SCI_NAME', 'COMM', 'APHIA_ID', 
                         'SDATE', 'MISSION', 'SETNO', 'DMIN', 'DMAX', 'DEPTH', 'START_DEPTH', 'END_DEPTH', 'TIME', 'REMARKS', 'SPEC', 
                         'TOTNO', 'TOTWGT', 'TOTWGT_RAW', 'DIST', 'TOTNO_RAW', 'theCatch', 'FSHNO', 'SPECIMEN_ID', 'id', 'geometry', 
                         'ELAT_DD', 'ELONG_DD', 'SLONG_DD', 'SLAT_DD', 'theSet', 'footprintSRS', 'footprintWKT', 'type', 'eventID', 
                         'parentEventID', 'eventRemarks', 'locationRemarks', 'sampleSizeValue', 'sampleSizeUnit', 'coordinateUncertaintyInMeters', 
                         'decimalLatitude', 'decimalLongitude', 'eventDate', 'DMIN_M', 'DMAX_M', 'GEAR', 'STRAT', 'HOWD', 'AREA_M2', 'GEARDESC', 
                         'WSPREAD', 'gearType', 'BOTTOM_SALINITY','BOTTOM_TEMPERATURE','DEPTH_M','DUR','SPEED','SURFACE_TEMPERATURE','FSEX','FWT',
                         'FLEN','FMAT','AGE','measurement','measurementValue','detID','occurrenceID','individualCount','organismQuantity','CALWT',
                         'SAMPWGT','TAXA_','TAXARANK_','SLONG_DD','SLAT_DD','NAFO','StrataID','STRAT','AREA_KM','MISSION','TOTWGT','TOTNO',
                         'TOTWGT_sqkm','TOTNO_sqkm','BIOMASS_set','ABUNDANCE_set','TOTWGT_sqkm_strat_mean','TOTNO_sqkm_strat_mean'))

rawTables  <- c("GSCAT", "GSDET", "GSGEAR", "GSINF", "GSMATURITY", "GSMISSIONS", "GSSEX", "GSSPEC", "GSSPECIES", "GSSPECIES_ANDES", "GSSPECIES_CHANGES", "GSSTRATUM", "GSVESSEL","GSWARPOUT", "GSXTYPE")
coreTables <- c("GSCAT", "GSDET", "GSGEAR", "GSINF", "GSMATURITY", "GSMISSIONS", "GSSEX", "GSSPECIES_NEW", "GSSTRATUM", "GSVESSEL", "GSWARPOUT", "GSXTYPE") 
get_pesd_rvt_dir <- function() {
  dir_path <- file.path("C:", "DFO-MPO", "PESDData","RVTransmogrifier")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  return(dir_path)
}

easyFlatten <- function(tblList = NULL, keep_nullsets=T){

  theFields<-c("MISSION", "SETNO","CALWT", "SAMPWGT", "TOTWGT", "TOTNO")
  if ("SPEC" %in% names(tblList$GSCAT)){
    tblList$GSCAT$SPEC[is.na(tblList$GSCAT$SPEC)] <- unique(tblList$GSCAT$SPEC[!is.na(tblList$GSCAT$SPEC)])
    theFields <- c(theFields,"SPEC")
  } else if ("TAXA_" %in% names(tblList$GSCAT)){
    tblList$GSCAT$TAXA_[is.na(tblList$GSCAT$TAXA_)] <- unique(tblList$GSCAT$TAXA_[!is.na(tblList$GSCAT$TAXA_)])
    tblList$GSCAT$TAXARANK_[is.na(tblList$GSCAT$TAXARANK_)] <- unique(tblList$GSCAT$TAXARANK_[!is.na(tblList$GSCAT$TAXARANK_)])
    theFields <- c(theFields,"TAXA_")
  }
  
  this <- merge(tblList$GSINF[,c("MISSION","SETNO","STRAT","SDATE","DIST", "TYPE","GEAR","DEPTH", "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE",  "BOTTOM_SALINITY", "SLAT_DD","SLONG_DD","ELAT_DD","ELONG_DD" )], 
                tblList$GSCAT[, theFields], by=c("MISSION", "SETNO"), all.x=keep_nullsets)
  this <- merge(this, tblList$GSSTRATUM[, c("STRAT", "AREA")], by=c("STRAT"), all.x=T)
  return(this)
}

correctForTowDist <- function(df, col, towDist=1.75, distCol = "DIST"){
  if (!distCol %in% names(df)) stop(sprintf("Column '%s' not found in data frame", distCol))
  if (!col %in% names(df))     stop(sprintf("Column '%s' not found in data frame", col))
  if (!is.numeric(df[[col]]))  stop(sprintf("Column '%s' must be numeric", col))
  rawCol <- paste0(col,"_RAW")
  df[[rawCol]] <- df[[col]]
  df[[distCol]][is.na(df[[distCol]])] <- towDist
  df[[col]] <- round(df[[rawCol]] * (towDist / df[[distCol]]), 7)
  return(df)
}

#' @title fathomsToMeters
#' @description Converts depth measurements from fathoms to meters
#' @param field the default is \code{NULL}. A numeric value or vector in fathoms to be converted to meters
#' @return A numeric value or vector representing depth in meters, rounded to 2 decimal places
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
fathomsToMeters <- function(field=NULL){
  field <- round(field*1.8288,2)
  return(field)
}

#' @title sqNMToSqKm
#' @description Converts area measurements from square nautical miles to square kilometers
#' @param field the default is \code{NULL}. A numeric value or vector in square nautical miles to be converted to square kilometers
#' @return A numeric value or vector representing area in square kilometers, rounded to 4 decimal places
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
sqNMToSqKm <- function(field=NULL){
  field <- round(field*3.4299,4)
  return(field)
}

#' @title binSizes
#' @description Assigns values to size bins by calculating the midpoint of the bin containing the value
#' @param bin the bin width/interval size
#' @param value the numeric value to be binned
#' @return The midpoint of the bin containing the input value
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
binSizes <- function(bin, value){
  (floor(value/bin)*bin) + ((bin*.5)-0.5)  
}

#' @title makeGSSPECIES_NEW
#' @description The makeGSSPECIES_NEW function consolidates species data from 
#' multiple sources into a unified, updated dataset. It resolves obsolete 
#' species codes using redirect mappings, ensures all codes are accounted for, 
#' and merges data from GSSPECIES_ANDES_, GSSPEC_, and GSSPECIES_. The function 
#' cleans the dataset by removing fully NA rows, updates species names (SPEC) 
#' and common names (COMM) only when both are missing, and renames columns for 
#' clarity. The result is a standardized dataset with current species 
#' information, ready for analysis.
#' @param GSSPECIES_ANDES_ the default is \code{NULL}. Original GSSPECIES_ANDES object
#' @param GSSPECIES_ the default is \code{NULL}. Original GSSPECIES object
#' @param GSSPEC_ the default is \code{NULL}. Original GSSPEC object 
#' @param GSSPECIES_CHANGES_ the default is \code{NULL}. Object containing species code redirects and changes
#' @return A consolidated and standardized species dataframe with resolved redirects and updated taxonomic information
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
makeGSSPECIES_NEW <- function(GSSPECIES_ANDES_=NULL, GSSPECIES_ = NULL, GSSPEC_=NULL, GSSPECIES_CHANGES_=NULL){
  # Step 1: Resolve redirects in GSSPECIES_CHANGES_
  resolve_redirects <- function(df) {
    redirects <- df[!is.na(df$REDIRECT_CODE), c("CODE", "REDIRECT_CODE")]
    
    all_codes <- unique(c(df$CODE, df$REDIRECT_CODE))
    mapping <- data.frame(CODE_OG = all_codes, CODE = all_codes, stringsAsFactors = FALSE)
    
    changed <- TRUE
    while (changed) {
      changed <- FALSE
      merged <- merge(mapping, redirects, by.x = "CODE", by.y = "CODE", all.x = TRUE, all.y = FALSE)
      merged$CODE <- ifelse(!is.na(merged$REDIRECT_CODE), merged$REDIRECT_CODE, merged$CODE)
      new_mapping <- merged[, c("CODE_OG", "CODE")]
      
      if (!all(new_mapping$CODE[stats::complete.cases(new_mapping$CODE, mapping$CODE)] == 
               mapping$CODE[stats::complete.cases(new_mapping$CODE, mapping$CODE)], na.rm = TRUE)) {
        mapping <- new_mapping
        changed <- TRUE
      }
    }
    
    mapping
  }
  mapping <- resolve_redirects(GSSPECIES_CHANGES_)

  # Step 2: Ensure all codes from GSSPECIES_ are in mapping
  all_gspecies_codes <- unique(GSSPECIES_$CODE)
  missing_codes <- setdiff(all_gspecies_codes, mapping$CODE_OG)
  if (length(missing_codes) > 0) {
    missing_mapping <- data.frame(CODE_OG = missing_codes, CODE = missing_codes, stringsAsFactors = FALSE)
    mapping <- rbind(mapping, missing_mapping)
  }
  
  final_df <- merge(mapping, GSSPECIES_ANDES_, by.x = "CODE", by.y = "CODE",  all.x = TRUE)
  final_df <- merge(final_df, GSSPEC_[, c("SPEC","LGRP","LFSEXED")], by.x = "CODE", by.y = "SPEC",  all = TRUE)
  final_df <- final_df[!rowSums(is.na(final_df)) == ncol(final_df), ]
  
  # Populate COMM and SPEC with updated values from GSSPECIES_
  gspecies_map <- GSSPECIES_[, c("CODE", "SPEC", "COMM")]
  names(gspecies_map)[1] <- "CODE_OG"
  # "SPEC","KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS"
  merged_data <- merge(final_df, gspecies_map, by = "CODE_OG", all.x = TRUE, suffixes = c("", ".y"))
  
  # Update SPEC and COMM only if both original values are NA
  both_na <- is.na(merged_data$SPEC) & is.na(merged_data$COMM)
  merged_data$SPEC[both_na] <- merged_data$SPEC.y[both_na]
  merged_data$COMM[both_na] <- merged_data$COMM.y[both_na]
  
  # Remove redundant columns
  merged_data <- merged_data[, !names(merged_data) %in% c("SPEC.y", "COMM.y")]
  return(merged_data)
}

#' @title fixHerringLengths
#' @description Converts historical herring length measurements from centimeters to millimeters for 
#' consistency across time series. Pre-2016 missions (excluding NED2016116 and NED2016016) measured 
#' herring in cm, while later missions use mm.
#' @param GSDET_ the default is \code{NULL}. Original GSDET object containing individual fish measurements
#' @return The GSDET dataframe with herring (SPEC=60) fork lengths standardized to millimeters
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
fixHerringLengths <-function(GSDET_ = NULL){
  #Fix herring lengths - ensure all are in mm
  #NED 2016016 - first instance of measuring herring in mm - convert all prior data from cm to mm
  GSDET_[GSDET_$SPEC == 60 &
           substr(GSDET_$MISSION,4,7) <= 2016 & 
           !GSDET_$MISSION %in% c("NED2016116","NED2016016") & 
           !is.na(GSDET_$FLEN),"FLEN"] <- GSDET_[GSDET_$SPEC == 60 &
                                                   substr(GSDET_$MISSION,4,7) <= 2016 & 
                                                   !GSDET_$MISSION %in% c("NED2016116","NED2016016") & 
                                                   !is.na(GSDET_$FLEN),"FLEN"]*10
  return(GSDET_)
}


#' @title rmSizeClasses
#' @description Aggregates catch data by removing size class distinctions, summing calculated weight, 
#' sample weight, total weight, and total number across all size classes for each mission, set, and species combination.
#' @param GSCAT_ the default is \code{NULL}. Original GSCAT object containing catch data by size class
#' @return A dataframe with catch data aggregated to the mission/set/species level with size classes removed
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
rmSizeClasses <-function(GSCAT_= NULL){
  GSCAT_[is.na(GSCAT_$TOTNO),"TOTNO"]<-0
  GSCAT_[is.na(GSCAT_$TOTWGT),"TOTWGT"]<-0
  GSCAT_[is.na(GSCAT_$SAMPWGT),"SAMPWGT"]<-0
  GSCAT_[is.na(GSCAT_$CALWT),"CALWT"]<-0
  
  GSCAT_ <- stats::aggregate(
    x = list(
      CALWT = GSCAT_$CALWT,
      SAMPWGT = GSCAT_$SAMPWGT,
      TOTWGT = GSCAT_$TOTWGT,
      TOTNO = GSCAT_$TOTNO),
    by = list(
      MISSION = GSCAT_$MISSION,
      SETNO = GSCAT_$SETNO,
      SPEC = GSCAT_$SPEC),
    sum
  )
  return(GSCAT_)
}

#' @title addDDCoords
#' @description Converts latitude and longitude coordinates from DDMMMM format (degrees and decimal minutes) 
#' to decimal degrees for both start (SLAT/SLONG) and end (ELAT/ELONG) positions of tows.
#' @param GSINF_ the default is \code{NULL}. Original GSINF object containing set information with 
#' coordinates in DDMMMM format
#' @return The GSINF dataframe with additional columns for decimal degree coordinates: SLAT_DD, SLONG_DD, 
#' ELAT_DD, and ELONG_DD
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
addDDCoords <- function(GSINF_=NULL){
  requireNamespace("Mar.utils", quietly = TRUE)
  GSINF_ <- Mar.utils::DDMMx_to_DD(df=GSINF_, format = "DDMMMM", lat.field = "ELAT", lon.field = "ELONG", WestHemisphere = T)
  colnames(GSINF_)[colnames(GSINF_)=="LAT_DD"] <- "ELAT_DD"
  colnames(GSINF_)[colnames(GSINF_)=="LON_DD"] <- "ELONG_DD"
  
  GSINF_ <- Mar.utils::DDMMx_to_DD(df=GSINF_, format = "DDMMMM", lat.field = "SLAT", lon.field = "SLONG", WestHemisphere = T)
  colnames(GSINF_)[colnames(GSINF_)=="LAT_DD"] <- "SLAT_DD"
  colnames(GSINF_)[colnames(GSINF_)=="LON_DD"] <- "SLONG_DD"
  
  return(GSINF_)
}

#' @title roundDD2Min
#' @description Rounds decimal degree coordinates to the nearest specified minute interval. 
#' Useful for generating plot boundaries that align with geographic minutes.
#' @param x the default is \code{NULL}. A numeric value in decimal degrees to be rounded
#' @param how the default is \code{"round"}. The rounding method to use - one of \code{"round"}, 
#' \code{"ceiling"}, or \code{"floor"}. \code{"round"} rounds to the nearest value, while 
#' \code{"ceiling"} and \code{"floor"} forcibly round up or down respectively
#' @param nearestMin the default is \code{1}. The minute interval to round to (values between 1 and 60 make sense)
#' @param digits the default is \code{4}. Number of decimal places in the result
#' @return A numeric value representing the rounded decimal degree coordinate
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @noRd
roundDD2Min<-function(x=NULL, how = "round", nearestMin = 1, digits=4){
  minDD = 0.016666666666 #this is 1 min in DD
  base = nearestMin*minDD
  if (how =="round"){
    res <- base * round(x/base)
  }else if (how =="ceiling"){
    res <- base * ceiling(x/base)
  }else if (how=="floor"){
    res <- base * floor(x/base)
  }
  res <- round(res,digits)
  return(res)
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

calcTotalSE_unstratified  <- function(theDataByStrat = NULL, valueField = NULL){
  res <- round(sqrt(sum(theDataByStrat[[valueField]]^2)), 5)
  return(res)
}

calcTotalSE_stratified  <- function(theDataByStrat = NULL, valueField = NULL, areaField = NULL){
  totArea <- sum(theDataByStrat[[areaField]])
  res <- sqrt(sum((theDataByStrat[[areaField]] / totArea)^2 * theDataByStrat[[valueField]]^2))
  res <- round(res, 5)
  return(res)
}

calcTotalMean <- function(theDataByStrat = NULL, valueField = NULL, areaField = NULL){
  totArea <- sum(theDataByStrat[[areaField]])
  res <- round(sum(theDataByStrat[[valueField]] * theDataByStrat[[areaField]] / totArea), 5)
  return(res)
}

calcTotalCI <- function(theDataByStrat = NULL, meanField = NULL, seField = NULL, areaField = NULL, level = 0.95){
  mean_val <- calcTotalMean(theDataByStrat, meanField, areaField)
  se_val <- calcTotalSE(theDataByStrat, seField, areaField)
  
  z_score <- qnorm(1 - (1 - level) / 2)
  
  lower_ci <- round(mean_val - (z_score * se_val), 5)
  upper_ci <- round(mean_val + (z_score * se_val), 5)
  
  return(c(lower_ci = lower_ci, upper_ci = upper_ci))
}

# calcYearSummaryMarea <- function(theDataByStrat = NULL, year = NULL, valueField = NULL, seField = NULL, areaField = NULL, panel.category = NULL, ts.name = NULL, level = 0.95, is_mean = TRUE){
#   
#   if(is_mean){
#     value <- calcTotalMean(theDataByStrat, valueField, areaField)
#     se_val <- calcTotalSE_stratified(theDataByStrat, seField, areaField)
#   } else {
#     value <- round(sum(theDataByStrat[[valueField]]), 5)
#     se_val <- calcTotalSE_unstratified(theDataByStrat, seField)
#   }
#   
#   se_val <- calcTotalSE(theDataByStrat, seField, areaField)
#   z_score <- qnorm(1 - (1 - level) / 2)
#   
#   lower_ci <- round(value - (z_score * se_val), 5)
#   upper_ci <- round(value + (z_score * se_val), 5)
#   
#   result <- data.frame(
#     panel.category = panel.category,
#     year = year,
#     ts.name = ts.name,
#     value = value,
#     low = lower_ci,
#     high = upper_ci
#   )
#   
#   return(result)
# }
calcYearSummary <- function(theDataByStrat = NULL, year = NULL, valueField = NULL, seField = NULL, areaField = NULL, level = 0.95, is_mean = TRUE){
  if(is_mean){
    value <- calcTotalMean(theDataByStrat, valueField, areaField)
    se_val <- calcTotalSE_stratified(theDataByStrat, seField, areaField)
  } else {
    value <- round(sum(theDataByStrat[[valueField]], na.rm = T), 5)
    se_val <- calcTotalSE_unstratified(theDataByStrat, seField)
  }

  z_score <- qnorm(1 - (1 - level) / 2)
  
  lower_ci <- round(value - (z_score * se_val), 5)
  upper_ci <- round(value + (z_score * se_val), 5)
  
  result <- data.frame(
    value = value,
    se = se_val,
    low = lower_ci,
    high = upper_ci
  )
  
  return(result)
}