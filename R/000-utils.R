#functions
utils::globalVariables(c("dev.cur", "png", "dev.off","qnorm"))
#aggregateByTaxa
utils::globalVariables(c("TOTNO", "TOTWGT", "CLEN"))  
#loadRVData
utils::globalVariables(c("STRAT", "AREA", "AREA_KM2"))
#standardize_catch_counts
utils::globalVariables(c("SIZE_CLASS","WINGSPREAD_FT","TYPE","weight_ratio","CAGE","CLEN_sqkm","CLEN_TOTAL","CLEN_SQKM_TOTAL","CAGE_sqkm","CAGE_TOTAL","CAGE_SQKM_TOTAL"))
#stranal_detailed
utils::globalVariables(c("CLEN_TOTAL","CLEN_SQKM_TOTAL","CLEN_values","CLEN_SQKM_values","CLEN_SQKM_MEAN","CAGE_TOTAL","CAGE_SQKM_TOTAL","CAGE_values","CAGE_SQKM_values", "CAGE_SQKM_MEAN","CLEN_TOTAL", "CLEN_SQKM_TOTAL", "CLEN_values", "CLEN_SQKM_values", "CLEN_SQKM_MEAN", "CAGE_TOTAL", "CAGE_SQKM_TOTAL", "CAGE_values", "CAGE_SQKM_values", "CAGE_SQKM_MEAN", "CLEN_SQKM_SE", "FLEN_col", "SEX_ORDER", "FLEN_NUM", "LENGTH_MEAN_SE", "LENGTH_TOTAL_SE", "se", "value"))
#stranal_simple
utils::globalVariables(c("TOTWGT_SUM", "TOTNO_SUM", "TOTWGT_sqkm","TOTNO_sqkm","BIOMASS_set","ABUNDANCE_set","TOTWGT_SQKM_STRAT_MEAN","TOTNO_SQKM_STRAT_MEAN"))
#ggStrata
utils::globalVariables(c("StrataID"))
#ggNAFO
utils::globalVariables(c("NAFO"))
#ggCatchPts
utils::globalVariables(c("SLONG_DD", "SLAT_DD"))
#extractFGP
utils::globalVariables(c("MISSION", "VESEL", "CRUNO", "YEAR", "SEASON", "SDATE", "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE", "BOTTOM_SALINITY", "SETNO", "TIME", "ELAT_DD", "ELONG_DD", "DUR", "DIST", "SPEED", "DEPTH_M", "SURF_TEMP", "BOTT_TEMP", "BOTT_SAL", "GEARDESC", "SPEC", "FLEN", "CODE", "SCI_NAME", "COMM", "APHIA_ID"))
#extractOBIS
utils::globalVariables(c("SDATE", "MISSION", "SETNO", "DMIN", "DMAX", "DEPTH", "START_DEPTH", "END_DEPTH", "TIME", "REMARKS", "SPEC", "TOTWGT_RAW", "DIST", "TOTNO_RAW", "theCatch", "FSHNO", "SPECIMEN_ID", "id", "geometry", "ELAT_DD", "ELONG_DD", "theSet", "footprintSRS", "footprintWKT", "type", "eventID", "parentEventID", "eventRemarks", "locationRemarks", "sampleSizeValue", "sampleSizeUnit", "coordinateUncertaintyInMeters", "decimalLatitude", "decimalLongitude", "eventDate", "DMIN_M", "DMAX_M", "GEAR", "HOWD", "AREA_M2", "GEARDESC", "WSPREAD", "gearType",  "BOTTOM_SALINITY",  "BOTTOM_TEMPERATURE", "DEPTH_M", "DUR", "SPEED",  "SURFACE_TEMPERATURE", "FSEX", "FWT", "FLEN", "FMAT", "AGE",  "measurement",  "measurementValue", "detID",  "occurrenceID",  "individualCount",  "organismQuantity", "CALWT", "SAMPWGT"))
#extractDATRAS
utils::globalVariables(c("DATETIME", "slat_dd", "slong_dd","tmp"))
#applyConversionFactors
utils::globalVariables(c("ATCHAM_TO_CARCAB_ABUND", "ATCHAM_TO_CARCAB_ABUND_LAM", "ATCHAM_TO_CARCAB_BMASS", "ATCHAM_TO_NEDTEM_ABUND_LAM", "ATCHAM_TO_TELVEN_ABUND", "ATCHAM_TO_TELVEN_ABUND_LAM", "ATCHAM_TO_TELVEN_BMASS", "CF_METRIC", "CF_USED", "CF_VALUE", "FROM_VESSEL", "FSEX_key", "GSCONVERSIONS", "LENWT_A", "LENWT_B", "NEDTEM_TO_CARCAB_ABUND", "NEDTEM_TO_CARCAB_ABUND_LAM", "NEDTEM_TO_CARCAB_BMASS", "NEDTEM_TO_TELVEN_ABUND", "NEDTEM_TO_TELVEN_ABUND_LAM", "NEDTEM_TO_TELVEN_BMASS", "R2", "Remove", "SAMPTOT_Ratio", "SRC", "TELVEN_TO_CARCAB_ABUND", "TELVEN_TO_CARCAB_ABUND_LAM", "TELVEN_TO_CARCAB_BMASS","GSSPEC2", "TO_VESSEL", "TYPE", "WINGSPREAD_FT"))
# calclYearSummary
utils::globalVariables(c())

rawTables  <- c("GSCAT", "GSDET", "GSGEAR", "GSINF", "GSMATURITY", "GSMISSIONS", "GSSEX", "GSSPECIES_NEW", "GSSTRATUM", "GSVESSEL","GSWARPOUT", "GSXTYPE")
coreTables <- c("GSCAT", "GSDET", "GSGEAR", "GSINF", "GSMATURITY", "GSMISSIONS", "GSSEX", "GSSPECIES_NEW", "GSSTRATUM", "GSVESSEL", "GSWARPOUT", "GSXTYPE") 
#' @title get_pesd_rvt_dir
#' @description Get or create the directory path for RVTransmogrifier data storage.
#' @return A character string representing the directory path "C:/DFO-MPO/PESDData/RVTransmogrifier". Creates the directory if it doesn't exist.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
get_pesd_rvt_dir <- function() {
  dir_path <- file.path("C:", "DFO-MPO", "PESDData","RVTransmogrifier")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  return(dir_path)
}

#' @title getSpInfo
#' @description This function returns any species that match the string sent to "taxa" for any of the recorded taxonomic
#' levels (e.g. "KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS", "SPECIES" and common name.)
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param taxa the default is \code{NULL}. Any value found in any of "SPEC", "KINGDOM", "PHYLUM", "CLASS", "ORDER", 
#' "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})
#' @returns a dataframe of all species records where some taxonomic description matches the submitted value of "taxa".
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @examples \dontrun{
#' getSpInfo(taxa ="mich", tblList=data_2025)
#'     CODE APHIA_ID      COMM                          SPEC  KINGDOM     PHYLUM       CLASS
#'   604   126306 SNIPE EEL        NEMICHTHYS SCOLOPACEUS ANIMALIA   CHORDATA   TELEOSTEI
#'  2056   106182      <NA> ARCOSCALPELLUM MICHELOTTIANUM ANIMALIA ARTHROPODA THECOSTRACA
#'  
#'  ORDER        FAMILY          GENUS
#'  ANGUILLIFORMES NEMICHTHYIDAE     NEMICHTHYS
#'  SCALPELLOMORPHA  SCALPELLIDAE ARCOSCALPELLUM
#' }
#' @export
getSpInfo <- function(taxa = NULL, tblList){
  taxa <- toupper(taxa)
  retFields <- c("CODE","APHIA_ID", "COMM","SPEC","KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS")
  these <- tblList$GSSPECIES_NEW[which(apply(tblList$GSSPECIES_NEW[,c("COMM","SPEC","KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS")], 1, function(r) any(grepl(taxa, toupper(r))))), retFields]
  these <- these |> unique()
  return(these)
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
#' @title valPerSqKm
#' @description Convert catch values to density per square kilometer based on tow distance and net width.
#' @param theData the default is \code{NULL}. A numeric vector of catch values. NA values are converted to 0.
#' @param towDist_NM the default is \code{1.75}. The tow distance in nautical miles.
#' @param netWidth_ft the default is \code{41}. The net width (wingspread) in feet.
#' @return A numeric vector of densities per square kilometer, rounded to 4 decimal places.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
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
