#functions
utils::globalVariables(c("dev.cur", "png", "dev.off","qnorm"))
#aggregateByTaxa
utils::globalVariables(c("TOTNO", "TOTWGT", "CLEN"))  
#loadRVData
utils::globalVariables(c("STRAT", "AREA", "AREA_KM2"))
#standardize_catch_counts
utils::globalVariables(c("SIZE_CLASS","WINGSPREAD_FT","TYPE","weight_ratio","CAGE","CLEN_sqkm","CLEN_TOTAL","CLEN_SQKM_TOTAL","CAGE_sqkm","CAGE_TOTAL","CAGE_SQKM_TOTAL"))
#stratify_detailed
utils::globalVariables(c("CLEN_TOTAL","CLEN_SQKM_TOTAL","CLEN_values","CLEN_SQKM_values","CLEN_SQKM_MEAN","CAGE_TOTAL","CAGE_SQKM_TOTAL","CAGE_values","CAGE_SQKM_values", "CAGE_SQKM_MEAN","CLEN_TOTAL", "CLEN_SQKM_TOTAL", "CLEN_values", "CLEN_SQKM_values", "CLEN_SQKM_MEAN", "CAGE_TOTAL", "CAGE_SQKM_TOTAL", "CAGE_values", "CAGE_SQKM_values", "CAGE_SQKM_MEAN", "CLEN_SQKM_SE", "FLEN_col", "SEX_ORDER", "FLEN_NUM", "LENGTH_MEAN_SE", "LENGTH_TOTAL_SE", "se", "value"))
#stratify_simple
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

#rawTables  <- c("GSCAT", "GSDET", "GSGEAR", "GSINF", "GSMATURITY", "GSMISSIONS", "GSSEX", "GSSPECIES_NEW", "GSSTRATUM", "GSVESSEL","GSWARPOUT", "GSXTYPE")
coreTables <- c("GSCAT", "GSDET", "GSGEAR", "GSINF", "GSMATURITY", "GSMISSIONS", "GSSEX", "GSSPECIES_NEW", "GSSTRATUM", "GSVESSEL", "GSWARPOUT", "GSXTYPE", "STOMACH_DATA_VW") 
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
  GSDET_ |>
    dplyr::mutate(
      FLEN = dplyr::case_when(
        SPEC == 60 &
          substr(MISSION, 4, 7) <= 2016 &
          !MISSION %in% c("NED2016116", "NED2016016") &
          !is.na(FLEN) ~ FLEN * 10,
        TRUE ~ FLEN
      )
    )
}


fixGrenadierLengths <-function(GSDET_ = NULL){
  #Fix grenadier lengths
  GSDET_ |>
    dplyr::mutate(
      FLEN = dplyr::case_when(
        SPEC == 410 & substr(MISSION, 4, 7) < 2019 & MISSION != "NED2010027" ~ ((FLEN - 1.770067) / 4.453725) * 10,
        SPEC == 410 & MISSION == "NED2010027" & !SETNO %in% c(219, 222, 223, 229, 230, 233, 236) ~ ((FLEN - 1.770067) / 4.453725) * 10,
        SPEC == 410 & MISSION == "NED2010027" & SETNO %in% c(219, 222, 223, 229, 230, 233, 236) ~ FLEN * 10,
        SPEC == 410 & MISSION == "NED2019102" & SETNO %in% c(13, 15, 132, 133, 150) ~ FLEN * 10,
        SPEC == 410 & MISSION == "NED2019030" & SETNO %in% c(132, 133, 150) ~ FLEN * 10,
        TRUE ~ FLEN
      )
    )
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

##### STRANAL utilities start
#' @title calcTotalSE_unstratified
#' @description Calculate the total standard error for unstratified data by taking the square root of the sum of squared standard errors.
#' @param theDataByStrat the default is \code{NULL}. A data frame containing stratified data.
#' @param valueField the default is \code{NULL}. The name of the field containing standard error values to be summed.
#' @return A numeric value representing the total standard error, rounded to 5 decimal places.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
calcTotalSE_unstratified  <- function(theDataByStrat = NULL, valueField = NULL){
  res <- round(sqrt(sum(theDataByStrat[[valueField]]^2)), 5)
  return(res)
}

#' @title calcTotalSE_stratified
#' @description Calculate the total standard error for stratified data using area-weighted standard errors.
#' @param theDataByStrat the default is \code{NULL}. A data frame containing stratified data.
#' @param valueField the default is \code{NULL}. The name of the field containing standard error values.
#' @param areaField the default is \code{NULL}. The name of the field containing area values for weighting.
#' @return A numeric value representing the stratified total standard error, rounded to 5 decimal places.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
calcTotalSE_stratified  <- function(theDataByStrat = NULL, valueField = NULL, areaField = NULL){
  totArea <- sum(theDataByStrat[[areaField]])
  res <- round(sqrt(sum((theDataByStrat[[areaField]] / totArea)^2 * theDataByStrat[[valueField]]^2)),5)
  return(res)
}

#' @title calcTotalMean
#' @description Calculate the area-weighted mean across strata.
#' @param theDataByStrat the default is \code{NULL}. A data frame containing stratified data.
#' @param valueField the default is \code{NULL}. The name of the field containing values to be averaged.
#' @param areaField the default is \code{NULL}. The name of the field containing area values for weighting.
#' @return A numeric value representing the area-weighted mean, rounded to 5 decimal places.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
calcTotalMean <- function(theDataByStrat = NULL, valueField = NULL, areaField = NULL){
  totArea <- sum(theDataByStrat[[areaField]])
  res <- round(sum(theDataByStrat[[valueField]] * theDataByStrat[[areaField]] / totArea), 5)
  return(res)
}

#' @title calcYearSummary
#' @description Calculate annual summary statistics including value, standard error, and confidence intervals for either means or totals.
#' @param theDataByStrat the default is \code{NULL}. A data frame containing stratified data.
#' @param year the default is \code{NULL}. The year for which summary statistics are being calculated.
#' @param valueField the default is \code{NULL}. The name of the field containing values to be summarized.
#' @param seField the default is \code{NULL}. The name of the field containing standard error values.
#' @param areaField the default is \code{NULL}. The name of the field containing area values for weighting.
#' @param is_mean the default is \code{TRUE}. If TRUE, calculates stratified mean; if FALSE, calculates unstratified total.
#' @return A data frame containing value, se, low (lower CI), and high (upper CI) columns.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
calcYearSummary <- function(theDataByStrat = NULL, year = NULL, valueField = NULL, seField = NULL, areaField = NULL, is_mean = TRUE){
  if(is_mean){
    value <- calcTotalMean(theDataByStrat, valueField, areaField)
    se_data <- theDataByStrat[!is.na(theDataByStrat[[seField]]), ]
    se_val <- calcTotalSE_stratified(se_data, seField, areaField)
  } else {
    value <- round(sum(theDataByStrat[[valueField]], na.rm = TRUE), 5)
    se_data <- theDataByStrat[!is.na(theDataByStrat[[seField]]), ]
    se_val <- calcTotalSE_unstratified(se_data, seField)
  }

  result <- data.frame(
    value = value,
    se = se_val
  )

  return(result)
}

#' @title standardize_catch_counts
#' @description Standardize catch counts from detailed length/age data (GSDET) by applying weight ratios, correcting for tow distance, and calculating density per square kilometer. Returns standardized data at the individual level as well as aggregated totals by length and age.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSCAT, GSDET, and GSINF.
#' @param towDist the default is \code{1.75}. The standard tow distance in nautical miles used for standardization.
#' @param by_sex the default is \code{FALSE}. If TRUE, calculations are grouped by sex (FSEX) in addition to other grouping variables.
#' @return A list containing three elements: standardized_data (individual-level records), length_total (aggregated by length), and age_total (aggregated by age). Returns NA for length_total or age_total if no length or age data exist.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr select left_join mutate filter summarise cross_join if_else distinct all_of case_when
#' @importFrom tidyr crossing
#' @export
standardize_catch_counts <- function(tblList, towDist = 1.75, by_sex = FALSE) {
  if(!"SPEC" %in% names(tblList$GSCAT) | nrow(tblList$GSDET) < 1) stop("Either this is Taxa-level data or GSDET has no records.  Either way, please use stratify_simple() instead")
  weight_data <- tblList$GSCAT |>
    select(MISSION, SETNO, SPEC, SIZE_CLASS, SAMPWGT, TOTWGT)
  
  dist_data <- tblList$GSINF |>
    select(MISSION, SETNO, DIST, AREA_KM2, GEAR, WINGSPREAD_FT)
  
  all_sets <- tblList$GSINF |>
    filter(TYPE == 1) |>
    select(MISSION, STRAT, SETNO, AREA_KM2) |>
    distinct()
  
  #ensure that NA FSEX are changed to 0, and anything recorded as "berried" is interpreted as female
  standardized_data <- tblList$GSDET |>
    left_join(weight_data, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")) |>
    left_join(dist_data, by = c("MISSION", "SETNO")) |>
    mutate(
      weight_ratio = case_when(
        is.na(TOTWGT) | is.na(SAMPWGT) | TOTWGT == 0 | SAMPWGT == 0 ~ 1,
        TRUE ~ TOTWGT / SAMPWGT
      ),
      FSEX = if_else(is.na(FSEX), 0, if_else(FSEX == 3, 2, FSEX)),
      CLEN = CLEN * weight_ratio,
      CAGE = if_else(!is.na(AGE), CLEN, NA_real_),
      DIST = if_else(is.na(DIST), towDist, DIST),
      CLEN_RAW = CLEN,
      CAGE_RAW = CAGE,
      CLEN = CLEN * (towDist / DIST),
      CAGE = if_else(!is.na(CAGE), CAGE * (towDist / DIST), NA_real_),
      CLEN_sqkm = if_else(!is.na(CLEN), valPerSqKm(CLEN, towDist_NM = DIST, netWidth_ft = WINGSPREAD_FT), NA_real_),
      CAGE_sqkm = if_else(!is.na(CAGE), valPerSqKm(CAGE, towDist_NM = DIST, netWidth_ft = WINGSPREAD_FT), NA_real_)
    ) |>
    select(-weight_ratio, -SAMPWGT, -TOTWGT, -SIZE_CLASS)
  
  spec_only <- standardized_data |>
    select(SPEC) |>
    distinct()
  
  base_groups <- c("MISSION", "SETNO", "SPEC")

  if (by_sex) base_groups <- c(base_groups, "FSEX")
  length_total <- if (any(!is.na(standardized_data$FLEN))) {
    length_data <- standardized_data |>
      filter(!is.na(FLEN)) |>
      summarise(
        CLEN_TOTAL = sum(CLEN, na.rm = TRUE),
        CLEN_SQKM_TOTAL = sum(CLEN_sqkm, na.rm = TRUE),
        .by = all_of(base_groups) |> c("FLEN")
      )
    if(by_sex){
      all_combos <- all_sets |>
        cross_join(spec_only) |>
        cross_join(length_data |> select(FLEN) |> distinct()) |> 
        cross_join(length_data |> select(FSEX) |> distinct())
      
      all_combos |>
        left_join(length_data, by = c("MISSION", "SETNO", "SPEC", "FLEN","FSEX")) |>
        mutate(
          CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
          CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
        )
    }else{
      all_combos <- all_sets |>
        cross_join(spec_only) |>
        cross_join(length_data |> select(FLEN) |> distinct())
      
      all_combos |>
        left_join(length_data, by = c("MISSION", "SETNO", "SPEC", "FLEN")) |>
        mutate(
          CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
          CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
        )
    }
  } else {
    NA
  }

  age_total <- if (any(!is.na(standardized_data$AGE))) {
    age_data <- standardized_data |>
      filter(!is.na(AGE)) |>
      summarise(
        CAGE_TOTAL = sum(CAGE, na.rm = TRUE),
        CAGE_SQKM_TOTAL = sum(CAGE_sqkm, na.rm = TRUE),
        .by = all_of(base_groups) |> c("AGE")
      )
    if(by_sex){
      all_combos <- all_sets |>
        cross_join(spec_only) |>
        cross_join(age_data |> select(AGE) |> distinct()) |> 
        cross_join(length_data |> select(FSEX) |> distinct())
      
      all_combos |>
        left_join(age_data, by = c("MISSION", "SETNO", "SPEC", "AGE","FSEX")) |>
        mutate(
          CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
          CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
        )
      
    } else{
      all_combos <- all_sets |>
        cross_join(spec_only) |>
        cross_join(age_data |> select(AGE) |> distinct())
      
      all_combos |>
        left_join(age_data, by = c("MISSION", "SETNO", "SPEC", "AGE")) |>
        mutate(
          CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
          CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
        )
    }  
    
  } else {
    NA
  }
  return(list(
    standardized_data = standardized_data,
    length_total = length_total,
    age_total = age_total
  ))
}

#' @title widen_data
#' @description Transform long-format biological data (e.g., length or age) into wide format with bins as columns. This function bins a specified continuous variable (such as fish length or age) according to a given bin size, aggregates a chosen value column within each bin, and creates a wide-format table where each bin becomes a separate column. Supports optional sex-based grouping and operates at either set-level (individual tows) or strata-level (aggregated across sets). Missing bins are filled with zeros to ensure complete coverage across the range.
#'
#' @param data A data frame containing the data to be widened. Must include columns for the variable specified in `var_col`, STRAT (stratum), AREA_KM2 (area), SPEC (species code), and the column specified in `value_col`. For set-level data, must also include MISSION and SETNO.
#' @param var_col A string specifying the name of the continuous variable to bin (e.g., "FLEN" for length or "AGE" for age).
#' @param value_col A string specifying the name of the column to aggregate and pivot (e.g., "CLEN_TOTAL", "CAGE_TOTAL", "CLEN_MEAN", etc.).
#' @param bin_size Numeric. Default is \code{1}. Only applicable when \code{var_col} is FLEN.The width of bins. Values are binned using the formula: \code{bin_size * floor(var / bin_size)}.
#' @param level Character. Either \code{"set"} or \code{"strata"}. Specifies the aggregation level. "set" includes MISSION and SETNO in grouping; "strata" aggregates across all sets within each stratum.
#' @param by_sex Logical. Default is \code{FALSE}. If \code{TRUE}, includes FSEX in grouping and creates separate columns for each sex-bin combination.
#'
#' @return A wide-format data frame with columns for grouping variables (STRAT, AREA_KM2, SPEC, and optionally MISSION/SETNO and FSEX) and columns named according to the binning scheme (e.g., \code{FLEN_10}, \code{AGE_5}, or \code{U_FLEN_10} for sexed data). Rows are sorted by STRAT (and MISSION, SETNO for set-level data).
#'
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @examples \dontrun{
#' # Length example
#' len_set <- widen_data(length_complete,
#'                       var_col = "FLEN",
#'                       value_col = "CLEN_TOTAL",
#'                       bin_size = 3,
#'                       level = "set",
#'                       by_sex = TRUE)
#'
#' # Age example
#' age_strat <- widen_data(age_strat,
#'                         var_col = "AGE",
#'                         value_col = "CAGE_MEAN",
#'                         bin_size = 1,
#'                         level = "strata",
#'                         by_sex = FALSE)
#' }
#'
#' @importFrom dplyr mutate summarise arrange all_of across .data
#' @importFrom tidyr complete nesting pivot_wider
#' @importFrom rlang syms
#' @importFrom stats setNames
#' @note The binning formula (\code{bin_size * floor(var / bin_size)}) matches the DFO standard used in Oracle queries. For bin_size = 3, this creates bins at 0, 3, 6, 9, etc.
#' @export
widen_data <- function(data, var_col, value_col, level = c("strata", "set"), bin_size = 1, by_sex = FALSE) {
  if (missing(var_col)) stop("Parameter 'var_col' is missing.")
  if (missing(value_col)) stop("Parameter 'value_col' is missing.")
  level <- match.arg(level)
  
  var_col_sym <- rlang::sym(var_col)
  value_col_sym <- rlang::sym(value_col)
  
  if (var_col == "FLEN") {
    bin_col <- "FLEN_BIN"
    min_val <- min(data$FLEN)
    max_val <- max(data$FLEN)
    
    data <- binnit(data, bin_size = bin_size)
    col_for_wide <- bin_col
    seq_vals <- seq(1 + bin_size * floor(min_val / bin_size), 1 + bin_size * floor(max_val / bin_size), by = bin_size)
  } else {
    col_for_wide <- var_col
    seq_vals <- sort(unique(data[[var_col]]))
  }
  
  if (level == "set") {
    group_vars <- c("MISSION", "SETNO", "STRAT", "AREA_KM2", "SPEC", col_for_wide)
    nesting_vars <- c("MISSION", "SETNO", "STRAT", "AREA_KM2", "SPEC")
    arrange_vars <- c("STRAT", "MISSION", "SETNO")
  } else {
    group_vars <- c("STRAT", "AREA_KM2", "SPEC", col_for_wide)
    nesting_vars <- c("STRAT", "AREA_KM2", "SPEC")
    arrange_vars <- "STRAT"
  }
  
  if (by_sex) {
    group_vars <- c(group_vars, "FSEX")
    nesting_vars <- c(nesting_vars, "FSEX")
  }
  
  fill_list <- setNames(list(0), value_col)
  result<- data |>
    summarise("{value_col}" := sum(.data[[value_col]]), .by = all_of(group_vars)) |>
    tidyr::complete(
      nesting(!!!syms(nesting_vars)),
      !!col_for_wide := seq_vals,
      fill = fill_list
    ) |>
    tidyr::pivot_wider(
      names_from = if (by_sex) c("FSEX", col_for_wide) else col_for_wide, values_from = all_of(value_col),
      names_glue = if (by_sex) {
        paste0("{c(FSEX = c('U','M','F')[FSEX+1])}_", toupper(var_col), "_{", col_for_wide, "}")
      } else {
        paste0(toupper(var_col), "_{", col_for_wide, "}")
      },
      values_fill = 0
    ) |>
    arrange(across(all_of(arrange_vars)))
  
  return(result)
}

#' @title binnit
#' @description Assigns each FLEN to a length bin using the specified bin size. Bin values are calculated as the lower 
#' edge of each bin, using the formula 1 + bin_size * floor(FLEN / bin_size). This approach is consistent with legacy 
#' APL code, but bins do not necessarily start at the minimum observed length.
#' @param data A data frame containing a numeric column named \code{FLEN}.
#' @param bin_size Numeric value specifying the width of each length bin. Default is \code{1}.
#' @return The input data frame with an added column \code{FLEN_BIN} indicating the bin assignment for each fish length.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr mutate
#' @export
binnit <- function(data, bin_size = 1) {
  FLEN_BIN <- 1 + bin_size * floor(data$FLEN / bin_size)
  dplyr::mutate(data, FLEN_BIN = FLEN_BIN)
}

# summarize_by_age <- function(data, value_col, area_col = "AREA_KM2", group_col = "STRAT", age_col = "AGE", method = c("weighted_mean", "rss_se", "rss_se_no_area", "weighted_mean_by_count"), count_col = NULL) {
#   method <- match.arg(method)
#   ages <- sort(unique(data[[age_col]]))
#   result <- sapply(ages, function(a) {
#     subset <- data[data[[age_col]] == a, ]
#     strata <- unique(subset[[group_col]])
#     if (method == "weighted_mean") {
#       mean_by_stratum <- sapply(strata, function(s) mean(subset[subset[[group_col]] == s, value_col], na.rm = TRUE))
#       area_by_stratum <- as.numeric(sapply(strata, function(s) unique(subset[subset[[group_col]] == s, area_col])[1]))
#       sum(mean_by_stratum * area_by_stratum) / sum(area_by_stratum)
#     } else if (method == "rss_se") {
#       se_by_stratum <- as.numeric(sapply(strata, function(s) subset[subset[[group_col]] == s, value_col][1]))
#       area_by_stratum <- as.numeric(sapply(strata, function(s) unique(subset[subset[[group_col]] == s, area_col])[1]))
#       sqrt(sum((se_by_stratum^2) * (area_by_stratum^2), na.rm = TRUE)) / sum(area_by_stratum)
#     } else if (method == "rss_se_no_area") {
#       se_by_stratum <- as.numeric(sapply(strata, function(s) subset[subset[[group_col]] == s, value_col][1]))
#       sqrt(sum(se_by_stratum^2, na.rm = TRUE))
#     } else if (method == "weighted_mean_by_count" && !is.null(count_col)) {
#       w <- subset[[value_col]]
#       n <- subset[[count_col]]
#       if (sum(n, na.rm = TRUE) == 0) return(NA)
#       sum(w * n, na.rm = TRUE) / sum(n, na.rm = TRUE)
#     } else {
#       NA
#     }
#   })
#   names(result) <- ages
#   return(result)
# }

summarize_by_age2 <- function(data, value_col, area_col = "AREA_KM2", group_col = "STRAT", age_col = "AGE", method = c("weighted_mean", "rss_se", "rss_se_no_area", "weighted_mean_by_count"), count_col = NULL) {
  method <- match.arg(method)
  ages <- sort(unique(data[[age_col]]))
  result <- sapply(ages, function(a) {
    subset <- data[data[[age_col]] == a, ]
    strata <- unique(subset[[group_col]])
    if (method == "weighted_mean") {
      mean_by_stratum <- sapply(strata, function(s) mean(subset[subset[[group_col]] == s, value_col], na.rm = TRUE))
      area_by_stratum <- as.numeric(sapply(strata, function(s) unique(subset[subset[[group_col]] == s, area_col])[1]))
      calcTotalMean(theDataByStrat = data.frame(value = mean_by_stratum, area = area_by_stratum), valueField = "value", areaField = "area")
    } else if (method == "rss_se") {
      first_by_stratum <- as.numeric(sapply(strata, function(s) subset[subset[[group_col]] == s, value_col][1]))
      area_by_stratum <- as.numeric(sapply(strata, function(s) unique(subset[subset[[group_col]] == s, area_col])[1]))
      calcTotalSE_stratified(theDataByStrat = data.frame(value = first_by_stratum, area = area_by_stratum), valueField = "value", areaField = "area")
    } else if (method == "rss_se_no_area") {
      #sum(subset[[value_col]], na.rm = TRUE)
      # calcTotalSE_unstratified(theDataByStrat = subset, valueField = value_col)
      calcTotalSE_unstratified(theDataByStrat = subset[subset[[value_col]] != 0, ], valueField = value_col)
    } else if (method == "weighted_mean_by_count" && !is.null(count_col)) {
      w <- subset[[value_col]]
      n <- subset[[count_col]]
      if (sum(n, na.rm = TRUE) == 0) return(NA)
      sum(w * n, na.rm = TRUE) / sum(n, na.rm = TRUE)
    } else {
      NA
    }
  })
  names(result) <- ages
  return(result)
}
