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
#' @param level the default is \code{0.95}. The confidence level for the interval (e.g., 0.95 for 95\% CI).
#' @param is_mean the default is \code{TRUE}. If TRUE, calculates stratified mean; if FALSE, calculates unstratified total.
#' @return A data frame containing value, se, low (lower CI), and high (upper CI) columns.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

calcYearSummary <- function(theDataByStrat = NULL, year = NULL, valueField = NULL, seField = NULL, areaField = NULL, level = 0.95, is_mean = TRUE){
  if(is_mean){
    value <- calcTotalMean(theDataByStrat, valueField, areaField)
    se_data <- theDataByStrat[!is.na(theDataByStrat[[seField]]), ]
    se_val <- calcTotalSE_stratified(se_data, seField, areaField)
  } else {
    value <- round(sum(theDataByStrat[[valueField]], na.rm = TRUE), 5)
    se_data <- theDataByStrat[!is.na(theDataByStrat[[seField]]), ]
    se_val <- calcTotalSE_unstratified(se_data, seField)
    
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

#' @title widen_length_data
#' @description Transform length frequency data from long to wide format, with length bins as columns. This function bins fish length data according to a specified bin size, aggregates a chosen value column within each bin, and creates a wide-format table where each length bin becomes a separate column. Can operate at either set-level (individual tows) or strata-level (aggregated across sets). Missing length bins are filled with zeros to ensure complete coverage across the length range.
#' @param data A data frame containing length frequency data. Must include columns: FLEN (fish length), STRAT (stratum), AREA_KM2 (area), SPEC (species code), and the column specified in value_col. For set-level data, must also include MISSION and SETNO.
#' @param value_col The name of the column to aggregate and pivot. Common options include "CLEN_TOTAL", "CLEN_MEAN", "CLEN_SE", "CLEN_STRAT_TOTAL", or "CLEN_SQKM_SE".
#' @param bin_size the default is \code{1}. The width of length bins. Length values are binned using the formula: 1 + bin_size * floor(FLEN / bin_size).
#' @param level the default is \code{c("strata", "set")}. Specifies the aggregation level. "set" includes MISSION and SETNO in grouping; "strata" aggregates across all sets within each stratum.
#' @param by_sex the default is \code{F}.   
#' @return A wide-format data frame with columns for grouping variables (STRAT, AREA_KM2, SPEC, and optionally MISSION/SETNO) and FLEN_* columns (one per length bin). Rows are sorted by STRAT (and MISSION, SETNO for set-level data).
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @examples \dontrun{
#' result5 <- widen_length_data(testData_strat_det$length_set, 
#'                              value_col = "CLEN_TOTAL",  
#'                              bin_size = 3,
#'                              level = "set")
#' result1 <- widen_length_data(testData_strat_det$length_strat, 
#'                              value_col = "CLEN_MEAN", 
#'                              bin_size = 1,
#'                              level = "strat")
#' result2 <- widen_length_data(testData_strat_det$length_strat, 
#'                              value_col = "CLEN_SE", 
#'                              bin_size = 1,
#'                              level = "strat")
#' result3 <- widen_length_data(testData_strat_det$length_strat, 
#'                              value_col = "CLEN_STRAT_TOTAL", 
#'                              bin_size = 1,
#'                              level = "strat")
#' result4 <- widen_length_data(testData_strat_det$length_strat, 
#'                              value_col = "CLEN_SQKM_SE", 
#'                              bin_size = 1,
#'                              level = "strat")
#' }
#' @importFrom data.table :=
#' @importFrom dplyr mutate summarise arrange all_of across .data
#' @importFrom tidyr complete nesting pivot_wider
#' @importFrom rlang syms
#' @importFrom stats setNames
#' @note The binning formula (1 + bin_size * floor(FLEN / bin_size)) matches the DFO standard used in Oracle queries. For bin_size=3, this creates bins at 4, 7, 10, 13, etc.
#' @export
# widen_length_data <- function(data, value_col, bin_size = 1, level = c("strata", "set"), by_sex=F) {
#   level <- match.arg(level)
#   min_flen <- min(data$FLEN)
#   max_flen <- max(data$FLEN)
#   
#   fill_list <- setNames(list(0), value_col)
#   
#   if (level == "set") {
#     group_vars <- c("MISSION", "SETNO", "STRAT", "AREA_KM2", "SPEC", "FLEN_BIN")
#     nesting_vars <- c("MISSION", "SETNO", "STRAT", "AREA_KM2", "SPEC")
#     arrange_vars <- c("STRAT", "MISSION", "SETNO")
#   } else {
#     group_vars <- c("STRAT", "AREA_KM2", "SPEC", "FLEN_BIN")
#     nesting_vars <- c("STRAT", "AREA_KM2", "SPEC")
#     arrange_vars <- "STRAT"
#   }
#   
#   data |>
#     mutate(FLEN_BIN = 1 + bin_size * floor(FLEN / bin_size)) |>
#     summarise("{value_col}" := sum(.data[[value_col]]), 
#               .by = all_of(group_vars)) |>
#     complete(nesting(!!!syms(nesting_vars)),
#              FLEN_BIN = seq(1 + bin_size * floor(min_flen / bin_size),
#                             1 + bin_size * floor(max_flen / bin_size),
#                             by = bin_size),
#              fill = fill_list) |>
#     pivot_wider(names_from = FLEN_BIN,
#                 values_from = all_of(value_col),
#                 names_prefix = "FLEN_",
#                 values_fill = 0) |> 
#     arrange(across(all_of(arrange_vars)))
# }

widen_length_data <- function(data, value_col, bin_size = 1, level = c("strata", "set"), by_sex = FALSE) {
  level <- match.arg(level)
  min_flen <- min(data$FLEN)
  max_flen <- max(data$FLEN)
  
  fill_list <- setNames(list(0), value_col)
  
  if (level == "set") {
    group_vars <- c("MISSION", "SETNO", "STRAT", "AREA_KM2", "SPEC", "FLEN_BIN")
    nesting_vars <- c("MISSION", "SETNO", "STRAT", "AREA_KM2", "SPEC")
    arrange_vars <- c("STRAT", "MISSION", "SETNO")
  } else {
    group_vars <- c("STRAT", "AREA_KM2", "SPEC", "FLEN_BIN")
    nesting_vars <- c("STRAT", "AREA_KM2", "SPEC")
    arrange_vars <- "STRAT"
  }
  
  # Add FSEX to grouping if by_sex = TRUE
  if (by_sex) {
    group_vars <- c(group_vars, "FSEX")
    nesting_vars <- c(nesting_vars, "FSEX")
  }
  
  data |>
    mutate(FLEN_BIN = 1 + bin_size * floor(FLEN / bin_size)) |>
    summarise("{value_col}" := sum(.data[[value_col]]), .by = all_of(group_vars)) |>
    complete(
      nesting(!!!syms(nesting_vars)),
      FLEN_BIN = seq(1 + bin_size * floor(min_flen / bin_size),
                     1 + bin_size * floor(max_flen / bin_size),
                     by = bin_size),
      fill = fill_list
    ) |>
    pivot_wider(
      names_from = if (by_sex) c("FSEX", "FLEN_BIN") else "FLEN_BIN",
      values_from = all_of(value_col),
      names_glue = if (by_sex) "{c(FSEX = c('U','M','F')[FSEX+1])}_FLEN_{FLEN_BIN}" else "FLEN_{FLEN_BIN}",
      values_fill = 0
    ) |>
    arrange(across(all_of(arrange_vars)))
}
#' @title stranal_simple
#' @description Calculate stratified estimates of biomass and abundance from RV survey data. This function handles taxa-level or species-level data without detailed length/age information. It standardizes catches to a common tow distance, calculates per-unit-area densities, and generates stratified and overall summary statistics with confidence intervals.
#' @param tblList the default is \code{NULL}. A list of RV dataframes. If provided, data will be flattened using easyFlatten().
#' @param df the default is \code{NULL}. A data frame containing pre-flattened RV data. Used if tblList is NULL.
#' @param towDist_NM the default is \code{1.75}. The standard tow distance in nautical miles used for standardization.
#' @param areaField the default is \code{"AREA_KM2"}. The name of the field containing area values.
#' @param areaFieldUnits the default is \code{c("KM2","NM2")}. The units of the area field. Must be either "KM2" or "NM2".
#' @param conf_limits the default is \code{95}.  This the value which will be used to calculate the upper and lower confidence intervals.
#' @param inc_limits the default is \code{TRUE}. This specifies if you would like to include the values for the upper and lower confidence intervals in your summary output.
#' @param debug the default is \code{FALSE}. If TRUE, additional diagnostic information is printed.
#' @return A list containing three elements: stratified_bySet (set-level calculations), stratified_byStrat (strata-level summaries), and OVERALL_SUMMARY (overall statistics with confidence intervals).
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr select mutate group_by summarise filter distinct all_of .data n
#' @importFrom tidyr crossing
#' @note This function should be used for taxa-level data or when GSDET has no records. For species-level data with length/age information, use stratify_detailed() instead.
#' @export

stranal_simple <- function(tblList=NULL, df=NULL, towDist_NM = 1.75, areaField = "AREA_KM2", areaFieldUnits= c("KM2","NM2"), conf_limits = 95, inc_limits =T, debug=F){
  if(!is.null(tblList)){
    df <- easyFlatten(tblList)
    df <- df[df$TYPE==1,]
  }else{
    areaFieldUnits <- match.arg(areaFieldUnits)
    if (!areaField %in% names(df)) stop(sprintf("Column %s not found in data frame", areaField))
    if (!"STRAT" %in% names(df)) stop("Column STRAT not found in data frame")
    if (!"WINGSPREAD_FT" %in% names(df)) stop("Column WINGSPREAD_FT not found in data frame")
    if (!"MISSION" %in% names(df)) stop("Column MISSION not found in data frame")
    if (!"SETNO" %in% names(df)) stop("Column SETNO not found in data frame")
    if (!"DIST" %in% names(df)) stop("Column DIST not found in data frame")
    if (!"GEAR" %in% names(df)) stop("Column GEAR not found in data frame")
    if (!any(c("SPEC", "TAXA_") %in% names(df))) stop("Column SPEC (or TAXA_) not found in data frame")
    if (!"TOTNO" %in% names(df)) stop("Column TOTNO not found in data frame")
    if (!"TOTWGT" %in% names(df)) stop("Column TOTWGT not found in data frame")
    if (areaFieldUnits == "NM2") df$AREA_KM2 <- sqNMToSqKm(field = df[[areaField]])
  }
  
  results <- list() 
  
  if ("SPEC" %in% names(df)){
    df$SPEC[is.na(df$SPEC)] <- unique(df$SPEC[!is.na(df$SPEC)])
  } else if ("TAXA_" %in% names(df)){
    df$TAXA_[is.na(df$TAXA_)] <- unique(df$TAXA_[!is.na(df$TAXA_)])
    df$TAXARANK_[is.na(df$TAXARANK_)] <- unique(df$TAXARANK_[!is.na(df$TAXARANK_)])
  }
  
  df[is.na(df$DIST), "DIST"] <- towDist_NM
  df[is.na(df$TOTNO), "TOTNO"] <- 0
  df[is.na(df$TOTWGT), "TOTWGT"] <- 0
  df[is.na(df$SIZE_CLASS), "SIZE_CLASS"] <- 1
  
  df$TOTWGT_sqkm <- valPerSqKm(df$TOTWGT, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$TOTNO_sqkm <- valPerSqKm(df$TOTNO, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$BIOMASS_set <- df$TOTWGT_sqkm * df$AREA_KM2
  df$ABUNDANCE_set <- df$TOTNO_sqkm * df$AREA_KM2
  
  species_col <- if ("SPEC" %in% names(df)) "SPEC" else if ("TAXA_" %in% names(df)) "TAXA_" else NULL
  
  # setRes <- df [,c(species_col, "MISSION","SETNO","STRAT", "AREA_KM2", "SLAT_DD", "SLONG_DD", "TOTWGT", "TOTWGT_sqkm", "TOTNO", "TOTNO_sqkm")]
  setRes <- df |> 
    select(
      all_of(species_col),  # selects the column whose name is stored in species_col
      MISSION, SETNO, STRAT, AREA_KM2, AREA , SLAT_DD, SLONG_DD,
      TOTWGT, TOTWGT_sqkm, TOTNO, TOTNO_sqkm
    ) |> 
    arrange(STRAT, SETNO)
  
  results$set_stratified <- setRes
  
  all_sets <- df |>
    select(MISSION, SETNO, STRAT, AREA_KM2) |>
    distinct()
  
  spec_only <- df |>
    select(all_of(species_col)) |>
    distinct()
  
  weight_grid <- all_sets |>
    tidyr::crossing(spec_only)
  
  df_complete <- weight_grid |>
    left_join(df, by = c("MISSION", "SETNO", "STRAT", "AREA_KM2", species_col)) |>
    mutate(
      TOTWGT = ifelse(is.na(TOTWGT), 0, TOTWGT),
      TOTNO = ifelse(is.na(TOTNO), 0, TOTNO),
      TOTWGT_sqkm = ifelse(is.na(TOTWGT_sqkm), 0, TOTWGT_sqkm),
      TOTNO_sqkm = ifelse(is.na(TOTNO_sqkm), 0, TOTNO_sqkm),
      BIOMASS_set = ifelse(is.na(BIOMASS_set), 0, BIOMASS_set),
      ABUNDANCE_set = ifelse(is.na(ABUNDANCE_set), 0, ABUNDANCE_set)
    )
  
  df_strat <- df_complete |>
    group_by(.data[[species_col]], STRAT, AREA_KM2) |>
    summarise(COUNT = n(),
              TOTWGT_SUM = round(sum(TOTWGT), 5),
              TOTNO_SUM = round(sum(TOTNO), 5),
              TOTWGT_MEAN = round(mean(TOTWGT), 5),
              TOTNO_MEAN = round(mean(TOTNO), 5),
              TOTWGT_SE = round(Mar.utils::st_err(TOTWGT), 5),
              TOTNO_SE = round(Mar.utils::st_err(TOTNO), 5),
              TOTWGT_SQKM_STRAT_MEAN = round(mean(TOTWGT_sqkm), 5),
              TOTWGT_SQKM_STRAT_SE = round(Mar.utils::st_err(TOTWGT_sqkm), 5),
              TOTNO_SQKM_STRAT_MEAN = round(mean(TOTNO_sqkm), 5),
              TOTNO_SQKM_STRAT_SE = round(Mar.utils::st_err(TOTNO_sqkm), 5),
              BIOMASS_SE = round(Mar.utils::st_err(BIOMASS_set), 5),
              ABUNDANCE_SE = round(Mar.utils::st_err(ABUNDANCE_set), 5),
              .groups = "drop") |>
    mutate(BIOMASS = round(TOTWGT_SQKM_STRAT_MEAN * AREA_KM2, 5),
           ABUNDANCE = round(TOTNO_SQKM_STRAT_MEAN * AREA_KM2, 5)) |>
    as.data.frame()
  
  AREA_KM_OVERALL <- sum(df_strat$AREA_KM2, na.rm = TRUE)
  COUNT_OVERALL <- sum(df_strat$COUNT, na.rm = TRUE)
  TOTWGT_OVERALL <- sum(df_strat$TOTWGT_SUM, na.rm = TRUE)
  TOTNO_OVERALL <- sum(df_strat$TOTNO_SUM, na.rm = TRUE)
  
  TOTWGT_MEAN <- calcYearSummary(df_strat, valueField = "TOTWGT_MEAN", seField = "TOTWGT_SE", areaField = "AREA_KM2", level = conf_limits/100, is_mean = TRUE)
  TOTNO_MEAN <- calcYearSummary(df_strat, valueField = "TOTNO_MEAN", seField = "TOTNO_SE", areaField = "AREA_KM2", level = conf_limits/100, is_mean = TRUE)
  TOTWGT_SQKM_MEAN <- calcYearSummary(df_strat, valueField = "TOTWGT_SQKM_STRAT_MEAN", seField = "TOTWGT_SQKM_STRAT_SE", areaField = "AREA_KM2", level = conf_limits/100, is_mean = TRUE)
  TOTNO_SQKM_MEAN <- calcYearSummary(df_strat, valueField = "TOTNO_SQKM_STRAT_MEAN", seField = "TOTNO_SQKM_STRAT_SE", areaField = "AREA_KM2", level = conf_limits/100, is_mean = TRUE)
  BIOMASS_OVERALL <- calcYearSummary(df_strat, valueField = "BIOMASS", seField = "BIOMASS_SE", areaField = "AREA_KM2", level = conf_limits/100, is_mean = FALSE)
  ABUNDANCE_OVERALL <- calcYearSummary(df_strat, valueField = "ABUNDANCE", seField = "ABUNDANCE_SE", areaField = "AREA_KM2", level = conf_limits/100, is_mean = FALSE)
  
  overall <- data.frame(
    AREA_KM_OVERALL = AREA_KM_OVERALL,
    COUNT_OVERALL = COUNT_OVERALL,
    TOTWGT_OVERALL = TOTWGT_OVERALL,
    TOTNO_OVERALL = TOTNO_OVERALL,
    TOTWGT_MEAN = TOTWGT_MEAN$value,
    TOTWGT_MEAN_SE = TOTWGT_MEAN$se,
    TOTWGT_MEAN_LOW = TOTWGT_MEAN$low,
    TOTWGT_MEAN_HIGH = TOTWGT_MEAN$high,
    TOTNO_MEAN = TOTNO_MEAN$value,
    TOTNO_MEAN_SE = TOTNO_MEAN$se,
    TOTNO_MEAN_LOW = TOTNO_MEAN$low,
    TOTNO_MEAN_HIGH = TOTNO_MEAN$high,
    TOTWGT_SQKM_MEAN = TOTWGT_SQKM_MEAN$value,
    TOTWGT_SQKM_MEAN_SE = TOTWGT_SQKM_MEAN$se,
    TOTWGT_SQKM_MEAN_LOW = TOTWGT_SQKM_MEAN$low,
    TOTWGT_SQKM_MEAN_HIGH = TOTWGT_SQKM_MEAN$high,
    TOTNO_SQKM_MEAN = TOTNO_SQKM_MEAN$value,
    TOTNO_SQKM_MEAN_SE = TOTNO_SQKM_MEAN$se,
    TOTNO_SQKM_MEAN_LOW = TOTNO_SQKM_MEAN$low,
    TOTNO_SQKM_MEAN_HIGH = TOTNO_SQKM_MEAN$high,
    BIOMASS = round(BIOMASS_OVERALL$value, 0),
    BIOMASS_SE = round(BIOMASS_OVERALL$se, 1),
    BIOMASS_LOW = round(BIOMASS_OVERALL$low, 1),
    BIOMASS_HIGH = round(BIOMASS_OVERALL$high, 1),
    ABUNDANCE = round(ABUNDANCE_OVERALL$value, 0),
    ABUNDANCE_SE = round(ABUNDANCE_OVERALL$se, 1),
    ABUNDANCE_LOW = round(ABUNDANCE_OVERALL$low, 1),
    ABUNDANCE_HIGH = round(ABUNDANCE_OVERALL$high, 1)
  )
  max_min_fields <- c("TOTWGT_MEAN_LOW", "TOTWGT_MEAN_HIGH",
                      "TOTNO_MEAN_LOW", "TOTNO_MEAN_HIGH",
                      "TOTWGT_SQKM_MEAN_LOW", "TOTWGT_SQKM_MEAN_HIGH",
                      "TOTNO_SQKM_MEAN_LOW" , "TOTNO_SQKM_MEAN_HIGH",
                      "BIOMASS_LOW", "BIOMASS_HIGH",
                      "ABUNDANCE_LOW", "ABUNDANCE_HIGH")

    results$strat_stratified <- df_strat |> select(-TOTWGT_SUM, -TOTNO_SUM)

if (!inc_limits){
  results$summary <- overall |> select(-all_of(max_min_fields))
}else{
  results$summary <- overall
}
  return(results)
}

#' @title stranal_detailed
#' @description Calculate detailed stratified estimates of biomass and abundance from RV survey data including length and age distributions. This function extends stratify_simple by incorporating detailed catch-at-length and catch-at-age data from GSDET, standardizing individual measurements, and generating stratified summaries by length and age classes.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, and GSDET.
#' @param towDist the default is \code{1.75}. The standard tow distance in nautical miles used for standardization.
#' @param by_sex the default is \code{FALSE}. If TRUE, calculations are grouped by sex (FSEX) in addition to other grouping variables.
#' @param conf_limits the default is \code{95}.  This the value which will be used to calculate the upper and lower confidence intervals.
#' @param inc_limits the default is \code{TRUE}. This specifies if you would like to include the values for the upper and lower confidence intervals in your summary output.
#' @param bin_size the default is \code{1}.
#' @return A list containing up to seven elements: stratified_bySet, stratified_byStrat, OVERALL_SUMMARY (from stratify_simple), plus length_set, length_strat (length-based summaries), age_set, and age_strat (age-based summaries). Length and age elements are only included if corresponding data exist.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr select left_join group_by summarise distinct contains bind_rows rename_with
#' @importFrom tidyr crossing
#' @export

stranal_detailed <- function(tblList, towDist = 1.75, by_sex = FALSE, conf_limits = 95, inc_limits =T, bin_size=1) {
  
  results <- stranal_simple(tblList = tblList, towDist_NM = towDist, conf_limits = conf_limits, inc_limits = inc_limits)
  
  totals <- standardize_catch_counts(tblList, towDist = towDist, by_sex = by_sex)

  strat_lookup <- results$set_stratified |>
    select(MISSION, SETNO, STRAT, AREA_KM2) |>
    distinct()
  
  all_sets <- results$set_stratified |>
    select(MISSION, SETNO, STRAT, AREA_KM2) |>
    distinct()
  
  spec_only <- results$set_stratified |>
    select(SPEC) |>
    distinct()
  
  if (inherits(totals$length_total, "data.frame")) {
    
    length_data <- totals$length_total 
  
  if (by_sex){
    all_combos <- all_sets |>
      crossing(spec_only) |>
      crossing(length_data |> select(FLEN) |> distinct(),
                      length_data |> select(FSEX) |> distinct())
    
    length_complete <- all_combos |>
      left_join(
        length_data |> select(MISSION, SETNO, SPEC, FLEN, FSEX, CLEN_TOTAL, CLEN_SQKM_TOTAL),
        by = c("MISSION", "SETNO", "SPEC", "FSEX", "FLEN")
      ) |>
      mutate(
        CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
        CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
      )
    
    length_strat <- length_complete |>
      group_by(SPEC, STRAT, AREA_KM2, FSEX, FLEN) |>
      summarise(
        COUNT = n(),
        CLEN_SUM = sum(CLEN_TOTAL, na.rm = TRUE),
        CLEN_MEAN = mean(CLEN_TOTAL, na.rm = TRUE),
        CLEN_MEAN_SE = round(Mar.utils::st_err(CLEN_TOTAL),5),
        CLEN_values = list(CLEN_TOTAL),
        CLEN_SQKM_SUM = sum(CLEN_SQKM_TOTAL, na.rm = TRUE),
        CLEN_SQKM_MEAN = mean(CLEN_SQKM_TOTAL, na.rm = TRUE),
        CLEN_SQKM_values = list(CLEN_SQKM_TOTAL),
        .groups = "drop"
      ) |>
      mutate(
        CLEN_SE = sapply(CLEN_values, Mar.utils::st_err),
        CLEN_SQKM_SE = sapply(CLEN_SQKM_values, Mar.utils::st_err),
        CLEN_STRAT_TOTAL = CLEN_SQKM_MEAN * AREA_KM2,
        CLEN_STRAT_TOTAL_SE =  CLEN_SQKM_SE * AREA_KM2
      ) |>
      select(-CLEN_values, -CLEN_SQKM_values)
  } else{
    all_combos <- all_sets |>
      crossing(spec_only) |>
      crossing(length_data |> select(FLEN) |> distinct())
    
    length_complete <- all_combos |>
      left_join(
        length_data |> select(MISSION, SETNO, SPEC, FLEN, CLEN_TOTAL, CLEN_SQKM_TOTAL),
        by = c("MISSION", "SETNO", "SPEC", "FLEN")
      ) |>
      mutate(
        CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
        CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
      )
    
    length_strat <- length_complete |>
      group_by(SPEC, STRAT, AREA_KM2, FLEN) |>
      summarise(
        COUNT = n(),
        CLEN_SUM = sum(CLEN_TOTAL, na.rm = TRUE),
        CLEN_MEAN = mean(CLEN_TOTAL, na.rm = TRUE),
        CLEN_MEAN_SE = round(Mar.utils::st_err(CLEN_TOTAL),5),
        CLEN_values = list(CLEN_TOTAL),
        CLEN_SQKM_SUM = sum(CLEN_SQKM_TOTAL, na.rm = TRUE),
        CLEN_SQKM_MEAN = mean(CLEN_SQKM_TOTAL, na.rm = TRUE),
        CLEN_SQKM_values = list(CLEN_SQKM_TOTAL),
        .groups = "drop"
      ) |>
      mutate(
        CLEN_SE = sapply(CLEN_values, Mar.utils::st_err),
        CLEN_SQKM_SE = sapply(CLEN_SQKM_values, Mar.utils::st_err),
        CLEN_STRAT_TOTAL = CLEN_SQKM_MEAN * AREA_KM2,
        CLEN_STRAT_TOTAL_SE =  CLEN_SQKM_SE * AREA_KM2
      ) |>
      select(-CLEN_values, -CLEN_SQKM_values)
  } 
    
    lenSet_mean <- widen_length_data(length_complete, value_col = "CLEN_TOTAL", bin_size = bin_size,level = "set", by_sex = by_sex)
    #lenSet_total <- widen_length_data(length_complete, value_col = "CLEN_SQKM_TOTAL", bin_size = bin_size,level = "set")
    lenStrat_mean <- widen_length_data(length_strat, value_col = "CLEN_MEAN", bin_size = bin_size,level = "strat", by_sex = by_sex)
    lenStrat_se <- widen_length_data(length_strat, value_col = "CLEN_SE", bin_size = bin_size,level = "strat", by_sex = by_sex)
    lenStrat_total <- widen_length_data(length_strat, value_col = "CLEN_STRAT_TOTAL", bin_size = bin_size,level = "strat", by_sex = by_sex)
    lenStrat_total_se <- widen_length_data(length_strat, value_col = "CLEN_STRAT_TOTAL_SE", bin_size = bin_size,level = "strat", by_sex = by_sex)
    
    ###### MEAN ######
    
    mean_df <- lenStrat_mean |>
      left_join(
        lenStrat_se |>
          dplyr::select(-AREA_KM2, -SPEC) |>
          dplyr::rename_with(
            ~ sub("(.*FLEN_)(\\d+)$", "\\1SE_\\2", .),  # Insert _SE_ before number
            dplyr::contains("FLEN_")
          ),
        by = "STRAT"
      )
    
    # Detect original FLEN columns (no _SE_)
    flen_cols <- grep("FLEN_\\d+$", names(mean_df), value = TRUE)
    
    # Detect SE columns (with _SE_)
    se_flen_cols <- grep("FLEN_SE_\\d+$", names(mean_df), value = TRUE)
    
    
    LENGTH_MEAN <- bind_rows(
      Map(function(x, y) {
        out <- calcYearSummary(mean_df,
                               valueField = x,
                               seField = y,
                               areaField = "AREA_KM2",
                               is_mean = TRUE)
        out$FLEN_col <- x
        out |>
          select(value, se, FLEN_col) |>
          rename(LENGTH_MEAN_SE = se,
                 LENGTH_MEAN = value)
      }, flen_cols, se_flen_cols)
    )
    
    
    ###### SECOND BLOCK ######
    
    total_df <- lenStrat_total |>
      left_join(
        lenStrat_total_se |>
          dplyr::select(-AREA_KM2, -SPEC) |>
          dplyr::rename_with(
            ~ sub("(.*FLEN_)(\\d+)$", "\\1SE_\\2", .),
            contains("FLEN_")
          ),
        by = "STRAT"
      )
    
    flen_cols2 <- grep("FLEN_\\d+$", names(total_df), value = TRUE)
    se_flen_cols2 <- grep("FLEN_SE_\\d+$", names(total_df), value = TRUE)
    
    
    LENGTH_TOTAL <- bind_rows(
      Map(function(x, y) {
        out <- calcYearSummary(total_df,
                               valueField = x,
                               seField = y,
                               areaField = "AREA_KM2",
                               is_mean = FALSE)
        out$FLEN_col <- x
        out |>
          select(value, se, FLEN_col) |>
          rename(LENGTH_TOTAL_SE = se,
                 LENGTH_TOTAL = value)
      }, flen_cols2, se_flen_cols2)
    )
    
    
  summary_flen <- LENGTH_MEAN |>
    left_join(LENGTH_TOTAL, by = "FLEN_col") |>
    mutate(
      # Extract sex prefix if present (U, M, F), else empty string
      SEX = if_else(grepl("^[UMF]_FLEN_", FLEN_col),
                    sub("_FLEN_.*", "", FLEN_col),
                    ""),
      
      # Extract numeric part after last underscore
      FLEN_NUM = as.integer(sub(".*_(\\d+)$", "\\1", FLEN_col)),
      
      # Sort order for sex: none first, then U, M, F
      SEX_ORDER = case_when(
        SEX == "" ~ 0,
        SEX == "U" ~ 1,
        SEX == "M" ~ 2,
        SEX == "F" ~ 3
      )
    ) |>
    arrange(SEX_ORDER, FLEN_NUM) |>
    select(FLEN_col, LENGTH_MEAN, LENGTH_MEAN_SE, LENGTH_TOTAL, LENGTH_TOTAL_SE)
  
    
    
    results$set_flen_mean <- lenSet_mean
    #results$length_set_total <- lenSet_total
    results$strat_flen_mean <- lenStrat_mean
    results$strat_flen_mean_se <- lenStrat_se
    results$strat_flen_total <- lenStrat_total
    results$strat_flen_total_se <- lenStrat_total_se
    results$summary_flen <- summary_flen
  }
  new_order <- c(
    "summary",
    "set_stratified",
    "strat_stratified",
    "summary_flen",
    "set_flen_mean",
    "strat_flen_mean",
    "strat_flen_mean_se",
    "strat_flen_total",
    "strat_flen_total_se"
  )
  results <- results[new_order]
  if (inherits(totals$age_total, "data.frame")) {
    age_data <- totals$age_total |>
      left_join(strat_lookup, by = c("MISSION", "SETNO"))
    
    all_combos <- all_sets |>
      crossing(spec_only) |>
      crossing(age_data |> select(AGE) |> distinct())
    
    age_complete <- all_combos |>
      left_join(
        age_data |> select(MISSION, SETNO, SPEC, AGE, CAGE_TOTAL, CAGE_SQKM_TOTAL),
        by = c("MISSION", "SETNO", "SPEC", "AGE")
      ) |>
      mutate(
        CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
        CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
      )
    
    age_strat <- age_complete |>
      group_by(SPEC, STRAT, AREA_KM2, AGE) |>
      summarise(
        COUNT = n(),
        CAGE_SUM = sum(CAGE_TOTAL, na.rm = TRUE),
        CAGE_MEAN = mean(CAGE_TOTAL, na.rm = TRUE),
        CAGE_values = list(CAGE_TOTAL),
        CAGE_SQKM_SUM = sum(CAGE_SQKM_TOTAL, na.rm = TRUE),
        CAGE_SQKM_MEAN = mean(CAGE_SQKM_TOTAL, na.rm = TRUE),
        CAGE_SQKM_values = list(CAGE_SQKM_TOTAL),
        .groups = "drop"
      ) |>
      mutate(
        CAGE_SE = sapply(CAGE_values, Mar.utils::st_err),
        CAGE_SQKM_SE = sapply(CAGE_SQKM_values, Mar.utils::st_err),
        CAGE_STRAT_TOTAL = CAGE_SQKM_MEAN * AREA_KM2
      ) |>
      select(-CAGE_values, -CAGE_SQKM_values)
    
    results$age_set <- age_complete
    results$age_strat <- age_strat
  }
  
  
  
  return(results)
}
