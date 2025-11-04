
stratify <- function(tblList=NULL, df=NULL, towDist_NM = 1.75, areaField = "AREA", areaFieldUnits= c("NM2","KM2"),debug=F){
  areaFieldUnits <- match.arg(areaFieldUnits)
  if(!is.null(tblList)){
    df <- easyFlatten(tblList)
    df <- df[df$TYPE==1,]
    df$AREA_KM<- sqNMToSqKm(field = df$AREA)
  }else{
    if (!areaField %in% names(df)) stop(sprintf("Column ",areaField," not found in data frame"))
    if (!"STRAT" %in% names(df))     stop(sprintf("Column STRAT not found in data frame"))
    if (!"DIST" %in% names(df))     stop(sprintf("Column DIST not found in data frame"))
    if (!"GEAR" %in% names(df)) stop(sprintf("Column GEAR not found in data frame"))
    if (!any(c("SPEC", "TAXA_") %in% names(df))) stop(sprintf("Column SPEC (or TAXA_) not found in data frame"))
    if (!"TOTNO" %in% names(df)) stop(sprintf("Column TOTNO not found in data frame"))
    if (!"TOTWGT" %in% names(df)) stop(sprintf("Column TOTWGT not found in data frame"))
    if (areaFieldUnits == "NM2") df$AREA_KM<- sqNMToSqKm(field = df[[areaField]])
  }
  
  results <- list() 
  
  if ("SPEC" %in% names(df)){
    df$SPEC[is.na(df$SPEC)] <- unique(df$SPEC[!is.na(df$SPEC)])
  } else if ("TAXA_" %in% names(df)){
    df$TAXA_[is.na(df$TAXA_)] <- unique(df$TAXA_[!is.na(df$TAXA_)])
    df$TAXARANK_[is.na(df$TAXARANK_)] <- unique(df$TAXARANK_[!is.na(df$TAXARANK_)])
  }
  

  df[is.na(df$DIST), "DIST"] <- 1.75
  df[is.na(df$TOTNO), "TOTNO"] <- 0
  df[is.na(df$TOTWGT), "TOTWGT"] <- 0
  
  gearDets <- data.frame(
    GEAR = c(9, 3, 15),
    GEAR_DESC = c("W2A", "Y36", "US4"),
    WINGSPREAD_M = c(12.5, 10.7, 12.6),
    WINGSPREAD_FT = c(41, 35, 41)
  )
  df <- merge(df,gearDets[,c("GEAR", "WINGSPREAD_FT")], all.x = T, by = "GEAR")
  df$TOTWGT_sqkm <- valPerSqKm(df$TOTWGT, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$BIOMASS_set<-df$TOTWGT_sqkm*df$AREA_KM
  df$TOTNO_sqkm  <- valPerSqKm(df$TOTNO, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$ABUNDANCE_set<-df$TOTNO_sqkm*df$AREA_KM
  results$stratified_bySet <- df
  
  species_col <- if ("SPEC" %in% names(df)) "SPEC" else if ("TAXA_" %in% names(df)) "TAXA_" else NULL

  df <- df |>
    # dplyr::group_by(MISSION, STRAT, AREA_KM) |>
    dplyr::group_by(.data[[species_col]], STRAT, AREA_KM) |>
    dplyr::summarise(COUNT= length(SETNO),
                     TOTWGT_SUM = round(sum(TOTWGT),5),
                     TOTNO_SUM = round(sum(TOTNO),5),
                     TOTWGT_MEAN = round(mean(TOTWGT),5),
                     TOTNO_MEAN = round(mean(TOTNO),5),
                     TOTWGT_SE = round(Mar.utils::st_err(TOTWGT),5),
                     TOTNO_SE = round(Mar.utils::st_err(TOTNO),5),
                     TOTWGT_SQKM_STRAT_MEAN = round(mean(TOTWGT_sqkm),5),
                     TOTWGT_SQKM_STRAT_SE =  round(Mar.utils::st_err(TOTWGT_sqkm),5),
                     TOTNO_SQKM_STRAT_MEAN = round(mean(TOTNO_sqkm),5),
                     TOTNO_SQKM_STRAT_SE =  round(Mar.utils::st_err(TOTNO_sqkm),5),
                     BIOMASS_SE= round(Mar.utils::st_err(BIOMASS_set),5),
                     ABUNDANCE_SE= round(Mar.utils::st_err(ABUNDANCE_set),5),
                     .groups = "keep") |>
    dplyr::mutate(BIOMASS = round(TOTWGT_SQKM_STRAT_MEAN * AREA_KM,5),
                  ABUNDANCE = round(TOTNO_SQKM_STRAT_MEAN * AREA_KM,5)) |>
    as.data.frame()

  results$stratified_byStrat <- df

  AREA_KM_OVERALL <- sum(df$AREA_KM, na.rm = T)
  COUNT_OVERALL   <- sum(df$COUNT, na.rm = T)
  TOTWGT_OVERALL  <- sum(df$TOTWGT_SUM, na.rm = T)
  TOTNO_OVERALL   <- sum(df$TOTNO_SUM, na.rm = T)
  
  TOTWGT_MEAN  <- calcYearSummary(df, valueField = "TOTWGT_MEAN", seField = "TOTWGT_SE", areaField = "AREA_KM", is_mean = T)
  TOTNO_MEAN  <- calcYearSummary(df, valueField = "TOTNO_MEAN", seField = "TOTNO_SE", areaField = "AREA_KM", is_mean = T)
  TOTWGT_SQKM_MEAN  <- calcYearSummary(df, valueField = "TOTWGT_SQKM_STRAT_MEAN", seField = "TOTWGT_SQKM_STRAT_SE", areaField = "AREA_KM", is_mean = T)
  TOTNO_SQKM_MEAN  <- calcYearSummary(df, valueField = "TOTNO_SQKM_STRAT_MEAN", seField = "TOTNO_SQKM_STRAT_SE", areaField = "AREA_KM", is_mean = T)
  
  BIOMASS_OVERALL <- calcYearSummary(df, valueField = "BIOMASS", seField = "BIOMASS_SE", areaField = "AREA_KM", is_mean = F)
  ABUNDANCE_OVERALL <- calcYearSummary(df, valueField = "ABUNDANCE", seField = "ABUNDANCE_SE", areaField = "AREA_KM", is_mean = F)
  
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
    BIOMASS = round(BIOMASS_OVERALL$value,0),
    BIOMASS_SE = round(BIOMASS_OVERALL$se,1),
    BIOMASS_LOW = round(BIOMASS_OVERALL$low,1),
    BIOMASS_HIGH = round(BIOMASS_OVERALL$high,1),
    ABUNDANCE = round(ABUNDANCE_OVERALL$value,0),
    ABUNDANCE_SE = round(ABUNDANCE_OVERALL$se,1),
    ABUNDANCE_LOW = round(ABUNDANCE_OVERALL$low,1),
    ABUNDANCE_HIGH = round(ABUNDANCE_OVERALL$high,1)
  )
  results$OVERALL_SUMMARY <- overall
  return(results)
}




