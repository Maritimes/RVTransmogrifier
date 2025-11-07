
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

  standardized_data <- tblList$GSDET |>
    left_join(weight_data, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")) |>
    left_join(dist_data, by = c("MISSION", "SETNO")) |>
    mutate(
      weight_ratio = case_when(
        is.na(TOTWGT) | is.na(SAMPWGT) | TOTWGT == 0 | SAMPWGT == 0 ~ 1,
        TRUE ~ TOTWGT / SAMPWGT
      ),
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

    all_combos <- all_sets |>
      cross_join(spec_only) |>
      cross_join(length_data |> select(FLEN) |> distinct())

    all_combos |>
      left_join(length_data, by = c("MISSION", "SETNO", "SPEC", "FLEN")) |>
      mutate(
        CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
        CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
      )
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

    all_combos <- all_sets |>
      cross_join(spec_only) |>
      cross_join(age_data |> select(AGE) |> distinct())

    all_combos |>
      left_join(age_data, by = c("MISSION", "SETNO", "SPEC", "AGE")) |>
      mutate(
        CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
        CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
      )
  } else {
    NA
  }

  return(list(
    standardized_data = standardized_data,
    length_total = length_total,
    age_total = age_total
  ))
}

stratify_simple <- function(tblList=NULL, df=NULL, towDist_NM = 1.75, areaField = "AREA_KM2", areaFieldUnits= c("KM2","NM2"), debug=F){

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

  df <- correctForTowDist(df, col = "TOTWGT", towDist = towDist_NM, distCol = "DIST")
  
  df$TOTWGT_sqkm <- valPerSqKm(df$TOTWGT, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$TOTNO_sqkm <- valPerSqKm(df$TOTNO, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$BIOMASS_set <- df$TOTWGT_sqkm * df$AREA_KM2
  df$ABUNDANCE_set <- df$TOTNO_sqkm * df$AREA_KM2
  
  results$stratified_bySet <- df
  
  species_col <- if ("SPEC" %in% names(df)) "SPEC" else if ("TAXA_" %in% names(df)) "TAXA_" else NULL
  
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
  
  results$stratified_byStrat <- df_strat
  
  AREA_KM_OVERALL <- sum(df_strat$AREA_KM2, na.rm = TRUE)
  COUNT_OVERALL <- sum(df_strat$COUNT, na.rm = TRUE)
  TOTWGT_OVERALL <- sum(df_strat$TOTWGT_SUM, na.rm = TRUE)
  TOTNO_OVERALL <- sum(df_strat$TOTNO_SUM, na.rm = TRUE)
  
  TOTWGT_MEAN <- calcYearSummary(df_strat, valueField = "TOTWGT_MEAN", seField = "TOTWGT_SE", areaField = "AREA_KM2", is_mean = TRUE)
  TOTNO_MEAN <- calcYearSummary(df_strat, valueField = "TOTNO_MEAN", seField = "TOTNO_SE", areaField = "AREA_KM2", is_mean = TRUE)
  TOTWGT_SQKM_MEAN <- calcYearSummary(df_strat, valueField = "TOTWGT_SQKM_STRAT_MEAN", seField = "TOTWGT_SQKM_STRAT_SE", areaField = "AREA_KM2", is_mean = TRUE)
  TOTNO_SQKM_MEAN <- calcYearSummary(df_strat, valueField = "TOTNO_SQKM_STRAT_MEAN", seField = "TOTNO_SQKM_STRAT_SE", areaField = "AREA_KM2", is_mean = TRUE)
  
  BIOMASS_OVERALL <- calcYearSummary(df_strat, valueField = "BIOMASS", seField = "BIOMASS_SE", areaField = "AREA_KM2", is_mean = FALSE)
  ABUNDANCE_OVERALL <- calcYearSummary(df_strat, valueField = "ABUNDANCE", seField = "ABUNDANCE_SE", areaField = "AREA_KM2", is_mean = FALSE)
  
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
  results$OVERALL_SUMMARY <- overall
  return(results)
}

stratify_detailed <- function(tblList, towDist = 1.75, by_sex = FALSE) {
  
  results <- stratify_simple(tblList = tblList, towDist_NM = towDist)
  
  totals <- standardize_catch_counts(tblList, towDist = towDist, by_sex = by_sex)
  
  strat_lookup <- results$stratified_bySet |>
    select(MISSION, SETNO, STRAT, AREA_KM2) |>
    distinct()
  
  all_sets <- results$stratified_bySet |>
    select(MISSION, SETNO, STRAT, AREA_KM2) |>
    distinct()
  
  spec_only <- results$stratified_bySet |>
    select(SPEC) |>
    distinct()
  
  if (inherits(totals$length_total, "data.frame")) {
    length_data <- totals$length_total |>
      left_join(strat_lookup, by = c("MISSION", "SETNO"))
    
    all_combos <- all_sets |>
      tidyr::crossing(spec_only) |>
      tidyr::crossing(length_data |> select(FLEN) |> distinct())
    
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
        CLEN_values = list(CLEN_TOTAL),
        CLEN_SQKM_SUM = sum(CLEN_SQKM_TOTAL, na.rm = TRUE),
        CLEN_SQKM_MEAN = mean(CLEN_SQKM_TOTAL, na.rm = TRUE),
        CLEN_SQKM_values = list(CLEN_SQKM_TOTAL),
        .groups = "drop"
      ) |>
      mutate(
        CLEN_SE = sapply(CLEN_values, Mar.utils::st_err),
        CLEN_SQKM_SE = sapply(CLEN_SQKM_values, Mar.utils::st_err),
        CLEN_STRAT_TOTAL = CLEN_SQKM_MEAN * AREA_KM2
      ) |>
      select(-CLEN_values, -CLEN_SQKM_values)
    
    results$length_set <- length_complete
    results$length_strat <- length_strat
  }
  
  if (inherits(totals$age_total, "data.frame")) {
    age_data <- totals$age_total |>
      left_join(strat_lookup, by = c("MISSION", "SETNO"))
    
    all_combos <- all_sets |>
      tidyr::crossing(spec_only) |>
      tidyr::crossing(age_data |> select(AGE) |> distinct())
    
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
