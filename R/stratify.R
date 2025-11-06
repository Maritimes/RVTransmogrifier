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
  df[is.na(df$SIZE_CLASS), "SIZE_CLASS"] <- 1

  df <- correctForTowDist(df, col = "TOTWGT", towDist = 1.75, distCol = "DIST")
  df <- add_per_sqkm_metrics(df, df)
  df$BIOMASS_set <- df$TOTWGT_sqkm * df$AREA_KM
  df$ABUNDANCE_set <- df$TOTNO_sqkm * df$AREA_KM

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


library(dplyr)
library(tidyr)

calculate_totals <- function(gsdet_df, gsinf_df, by_sex = FALSE) {
  
  all_sets <- gsinf_df |>
    filter(TYPE == 1) |>
    select(MISSION, SETNO) |>
    distinct()
  
  spec_only <- gsdet_df |>
    select(SPEC) |>
    distinct()
  
  base_groups <- c("MISSION", "SETNO", "SPEC")
  
  if (by_sex) {
    base_groups <- c(base_groups, "FSEX")
  }
  
  length_total <- if (any(!is.na(gsdet_df$FLEN))) {
    length_data <- gsdet_df |>
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
  
  age_total <- if (any(!is.na(gsdet_df$AGE))) {
    age_data <- gsdet_df |>
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
    length_total = length_total,
    age_total = age_total
  ))
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

standardize_catch_counts <- function(tblList) {
  weight_data <- tblList$GSCAT |>
    select(MISSION, SETNO, SPEC, SIZE_CLASS, SAMPWGT, TOTWGT)
  
  dist_data <- tblList$GSINF |>
    select(MISSION, SETNO, DIST, GEAR, WINGSPREAD_FT)
  
  standardized_data <- tblList$GSDET |>
    left_join(weight_data, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")) |>
    left_join(dist_data, by = c("MISSION", "SETNO")) |>
    mutate(
      weight_ratio = case_when(
        is.na(TOTWGT) | is.na(SAMPWGT) | TOTWGT == 0 | SAMPWGT == 0 ~ 1,
        TRUE ~ TOTWGT / SAMPWGT
      ),
      CLEN = CLEN * weight_ratio,
      CAGE = if_else(!is.na(AGE), CLEN, NA_real_)
    ) |>
    select(-weight_ratio, -SAMPWGT, -TOTWGT)
  
  #join standardized_data to dist_data here so all sets are represented??  
  return(standardized_data)
}

stratify_length_age <- function(length_total, age_total, strat_data) {
  
  strat_lookup <- strat_data$stratified_bySet |>
    select(MISSION, SETNO, STRAT, AREA_KM) |>
    distinct()
  
  if (inherits(length_total, "data.frame")) {
    length_set <- length_total |>
      left_join(strat_lookup, by = c("MISSION", "SETNO"))
    
    all_combos <- length_set |>
      group_by(STRAT, SPEC) |>
      summarise(
        sets = list(unique(paste(MISSION, SETNO, sep = "_"))),
        lengths = list(unique(FLEN)),
        AREA_KM = first(AREA_KM),
        .groups = "drop"
      ) |>
      rowwise() |>
      mutate(grid = list(expand.grid(SET_ID = sets, FLEN = lengths))) |>
      select(-sets, -lengths) |>
      tidyr::unnest(grid) |>
      tidyr::separate(SET_ID, into = c("MISSION", "SETNO"), sep = "_", convert = TRUE)
    
    length_complete <- all_combos |>
      left_join(
        length_set |> select(MISSION, SETNO, FLEN, CLEN_TOTAL, CLEN_SQKM_TOTAL),
        by = c("MISSION", "SETNO", "FLEN")
      ) |>
      mutate(
        CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
        CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
      )
    
    length_strat <- length_complete |>
      group_by(SPEC, STRAT, AREA_KM, FLEN) |>
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
        CLEN_STRAT_TOTAL = CLEN_SQKM_MEAN * AREA_KM
      ) |>
      select(-CLEN_values, -CLEN_SQKM_values)
  } else {
    length_set <- NULL
    length_strat <- NULL
  }
  
  if (inherits(age_total, "data.frame")) {
    age_set <- age_total |>
      left_join(strat_lookup, by = c("MISSION", "SETNO"))
    
    all_combos <- age_set |>
      group_by(STRAT, SPEC) |>
      summarise(
        sets = list(unique(paste(MISSION, SETNO, sep = "_"))),
        ages = list(unique(AGE)),
        AREA_KM = first(AREA_KM),
        .groups = "drop"
      ) |>
      rowwise() |>
      mutate(grid = list(expand.grid(SET_ID = sets, AGE = ages))) |>
      select(-sets, -ages) |>
      tidyr::unnest(grid) |>
      tidyr::separate(SET_ID, into = c("MISSION", "SETNO"), sep = "_", convert = TRUE)
    
    age_complete <- all_combos |>
      left_join(
        age_set |> select(MISSION, SETNO, AGE, CAGE_TOTAL, CAGE_SQKM_TOTAL),
        by = c("MISSION", "SETNO", "AGE")
      ) |>
      mutate(
        CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
        CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
      )
    
    age_strat <- age_complete |>
      group_by(SPEC, STRAT, AREA_KM, AGE) |>
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
        CAGE_STRAT_TOTAL = CAGE_SQKM_MEAN * AREA_KM
      ) |>
      select(-CAGE_values, -CAGE_SQKM_values)
  } else {
    age_set <- NULL
    age_strat <- NULL
  }
  
  return(list(
    length_set = length_set,
    length_strat = length_strat,
    age_set = age_set,
    age_strat = age_strat
  ))
}

