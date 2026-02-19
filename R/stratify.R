
#' @title stratify_simple
#' @description Calculate stratified estimates of biomass and abundance from RV survey data. This function handles taxa-level or species-level data without detailed length/age information. It standardizes catches to a common tow distance, calculates per-unit-area densities, and generates stratified and overall summary statistics with confidence intervals.
#' @param tblList the default is \code{NULL}. A list of RV dataframes. If provided, data will be flattened using easyFlatten().
#' @param df the default is \code{NULL}. A data frame containing pre-flattened RV data. Used if tblList is NULL.  The df must contain the following fields:
#' \itemize{
#'   \item \code{SPEC} or \code{TAXA_}
#'   \item \code{DIST}
#'   \item \code{WINGSPREAD_FT}
#'   \item \code{TOTNO}
#'   \item \code{TOTWGT}
#'   \item \code{AREA_KM2} (or column specified by \code{areaField})
#'   \item \code{STRAT}
#'   \item \code{MISSION}
#'   \item \code{SETNO}
#' }
#' @param towDist_NM the default is \code{1.75}. The standard tow distance in nautical miles used for standardization.
#' @param areaField the default is \code{"AREA_KM2"}. The name of the field containing area values.
#' @param areaFieldUnits the default is \code{c("KM2","NM2")}. The units of the area field. Must be either "KM2" or "NM2".
#' @param debug the default is \code{FALSE}. If TRUE, additional diagnostic information is printed.
#' @return A list containing three elements: stratified_bySet (set-level calculations), stratified_byStrat (strata-level summaries), and OVERALL_SUMMARY (overall statistics with confidence intervals).
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr select mutate group_by summarise filter distinct all_of .data n
#' @importFrom tidyr crossing
#' @note This function should be used for taxa-level data or when GSDET has no records. For species-level data with length/age information, use stratify_detailed() instead.
#' @export

stratify_simple <- function(tblList=NULL, df=NULL, towDist_NM = 1.75, areaField = "AREA_KM2", areaFieldUnits= c("KM2","NM2"), debug=F){
  if(!is.null(tblList)){
    df <- easyFlatten(tblList)
    df <- df[df$TYPE==1,]
  }else{
    areaFieldUnits <- match.arg(areaFieldUnits)
    if (!any(c("SPEC", "TAXA_") %in% names(df))) stop("Either SPEC or TAXA_ must exist in the data frame")
    if (!"DIST" %in% names(df)) stop("Column DIST not found in data frame")
    if (!"WINGSPREAD_FT" %in% names(df)) stop("Column WINGSPREAD_FT not found in data frame")
    if (!"TOTNO" %in% names(df)) stop("Column TOTNO not found in data frame")
    if (!"TOTWGT" %in% names(df)) stop("Column TOTWGT not found in data frame")
    if (!areaField %in% names(df)) stop(sprintf("Column %s not found in data frame", areaField))
    if (!"STRAT" %in% names(df)) stop("Column STRAT not found in data frame")
    if (!"MISSION" %in% names(df)) stop("Column MISSION not found in data frame")
    if (!"SETNO" %in% names(df)) stop("Column SETNO not found in data frame")
    if (areaFieldUnits == "NM2") {
      df$AREA_KM2 <- sqNMToSqKm(field = df[[areaField]])
    }else{
      df$AREA_KM2<- df[[areaField]]
    }
  }
  if ("SPEC" %in% names(df) & length(unique(df$SPEC)==1)){
    df$SPEC[is.na(df$SPEC)] <- unique(df$SPEC[!is.na(df$SPEC)])
    #df$SPEC[is.na(df$SPEC)] <- unique(df$SPEC[!is.na(df$SPEC)])
  } else if ("TAXA_" %in% names(df) & length(unique(df$SPEC)==1)){
    df$TAXA_[is.na(df$TAXA_)] <- unique(df$TAXA_[!is.na(df$TAXA_)])
    # df$TAXA_[is.na(df$TAXA_)] <- unique(df$TAXA_[!is.na(df$TAXA_)])
  } else{
    stop("This analytic can only be used on a single SPEC or taxonomic group at a time")
  }

  df[is.na(df$DIST), "DIST"] <- towDist_NM
  df[is.na(df$TOTNO), "TOTNO"] <- 0
  df[is.na(df$TOTWGT), "TOTWGT"] <- 0
  
  df$TOTWGT_sqkm <- valPerSqKm(df$TOTWGT, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$TOTNO_sqkm <- valPerSqKm(df$TOTNO, towDist_NM = df$DIST, netWidth_ft = df$WINGSPREAD_FT)
  df$BIOMASS_set <- df$TOTWGT_sqkm * df$AREA_KM2
  df$ABUNDANCE_set <- df$TOTNO_sqkm * df$AREA_KM2
  
  species_col <- if ("SPEC" %in% names(df)) "SPEC" else if ("TAXA_" %in% names(df)) "TAXA_" else NULL
  setRes <- df |> 
    select(
      all_of(species_col),  
      MISSION, SETNO, STRAT, AREA_KM2, 
      TOTWGT, TOTWGT_sqkm, TOTNO, TOTNO_sqkm
    ) |> 
    arrange(STRAT, SETNO)
  
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
    mutate(YEAR = as.integer(substr(MISSION, 4, 7))) |>
    #group_by(.data[[species_col]], MISSION, STRAT, AREA_KM2) |>
    group_by(.data[[species_col]], YEAR, STRAT, AREA_KM2) |>
    summarise(COUNT = n(),
              TOTWGT_SUM = round(sum(TOTWGT), 5),
              TOTNO_SUM = round(sum(TOTNO), 5),
              TOTWGT_MEAN = round(mean(TOTWGT), 5),
              TOTNO_MEAN = round(mean(TOTNO), 5),
              TOTWGT_MEAN_SE = round(Mar.utils::st_err(TOTWGT), 5),
              TOTNO_MEAN_SE = round(Mar.utils::st_err(TOTNO), 5),
              TOTWGT_SQKM_STRAT_MEAN = round(mean(TOTWGT_sqkm), 5),
              TOTWGT_SQKM_STRAT_MEAN_SE = round(Mar.utils::st_err(TOTWGT_sqkm), 5),
              TOTNO_SQKM_STRAT_MEAN = round(mean(TOTNO_sqkm), 5),
              TOTNO_SQKM_STRAT_MEAN_SE = round(Mar.utils::st_err(TOTNO_sqkm), 5),
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
  
  TOTWGT_MEAN <- calcYearSummary(df_strat, valueField = "TOTWGT_MEAN", seField = "TOTWGT_MEAN_SE", areaField = "AREA_KM2", is_mean = TRUE)
  TOTNO_MEAN <- calcYearSummary(df_strat, valueField = "TOTNO_MEAN", seField = "TOTNO_MEAN_SE", areaField = "AREA_KM2", is_mean = TRUE)
  TOTWGT_SQKM_MEAN <- calcYearSummary(df_strat, valueField = "TOTWGT_SQKM_STRAT_MEAN", seField = "TOTWGT_SQKM_STRAT_MEAN_SE", areaField = "AREA_KM2", is_mean = TRUE)
  TOTNO_SQKM_MEAN <- calcYearSummary(df_strat, valueField = "TOTNO_SQKM_STRAT_MEAN", seField = "TOTNO_SQKM_STRAT_MEAN_SE", areaField = "AREA_KM2", is_mean = TRUE)
  BIOMASS_OVERALL <- calcYearSummary(df_strat, valueField = "BIOMASS", seField = "BIOMASS_SE", areaField = "AREA_KM2", is_mean = FALSE)
  ABUNDANCE_OVERALL <- calcYearSummary(df_strat, valueField = "ABUNDANCE", seField = "ABUNDANCE_SE", areaField = "AREA_KM2", is_mean = FALSE)
  
  overall <- data.frame(
    AREA_KM_OVERALL = AREA_KM_OVERALL,
    COUNT_OVERALL = COUNT_OVERALL,
    TOTWGT_OVERALL = TOTWGT_OVERALL,
    TOTNO_OVERALL = TOTNO_OVERALL,
    TOTWGT_MEAN = TOTWGT_MEAN$value,
    TOTWGT_MEAN_SE = TOTWGT_MEAN$se,
    TOTNO_MEAN = TOTNO_MEAN$value,
    TOTNO_MEAN_SE = TOTNO_MEAN$se,
    TOTWGT_SQKM_MEAN = TOTWGT_SQKM_MEAN$value,
    TOTWGT_SQKM_MEAN_SE = TOTWGT_SQKM_MEAN$se,
    TOTNO_SQKM_MEAN = TOTNO_SQKM_MEAN$value,
    TOTNO_SQKM_MEAN_SE = TOTNO_SQKM_MEAN$se,
    BIOMASS = round(BIOMASS_OVERALL$value, 0),
    BIOMASS_SE = round(BIOMASS_OVERALL$se, 1),
    ABUNDANCE = round(ABUNDANCE_OVERALL$value, 0),
    ABUNDANCE_SE = round(ABUNDANCE_OVERALL$se, 1)
    
  )
  
  strat_stratified  <- df_strat  |>
    dplyr::add_row(
      SPEC = NA,
      STRAT = "TOTAL",
      AREA_KM2 = overall$AREA_KM_OVERALL,
      COUNT = overall$COUNT_OVERALL,
      TOTWGT_MEAN = overall$TOTWGT_MEAN,
      TOTNO_MEAN = overall$TOTNO_MEAN,
      TOTWGT_MEAN_SE = overall$TOTWGT_MEAN_SE,
      TOTNO_MEAN_SE = overall$TOTNO_MEAN_SE,
      TOTWGT_SQKM_STRAT_MEAN = overall$TOTWGT_SQKM_MEAN,
      TOTWGT_SQKM_STRAT_MEAN_SE = overall$TOTWGT_SQKM_MEAN_SE,
      TOTNO_SQKM_STRAT_MEAN = overall$TOTNO_SQKM_MEAN,
      TOTNO_SQKM_STRAT_MEAN_SE = overall$TOTNO_SQKM_MEAN_SE,
      BIOMASS = overall$BIOMASS,
      BIOMASS_SE = overall$BIOMASS_SE,
      ABUNDANCE = overall$ABUNDANCE,
      ABUNDANCE_SE = overall$ABUNDANCE_SE
    ) |> 
    select(SPEC, STRAT, YEAR, AREA_KM2, COUNT, 
           TOTWGT_MEAN, TOTWGT_MEAN_SE, TOTWGT_SQKM_STRAT_MEAN, TOTNO_SQKM_STRAT_MEAN_SE, BIOMASS, BIOMASS_SE,
           TOTNO_MEAN, TOTNO_MEAN_SE, TOTNO_SQKM_STRAT_MEAN, TOTNO_SQKM_STRAT_MEAN_SE, ABUNDANCE, ABUNDANCE_SE)
  
  results <- list() 
  results$overall <- strat_stratified
  results$set_stratified <- setRes
  
  return(results)
}

#' @title stratify_detailed
#' @description Calculate detailed stratified estimates of biomass and abundance from RV survey data including length and age distributions. This function extends stratify_simple by incorporating detailed catch-at-length and catch-at-age data from GSDET, standardizing individual measurements, and generating stratified summaries by length and age classes.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, and GSDET.
#' @param towDist the default is \code{1.75}. The standard tow distance in nautical miles used for standardization.
#' @param by_sex the default is \code{FALSE}. If TRUE, calculations are grouped by sex (FSEX) in addition to other grouping variables.
#' @param bin_size the default is \code{1}.
#' @return A list containing up to seven elements: stratified_bySet, stratified_byStrat, OVERALL_SUMMARY (from stratify_simple), plus length_set, length_strat (length-based summaries), age_set, and age_strat (age-based summaries). Length and age elements are only included if corresponding data exist.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr select left_join group_by summarise distinct contains bind_rows rename_with count
#' @importFrom tidyr crossing full_seq
#' @export

stratify_detailed <- function(tblList, towDist = 1.75, by_sex = FALSE, conf_limits = 95, inc_limits =T, bin_size=1) {
  
  stratSimp <- stratify_simple(tblList = tblList, towDist_NM = towDist, conf_limits = conf_limits, inc_limits = inc_limits)
  
  totals <- standardize_catch_counts(tblList, towDist = towDist, by_sex = by_sex)
  
  all_sets <- stratSimp$set_stratified |>
    select(MISSION, SETNO, STRAT, AREA_KM2) |>
    distinct()
  
  spec_only <- stratSimp$set_stratified |>
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
    
    
    
    lenSet_mean <- widen_data(length_complete,var_col = "FLEN", value_col = "CLEN_TOTAL", bin_size = bin_size,level = "set", by_sex = by_sex) 
    lenStrat_mean <- widen_data(length_strat, var_col = "FLEN",value_col = "CLEN_MEAN", bin_size = bin_size,level = "strat", by_sex = by_sex)
    lenStrat_se <- widen_data(length_strat,var_col = "FLEN", value_col = "CLEN_SE", bin_size = bin_size,level = "strat", by_sex = by_sex)
    lenStrat_total <- widen_data(length_strat,var_col = "FLEN", value_col = "CLEN_STRAT_TOTAL", bin_size = bin_size,level = "strat", by_sex = by_sex)
    lenStrat_total_LENGTHS <- widen_data(length_strat,var_col = "FLEN", value_col = "CLEN_STRAT_TOTAL", bin_size = bin_size,level = "strat", by_sex = F)
    lenStrat_total_se <- widen_data(length_strat, var_col = "FLEN",value_col = "CLEN_STRAT_TOTAL_SE", bin_size = bin_size,level = "strat", by_sex = by_sex)
    
    
    
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
        FLEN_NUM = as.integer(sub(".*_(\\d+)$", "\\1", FLEN_col)),
        SEX_ORDER = case_when(
          SEX == "" ~ 0,
          SEX == "U" ~ 1,
          SEX == "M" ~ 2,
          SEX == "F" ~ 3
        )
      ) |>
      arrange(SEX_ORDER, FLEN_NUM) |>
      select(FLEN_col, LENGTH_MEAN, LENGTH_MEAN_SE, LENGTH_TOTAL, LENGTH_TOTAL_SE)

    # handle flen summaries
    flen_mean_vals <- summary_flen |>
      dplyr::select(FLEN_col, LENGTH_MEAN ) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_MEAN ) |>
      as.list()
    
    flen_mean_se_vals <- summary_flen |>
      dplyr::select(FLEN_col, LENGTH_MEAN_SE ) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_MEAN_SE ) |>
      as.list()
    
    flen_total_vals <- summary_flen |>
      dplyr::select(FLEN_col, LENGTH_TOTAL) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_TOTAL) |>
      as.list()
    
    flen_total_se_vals <- summary_flen |>
      dplyr::select(FLEN_col, LENGTH_TOTAL_SE ) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_TOTAL_SE ) |>
      as.list()
    
    lenStrat_mean <- lenStrat_mean |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!flen_mean_vals
      )
    
    lenStrat_se <- lenStrat_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!flen_mean_se_vals
      )
    
    lenStrat_total <- lenStrat_total |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!flen_total_vals
      )
    
    lenStrat_total_se <- lenStrat_total_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!flen_total_se_vals
      )
    ###
    # lenStrat_se <- lenStrat_se |>
    #   mutate(TOTAL_SE = sqrt(rowSums(dplyr::across(matches("FLEN_\\d+$"))^2)))
    # 
    # lenStrat_total_se <- lenStrat_total_se |>
    #   mutate(TOTAL_SE = sqrt(rowSums(dplyr::across(matches("FLEN_\\d+$"))^2)))
  }
  if (inherits(totals$age_total, "data.frame")) {
    agelen <- tblList$GSDET  |>
      left_join(tblList$GSINF[,c("MISSION", "SETNO", "STRAT")], by = c("MISSION", "SETNO")) |>
      select(FLEN, AGE, STRAT, !!!if (by_sex) syms("FSEX"))
    
    if (by_sex) {
      agelen <- agelen |> filter(!is.na(FLEN) & !is.na(AGE) & !is.na(FSEX))
    } else {
      agelen <- agelen |> filter(!is.na(FLEN) & !is.na(AGE))
    }
    
    alk_group_vars <- c("FLEN_BIN", "AGE")
    if (by_sex) alk_group_vars <- c(alk_group_vars, "FSEX")
    
    alk <- agelen |>
      binnit(bin_size) |>
      count(across(all_of(alk_group_vars))) |>
      tidyr::complete(
        FLEN_BIN = seq(min(FLEN_BIN), max(FLEN_BIN), by = bin_size),
        AGE,
        !!!if (by_sex) syms("FSEX"),
        fill = list(n = 0)
      ) 
    
    if (by_sex) {
      alk <- alk |>
        tidyr::pivot_wider(
          names_from = c("FSEX", "AGE"),
          values_from = n,
          values_fill = 0,
          names_glue = "{c(FSEX = c('U','M','F')[FSEX+1])}_{AGE}"
        ) |>
        arrange(FLEN_BIN) |>
        rename(FLEN = FLEN_BIN) |>
        select(FLEN, tidyselect::matches("^U_\\d+$"), 
               tidyselect::matches("^M_\\d+$"), 
               tidyselect::matches("^F_\\d+$"))
    } else {
      alk <- alk |>
        tidyr::pivot_wider(
          names_from = "AGE",
          values_from = n,
          values_fill = 0,
          names_glue = "AGE_{AGE}"
        ) |>
        arrange(FLEN_BIN) |>
        rename(FLEN = FLEN_BIN)
    }
    alk <- alk |> as.data.frame()
    
    alk_wide <- agelen |>
      count(STRAT, AGE) |>
      tidyr::pivot_wider(
        names_from = AGE,
        values_from = n,
        names_glue = "AGE_{AGE}",
        values_fill = 0
      )
    
    alw_group_vars <- c("AGE", "FLEN_BIN")
    if (by_sex) alw_group_vars <- c(alw_group_vars, "FSEX")
    alw <- totals$standardized_data |>
      binnit(bin_size) |>
      group_by(across(all_of(alw_group_vars))) |>
      summarise(FWT = mean(FWT), 
                COUNT = n(),
                .groups = "drop") |>
      complete(FLEN_BIN = full_seq(FLEN_BIN, bin_size), 
               AGE, 
               !!!if (by_sex) syms("FSEX"), 
               fill = list(FWT = 0, COUNT = 0)) |>
      mutate(FWT = FWT / 1000)
    
    if (by_sex) {
      alw <- alw |>
        dplyr::group_by(FLEN_BIN, FSEX, AGE) |>
        dplyr::summarise(FWT = sum(FWT), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = c("FSEX", "AGE"),
          values_from = "FWT",
          names_glue = "{c(FSEX = c('U','M','F')[FSEX+1])}_AGE_{AGE}"
        ) |>
        dplyr::rename(FLEN = FLEN_BIN) |>
        dplyr::select(
          FLEN,
          tidyselect::matches("^U_AGE_\\d+$"),
          tidyselect::matches("^M_AGE_\\d+$"),
          tidyselect::matches("^F_AGE_\\d+$")
        )
    } else {
      alw <- alw |>
        pivot_wider(
          names_from = AGE,
          values_from = c(FWT,COUNT),
          names_glue =  "{.value}_{AGE}"
        ) |>
        dplyr::rename(FLEN = FLEN_BIN) |>
        select(
          FLEN,
          tidyselect::matches("^(FWT)_")
        )
    }
    alw <- alw |> 
      as.data.frame()
    
    alk_ap <- alk
    
    x <- prop.table(as.matrix(alk_ap[, setdiff(names(alk_ap), "FLEN")]), 1)
    x <- ifelse(is.nan(x), 0, x)
    ages_prop <- as.data.frame(x) |> dplyr::mutate(FLEN = alk_ap$FLEN)
    lengths = colSums(lenStrat_total_LENGTHS[,4:ncol(lenStrat_total_LENGTHS)])
    lengths = as.data.frame(lengths[lengths>0])
    colnames(lengths) = "length_sum"
    lengths$FLEN <-  sub("FLEN_", "", rownames(lengths))
    row.names(lengths) <- NULL
    
    age_table<-merge(ages_prop,lengths, by="FLEN", all.x=T)
    age_table[, !(names(age_table) %in% c("FLEN", "length_sum"))]<- round(age_table[, !(names(age_table) %in% c("FLEN", "length_sum"))] * age_table[["length_sum"]],4)
    age_table[is.na(age_table)]<-0
    age_table$length_sum <- NULL
    
    age_cols <- setdiff(names(age_table), "FLEN")
    age_table_avg_lengths <- sapply(age_cols, function(age) {
      x <- age_table[[age]]
      if (sum(x) == 0) return(NA)
      sum(age_table$FLEN * x) / sum(x)
    })
    
    set_age <- widen_data(totals$age_total, var_col = "AGE", value_col = "CAGE_TOTAL", bin_size = bin_size,level = "set", by_sex = by_sex)
    
    age_complete <- totals$age_total |>
      mutate(
        CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
        CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
      )
    
    group_vars <- c("SPEC", "STRAT", "AREA_KM2", "AGE")
    if (by_sex) group_vars <- c(group_vars, "FSEX")
    age_strat <- age_complete |>
      dplyr::group_by(across(all_of(group_vars))) |>
      dplyr::group_map(~{
        tibble::tibble(
          SPEC = .y$SPEC,
          STRAT = .y$STRAT,
          AREA_KM2 = .y$AREA_KM2,
          AGE = .y$AGE,
          !!!if (by_sex) list(FSEX = .y$FSEX),
          COUNT = nrow(.x),
          CAGE_SUM = sum(.x$CAGE_TOTAL, na.rm = TRUE),
          CAGE_MEAN = mean(.x$CAGE_TOTAL, na.rm = TRUE),
          CAGE_MEAN_SE = round(Mar.utils::st_err(.x$CAGE_TOTAL), 5),
          CAGE_SQKM_TOTAL = sum(.x$CAGE_SQKM_TOTAL, na.rm = TRUE),
          CAGE_SQKM_MEAN = mean(.x$CAGE_SQKM_TOTAL, na.rm = TRUE),
          CAGE_SQKM_SE = Mar.utils::st_err(.x$CAGE_SQKM_TOTAL),
          CAGE_TOTAL = CAGE_SQKM_MEAN * .y$AREA_KM2,
          CAGE_TOTAL_SE = CAGE_SQKM_SE * .y$AREA_KM2
        )
      }) |>
      dplyr::bind_rows()
    
    strat_age_mean <- widen_data(age_strat, var_col = "AGE", value_col = "CAGE_MEAN", bin_size = bin_size,level = "strat", by_sex = by_sex) 
    strat_age_mean_se <- widen_data(age_strat, var_col = "AGE", value_col = "CAGE_MEAN_SE", bin_size = bin_size,level = "strat", by_sex = by_sex) 
    strat_age_total <- widen_data(age_strat, var_col = "AGE", value_col = "CAGE_TOTAL", bin_size = bin_size,level = "strat", by_sex = by_sex) 
    strat_age_total_se <- widen_data(age_strat, var_col = "AGE", value_col = "CAGE_TOTAL_SE", bin_size = bin_size,level = "strat", by_sex = by_sex)
    
    mean_df <- strat_age_mean |>
      left_join(
        strat_age_mean_se |>
          dplyr::select(-AREA_KM2, -SPEC) |>
          dplyr::rename_with(
            ~ sub("(.*AGE_)(\\d+)$", "\\1SE_\\2", .),
            dplyr::contains("AGE_")
          ),
        by = "STRAT"
      )
    age_cols <- grep("AGE_\\d+", names(mean_df), value = TRUE)
    se_age_cols <- grep("AGE_SE_\\d+", names(mean_df), value = TRUE)
    
    AGE_MEAN <- bind_rows(
      Map(function(x, y) {
        out <- calcYearSummary(mean_df,
                               valueField = x,
                               seField = y,
                               areaField = "AREA_KM2",
                               is_mean = TRUE)
        out$AGE_col <- x
        out |>
          select(value, se, AGE_col) |>
          rename(AGE_MEAN_SE = se,
                 AGE_MEAN = value)
      }, age_cols, se_age_cols)
    )
    
    total_df <- strat_age_total |>
      left_join(
        strat_age_total_se |>
          dplyr::select(-AREA_KM2, -SPEC) |>
          dplyr::rename_with(
            ~ sub("(.*AGE_)(\\d+)$", "\\1SE_\\2", .),
            contains("AGE_")
          ),
        by = "STRAT"
      )
    age_cols2 <- grep("AGE_\\d+", names(total_df), value = TRUE)
    se_age_cols2 <- grep("AGE_SE_\\d+", names(total_df), value = TRUE)
    
    AGE_TOTAL <- bind_rows(
      Map(function(x, y) {
        out <- calcYearSummary(total_df,
                               valueField = x,
                               seField = y,
                               areaField = "AREA_KM2",
                               is_mean = FALSE)
        out$AGE_col <- x
        out |>
          select(value, se, AGE_col) |>
          rename(AGE_TOTAL_SE = se,
                 AGE_TOTAL = value)
      }, age_cols2, se_age_cols2)
    )
    
    summary_age <- AGE_MEAN |>
      left_join(AGE_TOTAL, by = "AGE_col") |>
      mutate(
        AGE_NUM = as.integer(sub(".*_(\\d+)$", "\\1", AGE_col))
      ) |>
      arrange(AGE_NUM) |>
      select(AGE_col, AGE_MEAN, AGE_MEAN_SE, AGE_TOTAL, AGE_TOTAL_SE)
    
    # strat_age_mean_se <- strat_age_mean_se |>
    #   mutate(TOTAL_SE = sqrt(rowSums(dplyr::across(matches("AGE_\\d+$"))^2)))
    # 
    # strat_age_total_se <- strat_age_total_se |>
    #   mutate(TOTAL_SE = sqrt(rowSums(dplyr::across(matches("AGE_\\d+$"))^2)))

    # handle age summaries
    age_mean_vals <- summary_age |>
      dplyr::select(AGE_col, AGE_MEAN ) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_MEAN ) |>
      as.list()
    
    age_mean_se_vals <- summary_age |>
      dplyr::select(AGE_col, AGE_MEAN_SE ) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_MEAN_SE ) |>
      as.list()
    
    age_total_vals <- summary_age |>
      dplyr::select(AGE_col, AGE_TOTAL) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_TOTAL) |>
      as.list()
    
    age_total_se_vals <- summary_age |>
      dplyr::select(AGE_col, AGE_TOTAL_SE ) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_TOTAL_SE ) |>
      as.list()
    
    strat_age_mean <- strat_age_mean |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!age_mean_vals
      )
    
    strat_age_mean_se <- strat_age_mean_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!age_mean_se_vals
      )
    
    ageStrat_total <- strat_age_total |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!age_total_vals
      )
    
    ageStrat_total_se <- strat_age_total_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        SPEC = NA_integer_,
        !!!age_total_se_vals
      )
  }

  results <- list()
  results$summaries$overall <- stratSimp$overall
  results$setLevel$allSets <- stratSimp$set_stratified
  
  if (inherits(totals$length_total, "data.frame")) {
    # results$summaries$flen <- summary_flen
    results$setLevel$flen_mean <- lenSet_mean
    results$stratLevel$flen_mean <- lenStrat_mean
    results$stratLevel$flen_mean_se <- lenStrat_se
    results$stratLevel$flen_total <- lenStrat_total
    results$stratLevel$flen_total_se <- lenStrat_total_se
  }
  if (inherits(totals$age_total, "data.frame")) {
    # results$summaries$age <- summary_age
    results$summaries$age_length_key <- alk
    results$summaries$age_table <- age_table
    results$summaries$age_length_weight <- alw
    results$setLevel$age_set <- set_age
    #results$age_strat <- age_strat
    results$stratLevel$age_mean <- strat_age_mean
    results$stratLevel$age_mean_se <- strat_age_mean_se
    results$stratLevel$age_total <- strat_age_total
    results$stratLevel$age_total_se <- strat_age_total_se
  }
  return(results)
}