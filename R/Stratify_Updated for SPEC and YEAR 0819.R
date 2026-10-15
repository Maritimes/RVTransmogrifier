@title stratify_simple
#' @description Calculate stratified estimates of biomass and abundance from RV survey data. This function   handles
#' taxa-level or species-level data without detailed length/age information. It standardizes catches to a common tow
#' distance, calculates per-unit-area densities, and generates stratified and overall summary statistics with
#' confidence intervals.
#' @param tblList the default is \code{NULL}. A list of RV dataframes. If provided, data will be flattened using
#' easyFlatten().
#' @param df the default is \code{NULL}. A data frame containing pre-flattened RV data. Used if tblList is NULL.  The
#' df must contain the following fields:
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
#' @param areaFieldUnits The units of the area field. Must be  \code{c("KM2","NM2","M2")}. 
#' @param conf_limits the default is \code{95}.  These are the are confidence intervals that will be calculated.
#' @param totwgt_units The units of the \code{TOTWGT} field. Must be  \code{c("kg","ton")}. 
#' @param debug the default is \code{FALSE}. If TRUE, additional diagnostic information is printed.
#' @return A list containing three elements: stratified_bySet (set-level calculations), stratified_byStrat (strata-level
#' summaries), and OVERALL_SUMMARY (overall statistics with confidence intervals).
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function should be used for taxa-level data or when GSDET has no records. For species-level data with
#' length/age information, use stratify_detailed() instead.
#' @importFrom rlang .data
#' @export

stratify_simple <- function(
    tblList = NULL,
    df = NULL,
    towDist_NM = 1.75,
    netWidthUnits= c("NEST","WIIA"),
    areaField = "AREA_KM2",
    areaFieldUnits = c("KM2", "NM2", "M2"),
    conf_limits = 95,
    totwgt_units = "ton",
    debug = F
) {
  
  if (!is.null(tblList)) {
    df <- easyFlatten(tblList)
    df <- df[df$TYPE == 1, ]
    df[is.na(df$DIST), "DIST"] <- 1.75
    df[is.na(df$TOTNO), "TOTNO"] <- 0
    df[is.na(df$TOTWGT), "TOTWGT"] <- 0
    
  } else {
    # Validation checks
    if (!any(c("SPEC", "TAXA_") %in% names(df))) {
      stop("Either SPEC or TAXA_ must exist in the data frame")
    }
    required_cols <- c("DIST", "WINGSPREAD_FT", "TOTNO", "TOTWGT", "STRAT", "MISSION", "SETNO", areaField)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
    }
    
    df[is.na(df$DIST), "DIST"] <- towDist_NM
    df[is.na(df$TOTNO), "TOTNO"] <- 0
    df[is.na(df$TOTWGT), "TOTWGT"] <- 0
  }
  
  
  if(totwgt_units=="ton"){
    df$TOTWGT <- df$TOTWGT/1000
  }
  
  species_col <- if ("SPEC" %in% names(df)) {
    "SPEC"
  } else if ("TAXA_" %in% names(df)) {
    "TAXA_"
  } else {
    stop("Either SPEC or TAXA_ must exist in the data frame")
  }
  
  areaFieldUnits <- match.arg(areaFieldUnits)
  if (areaFieldUnits == "NM2") {
    df$AREA_KM2 <- sqNMToSqKm(field = df[[areaField]])
  } else if (areaFieldUnits == "M2") {
    df$AREA_KM2 <- sqMToSqKm(field = df[[areaField]])
  } else if (areaFieldUnits == "KM2") {
    df$AREA_KM2 <- df[[areaField]]
  }
  
  if (netWidthUnits == "NEST") {
    df$WINGSPREAD_FT <- 42.65
  } else if (netWidthUnits == "WIIA") {
    df$WINGSPREAD_FT <- 41
  } 
  
  df$TOTWGT_sqkm <- valPerSqKm(
    df$TOTWGT,
    towDist_NM = df$DIST,
    netWidth_ft = df$WINGSPREAD_FT
  )
  df$TOTNO_sqkm <- valPerSqKm(
    df$TOTNO,
    towDist_NM = df$DIST,
    netWidth_ft = df$WINGSPREAD_FT
  )
  df$BIOMASS_set <- df$TOTWGT_sqkm * df$AREA_KM2
  df$ABUNDANCE_set <- df$TOTNO_sqkm * df$AREA_KM2
  
  setRes <- df |>
    dplyr::select(
      dplyr::all_of(species_col),
      MISSION,
      SETNO,
      STRAT,
      AREA_KM2,
      TOTWGT,
      TOTWGT_sqkm,
      TOTNO,
      TOTNO_sqkm
    ) |>
    dplyr::arrange(STRAT, SETNO)
  
  all_sets <- df |>
    dplyr::select(MISSION, SETNO, STRAT, AREA_KM2) |>
    dplyr::distinct()
  
  spec_only <- df |>
    dplyr::select(dplyr::all_of(species_col)) |>
    dplyr::distinct()
  
  weight_grid <- all_sets |>
    tidyr::crossing(spec_only)
  
  df_complete <- weight_grid |>
    dplyr::left_join(
      df,
      by = c("MISSION", "SETNO", "STRAT", "AREA_KM2", species_col)
    ) |>
    dplyr::mutate(
      TOTWGT = ifelse(is.na(TOTWGT), 0, TOTWGT),
      TOTNO = ifelse(is.na(TOTNO), 0, TOTNO),
      TOTWGT_sqkm = ifelse(is.na(TOTWGT_sqkm), 0, TOTWGT_sqkm),
      TOTNO_sqkm = ifelse(is.na(TOTNO_sqkm), 0, TOTNO_sqkm),
      BIOMASS_set = ifelse(is.na(BIOMASS_set), 0, BIOMASS_set),
      ABUNDANCE_set = ifelse(is.na(ABUNDANCE_set), 0, ABUNDANCE_set)
    )
  
  df_strat <- df_complete |>
    dplyr::mutate(YEAR = as.integer(substr(MISSION, 4, 7))) |>
    dplyr::group_by(.data[[species_col]], YEAR, STRAT, AREA_KM2) |>
    dplyr::summarise(
      COUNT = dplyr::n(),
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
      .groups = "drop"
    ) |>
    dplyr::mutate(
      BIOMASS = round(TOTWGT_SQKM_STRAT_MEAN * AREA_KM2, 5),
      ABUNDANCE = round(TOTNO_SQKM_STRAT_MEAN * AREA_KM2, 5)
    ) |>
    as.data.frame()
  
  results_by_year <- df_strat |>
    dplyr::group_by(.data[[species_col]],YEAR) |>
    dplyr::summarise(
      AREA_KM_OVERALL = sum(AREA_KM2, na.rm = TRUE),
      COUNT_OVERALL = sum(COUNT, na.rm = TRUE),
      TOTWGT_OVERALL = sum(TOTWGT_SUM, na.rm = TRUE),
      TOTNO_OVERALL = sum(TOTNO_SUM, na.rm = TRUE),
      
      TOTWGT_MEAN_RESULT = list(calcYearSummary(
        theDataByStrat = dplyr::pick(dplyr::everything()),
        valueField = "TOTWGT_MEAN",
        seField = "TOTWGT_MEAN_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = TRUE
      )),
      TOTNO_MEAN_RESULT = list(calcYearSummary(
        theDataByStrat = dplyr::pick(dplyr::everything()),
        valueField = "TOTNO_MEAN",
        seField = "TOTNO_MEAN_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = TRUE
      )),
      TOTWGT_SQKM_STRAT_MEAN_RESULT = list(calcYearSummary(
        theDataByStrat = dplyr::pick(dplyr::everything()),
        valueField = "TOTWGT_SQKM_STRAT_MEAN",
        seField = "TOTWGT_SQKM_STRAT_MEAN_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = TRUE
      )),
      TOTNO_SQKM_STRAT_MEAN_RESULT = list(calcYearSummary(
        theDataByStrat =  dplyr::pick(dplyr::everything()),
        valueField = "TOTNO_SQKM_STRAT_MEAN",
        seField = "TOTNO_SQKM_STRAT_MEAN_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = TRUE
      )),
      BIOMASS_RESULT = list(calcYearSummary(
        theDataByStrat = dplyr::pick(dplyr::everything()),
        valueField = "BIOMASS",
        seField = "BIOMASS_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = FALSE
      )),
      ABUNDANCE_RESULT = list(calcYearSummary(
        theDataByStrat = dplyr::pick(dplyr::everything()),
        valueField = "ABUNDANCE",
        seField = "ABUNDANCE_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = FALSE
      )),
      .groups = "drop"
    ) |>
    # Extract values from the list columns into individual columns
    dplyr::mutate(
      TOTWGT_MEAN = purrr::map_dbl(TOTWGT_MEAN_RESULT, "value"),
      TOTWGT_MEAN_SE = purrr::map_dbl(TOTWGT_MEAN_RESULT, "se"),
      TOTWGT_MEAN_LOW = purrr::map_dbl(TOTWGT_MEAN_RESULT, "low"),
      TOTWGT_MEAN_HIGH = purrr::map_dbl(TOTWGT_MEAN_RESULT, "high"),
      TOTNO_MEAN = purrr::map_dbl(TOTNO_MEAN_RESULT, "value"),
      TOTNO_MEAN_SE = purrr::map_dbl(TOTNO_MEAN_RESULT, "se"),
      TOTNO_MEAN_LOW = purrr::map_dbl(TOTNO_MEAN_RESULT, "low"),
      TOTNO_MEAN_HIGH = purrr::map_dbl(TOTNO_MEAN_RESULT, "high"),
      TOTWGT_SQKM_STRAT_MEAN = purrr::map_dbl(TOTWGT_SQKM_STRAT_MEAN_RESULT, "value"),
      TOTWGT_SQKM_STRAT_MEAN_SE = purrr::map_dbl(TOTWGT_SQKM_STRAT_MEAN_RESULT, "se"),
      TOTWGT_SQKM_STRAT_MEAN_LOW = purrr::map_dbl(TOTWGT_SQKM_STRAT_MEAN_RESULT, "low"),
      TOTWGT_SQKM_STRAT_MEAN_HIGH = purrr::map_dbl(TOTWGT_SQKM_STRAT_MEAN_RESULT, "high"),
      TOTNO_SQKM_STRAT_MEAN = purrr::map_dbl(TOTNO_SQKM_STRAT_MEAN_RESULT, "value"),
      TOTNO_SQKM_STRAT_MEAN_SE = purrr::map_dbl(TOTNO_SQKM_STRAT_MEAN_RESULT, "se"),
      TOTNO_SQKM_STRAT_MEAN_LOW = purrr::map_dbl(TOTNO_SQKM_STRAT_MEAN_RESULT, "low"),
      TOTNO_SQKM_STRAT_MEAN_HIGH = purrr::map_dbl(TOTNO_SQKM_STRAT_MEAN_RESULT, "high"),
      BIOMASS = purrr::map_dbl(BIOMASS_RESULT, "value"),
      BIOMASS_SE = purrr::map_dbl(BIOMASS_RESULT, "se"),
      BIOMASS_LOW = purrr::map_dbl(BIOMASS_RESULT, "low"),
      BIOMASS_HIGH = purrr::map_dbl(BIOMASS_RESULT, "high"),
      ABUNDANCE = purrr::map_dbl(ABUNDANCE_RESULT, "value"),
      ABUNDANCE_SE = purrr::map_dbl(ABUNDANCE_RESULT, "se"),
      ABUNDANCE_LOW = purrr::map_dbl(ABUNDANCE_RESULT, "low"),
      ABUNDANCE_HIGH = purrr::map_dbl(ABUNDANCE_RESULT, "high")
      
    ) |>
    dplyr::select(-c(TOTWGT_MEAN_RESULT, TOTNO_MEAN_RESULT, TOTWGT_SQKM_STRAT_MEAN_RESULT, TOTNO_SQKM_STRAT_MEAN_RESULT, BIOMASS_RESULT, ABUNDANCE_RESULT))
  
  # Step 2: Calculate overall metrics for the whole dataset
  overall_results <- df_strat |>
    dplyr::group_by(.data[[species_col]]) |> 
    dplyr::summarise(
      AREA_KM_OVERALL = sum(AREA_KM2, na.rm = TRUE),
      COUNT_OVERALL = sum(COUNT, na.rm = TRUE),
      TOTWGT_OVERALL = sum(TOTWGT_SUM, na.rm = TRUE),
      TOTNO_OVERALL = sum(TOTNO_SUM, na.rm = TRUE),
      # Call calcYearSummary once for TOTWGT_MEAN
      TOTWGT_MEAN_RESULT = list(calcYearSummary(
        theDataByStrat =  dplyr::pick(dplyr::everything()),
        valueField = "TOTWGT_MEAN",
        seField = "TOTWGT_MEAN_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = TRUE
      )),
      # Call calcYearSummary once for TOTNO_MEAN
      TOTNO_MEAN_RESULT = list(calcYearSummary(
        theDataByStrat =  dplyr::pick(dplyr::everything()),
        valueField = "TOTNO_MEAN",
        seField = "TOTNO_MEAN_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = TRUE
      )),
      # Call calcYearSummary once for BIOMASS
      BIOMASS_RESULT = list(calcYearSummary(
        theDataByStrat =  dplyr::pick(dplyr::everything()),
        valueField = "BIOMASS",
        seField = "BIOMASS_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = FALSE
      )),
      # Call calcYearSummary once for ABUNDANCE
      ABUNDANCE_RESULT = list(calcYearSummary(
        theDataByStrat =  dplyr::pick(dplyr::everything()),
        valueField = "ABUNDANCE",
        seField = "ABUNDANCE_SE",
        areaField = "AREA_KM2",
        level = conf_limits / 100,
        is_mean = FALSE
      )),
      .groups = "drop"
    ) |>
    # Extract values from each list column into separate flat columns
    dplyr::mutate(
      TOTWGT_MEAN = purrr::map_dbl(TOTWGT_MEAN_RESULT, "value"),
      TOTWGT_MEAN_SE = purrr::map_dbl(TOTWGT_MEAN_RESULT, "se"),
      TOTWGT_MEAN_LOW = purrr::map_dbl(TOTWGT_MEAN_RESULT, "low"),
      TOTWGT_MEAN_HIGH = purrr::map_dbl(TOTWGT_MEAN_RESULT, "high"),
      TOTNO_MEAN = purrr::map_dbl(TOTNO_MEAN_RESULT, "value"),
      TOTNO_MEAN_SE = purrr::map_dbl(TOTNO_MEAN_RESULT, "se"),
      TOTNO_MEAN_LOW = purrr::map_dbl(TOTNO_MEAN_RESULT, "low"),
      TOTNO_MEAN_HIGH = purrr::map_dbl(TOTNO_MEAN_RESULT, "high"),
      BIOMASS = purrr::map_dbl(BIOMASS_RESULT, "value"),
      BIOMASS_SE = purrr::map_dbl(BIOMASS_RESULT, "se"),
      BIOMASS_LOW = purrr::map_dbl(BIOMASS_RESULT, "low"),
      BIOMASS_HIGH = purrr::map_dbl(BIOMASS_RESULT, "high"),
      ABUNDANCE = purrr::map_dbl(ABUNDANCE_RESULT, "value"),
      ABUNDANCE_SE = purrr::map_dbl(ABUNDANCE_RESULT, "se"),
      ABUNDANCE_LOW = purrr::map_dbl(ABUNDANCE_RESULT, "low"),
      ABUNDANCE_HIGH = purrr::map_dbl(ABUNDANCE_RESULT, "high")
    ) |>
    # Drop the intermediate list columns to tidy up the final output
    dplyr::select(
      -c(TOTWGT_MEAN_RESULT, TOTNO_MEAN_RESULT, BIOMASS_RESULT, ABUNDANCE_RESULT)
    )
  
  species_list <- unique(df[[species_col]])
  
  results_by_species <- lapply(species_list, function(sp) {
    list(
      stratified_by_set = setRes[setRes[[species_col]] == sp, ],
      stratified_by_strat = df_strat[df_strat[[species_col]] == sp, ],
      results_by_year = results_by_year[results_by_year[[species_col]] == sp, ],
      stratified_summary = overall_results[overall_results[[species_col]] == sp, ]
    )
  })
  
  # Name the list elements by species code
  names(results_by_species) <- paste0(species_col, "_", species_list)
  
  return(results_by_species)
}

#' @title stratify_detailed
#' @description Calculate detailed stratified estimates of biomass and abundance from RV survey data including length and age distributions. This function extends stratify_simple by incorporating detailed catch-at-length and catch-at-age data from GSDET, standardizing individual measurements, and generating stratified summaries by length and age classes.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, and GSDET.
#' @param towDist the default is \code{1.75}. The standard tow distance in nautical miles used for standardization.
#' @param by_sex the default is \code{FALSE}. If TRUE, calculations are grouped by sex (FSEX) in addition to other grouping variables.
#' @param conf_limits  the default is \code{95}
#' @param inc_limits  the default is \code{TRUE}
#' @param bin_size the default is \code{1}.
#' @return A list containing up to seven elements: stratified_bySet, stratified_byStrat, OVERALL_SUMMARY (from stratify_simple), plus length_set, length_strat (length-based summaries), age_set, and age_strat (age-based summaries). Length and age elements are only included if corresponding data exist.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export

stratify_detailed <- function(
    tblList,
    towDist = 1.75,
    netWidthUnits = "WIIA",
    by_sex = FALSE,
    conf_limits = 95,
    inc_limits = T,
    bin_size = 1,
    Converted_Data=TRUE
) {
  stratSimp <- stratify_simple(
    tblList = tblList,
    towDist_NM = towDist,
    netWidthUnits = netWidthUnits ,
    conf_limits = conf_limits
  )
  
  totals <- standardize_catch_counts(
    tblList,
    towDist = towDist,
    netWidthUnits = netWidthUnits,
    by_sex = by_sex,
    Converted_Data = Converted_Data
  )
  df<-easyFlatten(tblList)
  all_sets <- df |>
    dplyr::select(MISSION, SETNO, STRAT, AREA_KM2) |>
    dplyr::distinct()
  
  spec_only <- df |>
    dplyr::select(SPEC) |>
    dplyr::distinct()
  
  totals$length_total$YEAR<-as.numeric(substr(totals$length_total$MISSION, 4, 7))
  totals$age_total$YEAR<-as.numeric(substr(totals$age_total$MISSION, 4, 7))
  totals$standardized_data$YEAR<-as.numeric(substr(totals$standardized_data$MISSION, 4, 7))
  
  if (inherits(totals$length_total, "data.frame")) {
    length_data <- totals$length_total
    
    if (by_sex) {
      all_combos <- all_sets |>
        tidyr::crossing(spec_only) |>
        tidyr::crossing(
          length_data |> dplyr::select(FLEN) |> dplyr::distinct(),
          length_data |> dplyr::select(FSEX) |> dplyr::distinct()
        )
      
      length_complete <- all_combos |>
        dplyr::left_join(
          length_data |>
            dplyr::select(
              MISSION,
              SETNO,
              SPEC,
              FLEN,
              FSEX,
              CLEN_TOTAL,
              CLEN_SQKM_TOTAL,
              YEAR
            ),
          by = c("MISSION", "SETNO", "SPEC", "FSEX", "FLEN")
        ) |>
        dplyr::mutate(
          CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
          CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
        )
      
      length_strat <- length_complete |>
        dplyr::group_by(YEAR, SPEC, STRAT, AREA_KM2, FSEX, FLEN) |>
        dplyr::summarise(
          COUNT = dplyr::n(),
          CLEN_SUM = sum(CLEN_TOTAL, na.rm = TRUE),
          CLEN_MEAN = mean(CLEN_TOTAL, na.rm = TRUE),
          CLEN_MEAN_SE = round(Mar.utils::st_err(CLEN_TOTAL), 5),
          CLEN_values = list(CLEN_TOTAL),
          CLEN_SQKM_SUM = sum(CLEN_SQKM_TOTAL, na.rm = TRUE),
          CLEN_SQKM_MEAN = mean(CLEN_SQKM_TOTAL, na.rm = TRUE),
          CLEN_SQKM_values = list(CLEN_SQKM_TOTAL),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          CLEN_SE = sapply(CLEN_values, Mar.utils::st_err),
          CLEN_SQKM_SE = sapply(CLEN_SQKM_values, Mar.utils::st_err),
          CLEN_STRAT_TOTAL = CLEN_SQKM_MEAN * AREA_KM2,
          CLEN_STRAT_TOTAL_SE = CLEN_SQKM_SE * AREA_KM2
        ) |>
        dplyr::select(-CLEN_values, -CLEN_SQKM_values)
    } else {
      all_combos <- all_sets |>
        tidyr::crossing(spec_only) |>
        tidyr::crossing(length_data |> dplyr::select(FLEN) |> dplyr::distinct())
      
      length_complete <- all_combos |>
        dplyr::left_join(
          length_data |>
            dplyr::select(
              MISSION,
              SETNO,
              SPEC,
              FLEN,
              CLEN_TOTAL,
              CLEN_SQKM_TOTAL,
              YEAR
            ),
          by = c("MISSION", "SETNO", "SPEC", "FLEN")
        ) |>
        dplyr::mutate(
          CLEN_TOTAL = ifelse(is.na(CLEN_TOTAL), 0, CLEN_TOTAL),
          CLEN_SQKM_TOTAL = ifelse(is.na(CLEN_SQKM_TOTAL), 0, CLEN_SQKM_TOTAL)
        )
      
      length_strat <- length_complete |>
        dplyr::group_by(YEAR, SPEC, STRAT, AREA_KM2, FLEN) |>
        dplyr::summarise(
          COUNT = dplyr::n(),
          CLEN_SUM = sum(CLEN_TOTAL, na.rm = TRUE),
          CLEN_MEAN = mean(CLEN_TOTAL, na.rm = TRUE),
          CLEN_MEAN_SE = round(Mar.utils::st_err(CLEN_TOTAL), 5),
          CLEN_values = list(CLEN_TOTAL),
          CLEN_SQKM_SUM = sum(CLEN_SQKM_TOTAL, na.rm = TRUE),
          CLEN_SQKM_MEAN = mean(CLEN_SQKM_TOTAL, na.rm = TRUE),
          CLEN_SQKM_values = list(CLEN_SQKM_TOTAL),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          CLEN_SE = sapply(CLEN_values, Mar.utils::st_err),
          CLEN_SQKM_SE = sapply(CLEN_SQKM_values, Mar.utils::st_err),
          CLEN_STRAT_TOTAL = CLEN_SQKM_MEAN * AREA_KM2,
          CLEN_STRAT_TOTAL_SE = CLEN_SQKM_SE * AREA_KM2
        ) |>
        dplyr::select(-CLEN_values, -CLEN_SQKM_values)
    }
    
    lenSet_mean <- widen_data(
      length_complete,
      var_col = "FLEN",
      value_col = "CLEN_TOTAL",
      bin_size = bin_size,
      level = "set",
      by_sex = by_sex
    )
    lenStrat_mean <- widen_data(
      length_strat,
      var_col = "FLEN",
      value_col = "CLEN_MEAN",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    lenStrat_se <- widen_data(
      length_strat,
      var_col = "FLEN",
      value_col = "CLEN_SE",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    lenStrat_total <- widen_data(
      length_strat,
      var_col = "FLEN",
      value_col = "CLEN_STRAT_TOTAL",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    lenStrat_total_LENGTHS <- widen_data(
      length_strat,
      var_col = "FLEN",
      value_col = "CLEN_STRAT_TOTAL",
      bin_size = bin_size,
      level = "strat",
      by_sex = F
    )
    lenStrat_total_se <- widen_data(
      length_strat,
      var_col = "FLEN",
      value_col = "CLEN_STRAT_TOTAL_SE",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    
    ###### MEAN ######
    
    mean_df <- lenStrat_mean |>
      dplyr::left_join(
        lenStrat_se |>
          dplyr::select(-AREA_KM2,) |>
          dplyr::rename_with(
            ~ sub("(.*FLEN_)(\\d+)$", "\\1SE_\\2", .), # Insert _SE_ before number
            dplyr::contains("FLEN_")
          ),
        by = c("YEAR","STRAT","SPEC")
      )
    
    # Detect original FLEN columns (no _SE_)
    flen_cols <- grep("FLEN_\\d+$", names(mean_df), value = TRUE)
    
    # Detect SE columns (with _SE_)
    se_flen_cols <- grep("FLEN_SE_\\d+$", names(mean_df), value = TRUE)
    
    LENGTH_MEAN <- dplyr::bind_rows(
      lapply(
        split(mean_df, list(mean_df$YEAR, mean_df$SPEC), drop = TRUE),
        function(group_df) {
          
          yr   <- unique(group_df$YEAR)
          spec <- unique(group_df$SPEC)
          
          
          dplyr::bind_rows(
            Map(
              function(x, y) {
                out <- calcYearSummary(
                  group_df,
                  valueField = x,
                  seField = y,
                  areaField = "AREA_KM2",
                  is_mean = TRUE,
                )
                out$YEAR<-yr
                out$SPEC<-spec
                out$FLEN_col <- x
                out |>
                  dplyr::select(YEAR,SPEC,value, se, FLEN_col) |>
                  dplyr::rename(LENGTH_MEAN_SE = se, LENGTH_MEAN = value)
              },
              flen_cols,
              se_flen_cols
            )
          )
        }
      )
    )
    ###### SECOND BLOCK ######
    
    total_df <- lenStrat_total |>
      dplyr::left_join(
        lenStrat_total_se |>
          dplyr::select(-AREA_KM2) |>
          dplyr::rename_with(
            ~ sub("(.*FLEN_)(\\d+)$", "\\1SE_\\2", .),
            dplyr::contains("FLEN_")
          ),
        by = c("STRAT","YEAR","SPEC")
      )
    
    flen_cols2 <- grep("FLEN_\\d+$", names(total_df), value = TRUE)
    se_flen_cols2 <- grep("FLEN_SE_\\d+$", names(total_df), value = TRUE)
    
    LENGTH_TOTAL <- dplyr::bind_rows(
      lapply(
        split(total_df, list(total_df$YEAR, total_df$SPEC), drop = TRUE),
        function(group_df) {
          
          yr   <- unique(group_df$YEAR)
          spec <- unique(group_df$SPEC)
          
          dplyr::bind_rows(
            Map(
              function(x, y) {
                out <- calcYearSummary(
                  group_df,
                  valueField = x,
                  seField = y,
                  areaField = "AREA_KM2",
                  is_mean = FALSE
                )
                out$YEAR<-yr
                out$SPEC<-spec
                out$FLEN_col <- x
                out |>
                  dplyr::select(YEAR,SPEC, value, se, FLEN_col) |>
                  dplyr::rename(LENGTH_TOTAL_SE = se, LENGTH_TOTAL = value)
              },
              flen_cols2,
              se_flen_cols2
            )
          )
        }
      )
    )
    
    summary_flen <- LENGTH_MEAN |>
      dplyr::left_join(LENGTH_TOTAL, by = c("YEAR","SPEC","FLEN_col")) |>
      dplyr::mutate(
        # Extract sex prefix if present (U, M, F), else empty string
        SEX = dplyr::if_else(
          grepl("^[UMF]_FLEN_", FLEN_col),
          sub("_FLEN_.*", "", FLEN_col),
          ""
        ),
        FLEN_NUM = as.integer(sub(".*_(\\d+)$", "\\1", FLEN_col)),
        SEX_ORDER = dplyr::case_when(
          SEX == "" ~ 0,
          SEX == "U" ~ 1,
          SEX == "M" ~ 2,
          SEX == "F" ~ 3
        )
      ) |>
      dplyr::arrange(SEX_ORDER, FLEN_NUM) |>
      dplyr::select(
        YEAR,
        SPEC,
        FLEN_col,
        LENGTH_MEAN,
        LENGTH_MEAN_SE,
        LENGTH_TOTAL,
        LENGTH_TOTAL_SE
      )
    
    # handle flen summaries
    flen_mean_vals <- summary_flen |>
      dplyr::select(YEAR, SPEC, FLEN_col, LENGTH_MEAN) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_MEAN) |>
      as.list()
    
    flen_mean_se_vals <- summary_flen |>
      dplyr::select(YEAR, SPEC, FLEN_col, LENGTH_MEAN_SE) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_MEAN_SE) |>
      as.list()
    
    flen_total_vals <- summary_flen |>
      dplyr::select(YEAR, SPEC, FLEN_col, LENGTH_TOTAL) |>
      tidyr::pivot_wider(names_from = FLEN_col, values_from = LENGTH_TOTAL) |>
      as.list()
    
    flen_total_se_vals <- summary_flen |>
      dplyr::select(YEAR, SPEC, FLEN_col, LENGTH_TOTAL_SE) |>
      tidyr::pivot_wider(
        names_from = FLEN_col,
        values_from = LENGTH_TOTAL_SE
      ) |>
      as.list()
    
    lenStrat_mean <- lenStrat_mean |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        !!!flen_mean_vals
      )
    
    lenStrat_se <- lenStrat_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        !!!flen_mean_se_vals
      )
    
    lenStrat_total <- lenStrat_total |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        !!!flen_total_vals
      )
    
    lenStrat_total_se <- lenStrat_total_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
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
    agelen <- tblList$GSDET |>
      dplyr::left_join(
        tblList$GSINF[, c("MISSION", "SETNO", "STRAT")],
        by = c("MISSION", "SETNO")
      ) |>
      dplyr::mutate(YEAR=as.numeric(substr(MISSION, 4, 7)))|>
      dplyr::select(YEAR, SPEC, FLEN, AGE, STRAT, !!!if (by_sex) rlang::syms("FSEX"))
    
    if (by_sex) {
      agelen <- agelen |>
        dplyr::filter(!is.na(FLEN) & !is.na(AGE) & !is.na(FSEX))
    } else {
      agelen <- agelen |> dplyr::filter(!is.na(FLEN) & !is.na(AGE))
    }
    
    alk_group_vars <- c("YEAR","SPEC","FLEN_BIN", "AGE")
    if (by_sex) {
      alk_group_vars <- c(alk_group_vars, "FSEX")
    }
    
    alk <- agelen |>
      binnit(bin_size) |>
      dplyr::count(dplyr::across(dplyr::all_of(alk_group_vars))) |>
      tidyr::complete(
        FLEN_BIN = seq(min(FLEN_BIN), max(FLEN_BIN), by = bin_size),
        AGE,
        YEAR,
        SPEC,
        !!!if (by_sex) dplyr::syms("FSEX"),
        fill = list(n = 0)
      )
    
    if (by_sex) {
      alk <- alk |>
        tidyr::pivot_wider(
          names_from = c( "AGE"),
          values_from = n,
          values_fill = 0
        ) |>
        dplyr::arrange(FLEN_BIN) |>
        dplyr::rename(FLEN = FLEN_BIN) |>
        dplyr::mutate(FLEN=ifelse(FSEX==0,paste("U_",FLEN,sep=""),
                                  ifelse(FSEX==1,paste("M_",FLEN,sep=""),paste("F_",FLEN,sep="")))) %>%
        dplyr::select(
          -FSEX
        )
    } else {
      alk <- alk |>
        tidyr::pivot_wider(
          names_from = "AGE",
          values_from = n,
          values_fill = 0,
          names_glue = "AGE_{AGE}"
        ) |>
        dplyr::arrange(FLEN_BIN) |>
        dplyr::rename(FLEN = FLEN_BIN)
    }
    alk <- alk |> as.data.frame()
    
    alk_wide <- agelen |>
      dplyr::count(YEAR, SPEC, STRAT, AGE) |>
      tidyr::pivot_wider(
        names_from = AGE,
        values_from = n,
        names_glue = "AGE_{AGE}",
        values_fill = 0
      )
    
    alw_group_vars <- c("YEAR", "SPEC", "AGE", "FLEN_BIN")
    if (by_sex) {
      alw_group_vars <- c(alw_group_vars, "FSEX")
    }
    
    if (!Converted_Data) {
      totals$standardized_data$FWT_OG<-totals$standardized_data$FWT
      
    }
    
    alw <- totals$standardized_data |>
      binnit(bin_size) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(alw_group_vars))) |>
      dplyr::summarise(FWT = mean(FWT_OG), COUNT = dplyr::n(), .groups = "drop") |>
      tidyr::complete(
        FLEN_BIN = tidyr::full_seq(FLEN_BIN, bin_size),
        AGE,
        YEAR,
        SPEC,
        !!!if (by_sex) dplyr::syms("FSEX"),
        fill = list(FWT = 0, COUNT = 0)
      ) |>
      dplyr::mutate(FWT = FWT / 1000)
    
    if (by_sex) {
      alw <- alw |>
        dplyr::group_by(YEAR, SPEC, FLEN_BIN, FSEX, AGE) |>
        dplyr::summarise(FWT = sum(FWT), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = c("FSEX", "AGE"),
          values_from = "FWT",
          names_glue = "{c(FSEX = c('U','M','F')[FSEX+1])}_AGE_{AGE}"
        ) |>
        dplyr::rename(FLEN = FLEN_BIN) |>
        dplyr::select(
          YEAR,
          SPEC,
          FLEN,
          tidyselect::matches("^U_AGE_\\d+$"),
          tidyselect::matches("^M_AGE_\\d+$"),
          tidyselect::matches("^F_AGE_\\d+$")
        )
    } else {
      alw <- alw |>
        tidyr::pivot_wider(
          names_from = AGE,
          values_from = c(FWT, COUNT),
          names_glue = "{.value}_{AGE}"
        ) |>
        dplyr::rename(FLEN = FLEN_BIN) |>
        dplyr::select(
          YEAR,
          SPEC,
          FLEN,
          tidyselect::matches("^(FWT)_")
        )
    }
    alw <- alw |>
      as.data.frame()
    
    alk_ap <- alk
    
    x <- prop.table(as.matrix(alk_ap[, setdiff(names(alk_ap), c("YEAR", "SPEC","FLEN"))]), 1)
    x <- ifelse(is.nan(x), 0, x)
    ages_prop <- as.data.frame(x) |> dplyr::mutate(FLEN = alk_ap$FLEN,
                                                   YEAR = alk_ap$YEAR,
                                                   SPEC = alk_ap$SPEC)
    
    if (by_sex) {
      
      lengths<- lenStrat_total %>%
        dplyr::group_by(YEAR,SPEC) %>%
        dplyr::filter(!STRAT=="TOTAL")%>%
        dplyr::summarise(across(5:ncol(lenStrat_total)-2, sum, na.rm = TRUE), .groups = "drop")
      
      lengths<- lengths %>%
        tidyr::pivot_longer(!c(YEAR,SPEC),names_to="FLEN",values_to ="length_sum") %>%
        dplyr::mutate(FLEN=gsub("FLEN_","",FLEN))
      
      
      
      
      age_table <- merge(ages_prop, lengths, by = c("YEAR","SPEC","FLEN"), all.x = T)
      
      age_table[, !(names(age_table) %in% c("YEAR","SPEC","FLEN", "length_sum"))] <- round(
        age_table[, !(names(age_table) %in% c("YEAR","SPEC","FLEN", "length_sum"))] *
          age_table[["length_sum"]],
        4
      )
      age_table[is.na(age_table)] <- 0
      age_table$length_sum <- NULL
      
      age_cols <- setdiff(names(age_table), c("YEAR","SPEC","FLEN"))
      age_table_avg_lengths <- sapply(age_cols, function(age) {
        x <- age_table[[age]]
        if (sum(x) == 0) {
          return(NA)
        }
        sum(as.numeric(substring(age_table$FLEN, 3)) * x) / sum(x)
      })
      
      
      AGE_PROP<-age_table %>%
        group_by(YEAR,SPEC,FLEN) %>%
        mutate(TOTAL=rowSums(pick(1:(ncol(age_table)-3)))) 
      
      AGE_PROP<-AGE_PROP %>%
        ungroup() %>%
        mutate(across(4:(ncol(AGE_PROP)-1),~.x/TOTAL))
      
      
      Length_Age<- length_complete %>%
        binnit(bin_size) %>%
        mutate(FLEN=FLEN_BIN,
               FLEN=ifelse(FSEX=="0",paste("U_",FLEN,sep=""),
                           ifelse(FSEX=="2",paste("F_",FLEN,sep=""),
                                  ifelse(FSEX=="1",paste("M_",FLEN,sep=""),NA)))) %>%
        select(-c(FLEN_BIN))
      
      Length_Age<- merge(Length_Age,AGE_PROP,by=c("YEAR","SPEC","FLEN"))
      
      Length_Age_TOT<-Length_Age %>%
        mutate(across(10:(ncol(Length_Age)-1),~.x*CLEN_TOTAL))
      
      Length_Age_TOT<-  Length_Age_TOT %>% 
        group_by(MISSION,YEAR, SPEC,FSEX, STRAT,SETNO,AREA_KM2) %>%
        summarise(across(3:(ncol(Length_Age_TOT)-7),~sum(.x,na.rm = TRUE)),.groups = "drop")
      
      Length_Age_SQKM<-Length_Age %>%
        mutate(across(10:(ncol(Length_Age)-1),~.x*CLEN_SQKM_TOTAL))
      
      Length_Age_SQKM<- Length_Age_SQKM %>% 
        group_by(MISSION,YEAR, SPEC,FSEX, STRAT,SETNO,AREA_KM2) %>%
        summarise(across(3:(ncol(Length_Age)-7),~sum(.x,na.rm = TRUE)),.groups = "drop")
      
      
      Length_Age_TOT<- Length_Age_TOT %>%
        tidyr::pivot_longer(!c(MISSION,YEAR,SPEC,FSEX,STRAT,SETNO,AREA_KM2,CLEN_SQKM_TOTAL),names_to="AGE",values_to ="CAGE_TOTAL") %>%
        dplyr::mutate(AGE=gsub("AGE_","",AGE)) %>%
        filter(!AGE=="TOTAL")
      
      
      Length_Age_SQKM<- Length_Age_SQKM %>%
        tidyr::pivot_longer(!c(MISSION,YEAR,SPEC,FSEX,STRAT,SETNO,AREA_KM2,CLEN_SQKM_TOTAL),names_to="AGE",values_to ="CAGE_SQKM_TOTAL") %>%
        dplyr::mutate(AGE=gsub("AGE_","",AGE)) %>%
        filter(!AGE=="TOTAL")
      
      age_complete<-merge(Length_Age_TOT,Length_Age_SQKM,by=c("MISSION","YEAR","SPEC","FSEX","STRAT","SETNO","AREA_KM2","AGE"))
    
    } else {  
      
    lengths<- lenStrat_total_LENGTHS %>%
      dplyr::group_by(YEAR,SPEC) %>%
      dplyr::filter(!STRAT=="TOTAL")%>%
      dplyr::summarise(across(5:ncol(lenStrat_total_LENGTHS)-2, sum, na.rm = TRUE), .groups = "drop")
    
    lengths<- lengths %>%
      tidyr::pivot_longer(!c(YEAR,SPEC),names_to="FLEN",values_to ="length_sum") %>%
      dplyr::mutate(FLEN=gsub("FLEN_","",FLEN))
    
    age_table <- merge(ages_prop, lengths, by = c("YEAR","SPEC","FLEN"), all.x = T)
    age_table[, !(names(age_table) %in% c("YEAR","SPEC","FLEN", "length_sum"))] <- round(
      age_table[, !(names(age_table) %in% c("YEAR","SPEC","FLEN", "length_sum"))] *
        age_table[["length_sum"]],
      4
    )
    age_table[is.na(age_table)] <- 0
    age_table$length_sum <- NULL
    
    age_cols <- setdiff(names(age_table), c("YEAR","SPEC","FLEN"))
    age_table_avg_lengths <- sapply(age_cols, function(age) {
      x <- age_table[[age]]
      if (sum(x) == 0) {
        return(NA)
      }
      sum(age_table$FLEN * x) / sum(x)
    })
    
    
    AGE_PROP<-age_table %>%
      group_by(YEAR,SPEC,FLEN) %>%
      mutate(TOTAL=rowSums(pick(1:(ncol(age_table)-3)))) 
    
    AGE_PROP<-AGE_PROP %>%
      ungroup() %>%
      mutate(across(4:(ncol(AGE_PROP)-1),~.x/TOTAL))
    
    
    Length_Age<- length_complete %>%
      binnit(bin_size) %>%
      mutate(FLEN=FLEN_BIN) %>%
      select(-c(FLEN_BIN))
    
    Length_Age<- merge(Length_Age,AGE_PROP,by=c("YEAR","SPEC","FLEN"))
    
    Length_Age_TOT<-Length_Age %>%
      mutate(across(10:(ncol(Length_Age)-1),~.x*CLEN_TOTAL))
    
    Length_Age_TOT<-  Length_Age_TOT %>% 
      group_by(MISSION,YEAR, SPEC, STRAT,SETNO,AREA_KM2) %>%
      summarise(across(4:(ncol(Length_Age_TOT)-6),~sum(.x,na.rm = TRUE)),.groups = "drop")
    
    Length_Age_SQKM<-Length_Age %>%
      mutate(across(10:(ncol(Length_Age)-1),~.x*CLEN_SQKM_TOTAL))
    
    Length_Age_SQKM<- Length_Age_SQKM %>% 
      group_by(MISSION,YEAR, SPEC, STRAT,SETNO,AREA_KM2) %>%
      summarise(across(4:(ncol(Length_Age)-6),~sum(.x,na.rm = TRUE)),.groups = "drop")
    
    
    Length_Age_TOT<- Length_Age_TOT %>%
      tidyr::pivot_longer(!c(MISSION,YEAR,SPEC,STRAT,SETNO,AREA_KM2),names_to="AGE",values_to ="CAGE_TOTAL") %>%
      dplyr::mutate(AGE=gsub("AGE_","",AGE)) %>%
      filter(!AGE=="TOTAL")
    
    
    Length_Age_SQKM<- Length_Age_SQKM %>%
      tidyr::pivot_longer(!c(MISSION,YEAR,SPEC,STRAT,SETNO,AREA_KM2),names_to="AGE",values_to ="CAGE_SQKM_TOTAL") %>%
      dplyr::mutate(AGE=gsub("AGE_","",AGE)) %>%
      filter(!AGE=="TOTAL")
    
    age_complete<-merge(Length_Age_TOT,Length_Age_SQKM,by=c("MISSION","YEAR","SPEC","STRAT","SETNO","AREA_KM2","AGE"))
    
    }

    
    set_age <- widen_data(
      age_complete,
      var_col = "AGE",
      value_col = "CAGE_TOTAL",
      bin_size = bin_size,
      level = "set",
      by_sex = by_sex
    )
    
    age_complete <- age_complete |>
      dplyr::mutate(
        CAGE_TOTAL = ifelse(is.na(CAGE_TOTAL), 0, CAGE_TOTAL),
        CAGE_SQKM_TOTAL = ifelse(is.na(CAGE_SQKM_TOTAL), 0, CAGE_SQKM_TOTAL)
      )
    
    
    
    group_vars <- c("YEAR","SPEC", "STRAT", "AREA_KM2", "AGE")
    if (by_sex) {
      group_vars <- c(group_vars, "FSEX")
    }
    age_strat <- age_complete |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::group_map(
        ~ {
          tibble::tibble(
            YEAR = .y$YEAR,
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
        }
      ) |>
      dplyr::bind_rows()
    
    strat_age_mean <- widen_data(
      age_strat,
      var_col = "AGE",
      value_col = "CAGE_MEAN",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    strat_age_mean_se <- widen_data(
      age_strat,
      var_col = "AGE",
      value_col = "CAGE_MEAN_SE",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    strat_age_total <- widen_data(
      age_strat,
      var_col = "AGE",
      value_col = "CAGE_TOTAL",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    strat_age_total_se <- widen_data(
      age_strat,
      var_col = "AGE",
      value_col = "CAGE_TOTAL_SE",
      bin_size = bin_size,
      level = "strat",
      by_sex = by_sex
    )
    
    mean_df <- strat_age_mean |>
      dplyr::left_join(
        strat_age_mean_se |>
          dplyr::select(-AREA_KM2) |>
          dplyr::rename_with(
            ~ sub("(.*AGE_)(\\d+)$", "\\1SE_\\2", .),
            dplyr::contains("AGE_")
          ),
        by = c("YEAR","SPEC","STRAT")
      )
    age_cols <- grep("AGE_\\d+", names(mean_df), value = TRUE)
    se_age_cols <- grep("AGE_SE_\\d+", names(mean_df), value = TRUE)
    
    AGE_MEAN <- dplyr::bind_rows(
      lapply(
        split(mean_df, list(mean_df$YEAR, mean_df$SPEC), drop = TRUE),
        function(group_df) {
          
          yr   <- unique(group_df$YEAR)
          spec <- unique(group_df$SPEC)
          
          dplyr::bind_rows(
      Map(
        function(x, y) {
          out <- calcYearSummary(
            group_df,
            valueField = x,
            seField = y,
            areaField = "AREA_KM2",
            is_mean = TRUE
          )
          out$YEAR<-yr
          out$SPEC<-spec
          out$AGE_col <- x
          out |>
            dplyr::select(YEAR,SPEC,value, se, AGE_col) |>
            dplyr::rename(AGE_MEAN_SE = se, AGE_MEAN = value)
        },
        age_cols,
        se_age_cols
      )
          )
        }
      )
    )
    
   
    
    total_df <- strat_age_total |>
      dplyr::left_join(
        strat_age_total_se |>
          dplyr::select(-AREA_KM2) |>
          dplyr::rename_with(
            ~ sub("(.*AGE_)(\\d+)$", "\\1SE_\\2", .),
            dplyr::contains("AGE_")
          ),
        by = c("YEAR","SPEC","STRAT")
      )
    age_cols2 <- grep("AGE_\\d+", names(total_df), value = TRUE)
    se_age_cols2 <- grep("AGE_SE_\\d+", names(total_df), value = TRUE)
    
    AGE_TOTAL <- dplyr::bind_rows(
      lapply(
        split(total_df, list(total_df$YEAR, total_df$SPEC), drop = TRUE),
        function(group_df) {
          
          yr   <- unique(group_df$YEAR)
          spec <- unique(group_df$SPEC)
          
          dplyr::bind_rows(
      Map(
        function(x, y) {
          out <- calcYearSummary(
            group_df,
            valueField = x,
            seField = y,
            areaField = "AREA_KM2",
            is_mean = FALSE
          )
          out$YEAR<-yr
          out$SPEC<-spec
          out$AGE_col <- x
          out |>
            dplyr::select(YEAR,SPEC,value, se, AGE_col) |>
            dplyr::rename(AGE_TOTAL_SE = se, AGE_TOTAL = value)
        },
        age_cols2,
        se_age_cols2
      )
          )
        }
      )
    )
    
    
    summary_age <- AGE_MEAN |>
      dplyr::left_join(AGE_TOTAL, by = c("YEAR","SPEC","AGE_col")) |>
      dplyr::mutate(
        AGE_NUM = as.integer(sub(".*_(\\d+)$", "\\1", AGE_col))
      ) |>
      dplyr::arrange(AGE_NUM) |>
      dplyr::select(YEAR, SPEC, AGE_col, AGE_MEAN, AGE_MEAN_SE, AGE_TOTAL, AGE_TOTAL_SE)
    
    # strat_age_mean_se <- strat_age_mean_se |>
    #   mutate(TOTAL_SE = sqrt(rowSums(dplyr::across(matches("AGE_\\d+$"))^2)))
    #
    # strat_age_total_se <- strat_age_total_se |>
    #   mutate(TOTAL_SE = sqrt(rowSums(dplyr::across(matches("AGE_\\d+$"))^2)))
    
    # handle age summaries
    age_mean_vals <- summary_age |>
      dplyr::select(YEAR, SPEC, AGE_col, AGE_MEAN) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_MEAN) |>
      as.list()
    
    age_mean_se_vals <- summary_age |>
      dplyr::select(YEAR, SPEC, AGE_col, AGE_MEAN_SE) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_MEAN_SE) |>
      as.list()
    
    age_total_vals <- summary_age |>
      dplyr::select(YEAR, SPEC, AGE_col, AGE_TOTAL) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_TOTAL) |>
      as.list()
    
    age_total_se_vals <- summary_age |>
      dplyr::select(YEAR, SPEC, AGE_col, AGE_TOTAL_SE) |>
      tidyr::pivot_wider(names_from = AGE_col, values_from = AGE_TOTAL_SE) |>
      as.list()
    
    strat_age_mean <- strat_age_mean |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        !!!age_mean_vals
      )
    
    strat_age_mean_se <- strat_age_mean_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        !!!age_mean_se_vals
      )
    
    ageStrat_total <- strat_age_total |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
        !!!age_total_vals
      )
    
    ageStrat_total_se <- strat_age_total_se |>
      dplyr::add_row(
        STRAT = "TOTAL",
        AREA_KM2 = NA_real_,
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
