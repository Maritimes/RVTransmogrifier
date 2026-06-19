#' @title Summarize Quality-Controlled Catch Data
#'
#' @description
#' Computes summary statistics for quality-controlled RV survey data (GSCAT_CONV 
#' or GSDET_CONV format). Generates group-wise (by SPEC, YEAR, FROM_VESSEL) and 
#' overall aggregates, comparing original (`*_OG`) and converted (`*`) 
#' abundance/biomass (number and weight), including means, medians, standard 
#' deviations, differences, and ratios, as well as missing value counts. 
#' Designed to help diagnose the effect and quality of vessel conversion factor 
#' application.
#'
#' @param df A data frame in `GSCAT_CONV` or `GSDET_CONV` format. Must contain 
#' at minimum the columns: `"SPEC"`, `"MISSION"`, `"FROM_VESSEL"`, and either 
#' `("TOTWGT", "TOTNO", "TOTWGT_OG", "TOTNO_OG")` for GSCAT or `("FWT", "CLEN", 
#' "FWT_OG", "CLEN_OG")` for GSDET.
#' @param annual Logical, defaults to \code{FALSE}. (Currently unused in 
#' function but reserved for future behavior with annual summaries.)
#'
#' @return
#' A named list with:
#' \describe{
#'   \item{df_by_year}{A data frame with summary statistics (by SPEC, YEAR, and FROM_VESSEL) for converted and original catch abundance/biomass, differences, ratios (excluding denominator = 0 or NA), and counts of missing values.}
#'   \item{df_overall}{A data frame of overall summary statistics by SPEC and FROM_VESSEL (across years), including means, medians, overall ratios, differences and combined missing value statistics.}
#' }
#'
#' @details
#' \itemize{
#'   \item Where ratio statistics are calculated, only records with nonzero and non-missing denominators are included.
#'   \item Summaries are computed by species, year, and vessel (and overall by species and vessel).
#'   \item Supports automatic detection and handling of input either from GSCAT_CONV or GSDET_CONV format.
#' }
#'
#' @note
#' Cases where the denominator is zero or missing are excluded from ratio calculations to avoid division by zero. This can bias ratio estimates if such cases are frequent or non-random—consider inspecting the number and pattern of missing/zero denominator rows in your data.
#'
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#'
#' @examples
#' \dontrun{
#' # For a GSCAT_CONV-format data frame 'df':
#' result <- qc_summary(df)
#' head(result$df_by_year)
#' head(result$df_overall)
#' }
#' @export
qc_summary <- function(df, annual=F) {
  
  if (all(c("SPEC", "MISSION", "FROM_VESSEL", "TOTWGT", "TOTNO", "TOTWGT_OG", "TOTNO_OG") %in% names(df))){
    print("GSCAT_CONV")
    theWeight <- "TOTWGT"
    theWeight_og <- "TOTWGT_OG"
    theNumber <-"TOTNO"
    theNumber_og <-"TOTNO_OG"
    wtName <- "TOTWGT"
    numName <- "TOTNO"
  }else if (all(c("SPEC", "MISSION", "FROM_VESSEL", "FWT", "CLEN", "FWT_OG", "CLEN_OG") %in% names(df))){
    print("GSDET_CONV")
    theWeight <- "FWT"
    theWeight_og <- "FWT_OG"
    theNumber <-"CLEN"
    theNumber_og <-"CLEN_OG"
    wtName <- "FWT"
    numName <- "CLEN"
  } else {
    stop("The data object was not recognized as one of GSCAT_CONV or GSDET_CONV")
  } 
  
  # Add YEAR column extracted from MISSION
  df <- df |>
    mutate(
      YEAR = as.integer(substr(MISSION, 4, 7))
    )
  
  df <- df |>
    group_by(SPEC, YEAR, FROM_VESSEL) |>
    summarise(
      n_obs = n(),
      mean_wt = mean(.data[[theWeight]], na.rm = TRUE),
      mean_wt_OG = mean(.data[[theWeight_og]], na.rm = TRUE),
      median_wt = median(.data[[theWeight]], na.rm = TRUE),
      median_wt_OG = median(.data[[theWeight_og]], na.rm = TRUE),
      sd_wt = sd(.data[[theWeight]], na.rm = TRUE),
      sd_wt_OG = sd(.data[[theWeight_og]], na.rm = TRUE),
      mean_diff_wt = mean(.data[[theWeight]] - .data[[theWeight_og]], na.rm = TRUE),
      # mean_ratio_wt = mean(.data[[theWeight]] / .data[[theWeight_og]], na.rm = TRUE),
      # sd_ratio_wt = sd(.data[[theWeight]] / .data[[theWeight_og]], na.rm = TRUE),
      mean_ratio_wt = mean(
        ifelse(!is.na(.data[[theWeight_og]]) & .data[[theWeight_og]] != 0,
               .data[[theWeight]] / .data[[theWeight_og]],
               NA), na.rm = TRUE
      ),
      sd_ratio_wt = sd(
        ifelse(!is.na(.data[[theWeight_og]]) & .data[[theWeight_og]] != 0,
               .data[[theWeight]] / .data[[theWeight_og]],
               NA), na.rm = TRUE
      ),
      ratio_of_means_wt = mean(.data[[theWeight]], na.rm=TRUE) / mean(.data[[theWeight_og]], na.rm=TRUE),
      mean_num = mean(.data[[theNumber]], na.rm = TRUE),
      mean_num_OG = mean(.data[[theNumber_og]], na.rm = TRUE),
      median_num = median(.data[[theNumber]], na.rm = TRUE),
      median_num_OG = median(.data[[theNumber_og]], na.rm = TRUE),
      sd_num = sd(.data[[theNumber]], na.rm = TRUE),
      sd_num_OG = sd(.data[[theNumber_og]], na.rm = TRUE),
      mean_diff_num = mean(.data[[theNumber]] - .data[[theNumber_og]], na.rm = TRUE),
      # mean_ratio_num = mean(.data[[theNumber]] / .data[[theNumber_og]], na.rm = TRUE),
      # sd_ratio_num = sd(.data[[theNumber]] / .data[[theNumber_og]], na.rm = TRUE),
      mean_ratio_num = mean(
        ifelse(!is.na(.data[[theNumber_og]]) & .data[[theNumber_og]] != 0,
               .data[[theNumber]] / .data[[theNumber_og]],
               NA), na.rm = TRUE
      ),
      sd_ratio_num = sd(
        ifelse(!is.na(.data[[theNumber_og]]) & .data[[theNumber_og]] != 0,
               .data[[theNumber]] / .data[[theNumber_og]],
               NA), na.rm = TRUE
      ),
      ratio_of_means_num = mean(.data[[theNumber]], na.rm=TRUE) / mean(.data[[theNumber_og]], na.rm=TRUE),
      # Missing value counts
      n_missing_wt = sum(is.na(.data[[theWeight]])),
      n_missing_wt_OG = sum(is.na(.data[[theWeight_og]])),
      n_missing_num = sum(is.na(.data[[theNumber]])),
      n_missing_num_OG = sum(is.na(.data[[theNumber_og]])),
      .groups = "drop"
    ) |>
    ungroup()

  df_overall <- df |>
    group_by(SPEC, FROM_VESSEL) |>
    summarise(
      n_years = n(),
      total_n_obs = sum(n_obs, na.rm = TRUE),
      overall_mean_wt = mean(mean_wt, na.rm = TRUE),
      overall_mean_wt_OG = mean(mean_wt_OG, na.rm = TRUE),
      overall_median_wt = median(median_wt, na.rm = TRUE),
      overall_median_wt_OG = median(median_wt_OG, na.rm = TRUE),
      overall_mean_diff_wt = mean(mean_diff_wt, na.rm = TRUE),
      overall_mean_ratio_wt = mean(mean_ratio_wt[is.finite(mean_ratio_wt)], na.rm = TRUE),
      min_mean_ratio_wt = min(mean_ratio_wt[is.finite(mean_ratio_wt)], na.rm = TRUE),
      max_mean_ratio_wt = max(mean_ratio_wt[is.finite(mean_ratio_wt)], na.rm = TRUE),
      
      overall_mean_num = mean(mean_num, na.rm = TRUE),
      overall_mean_num_OG = mean(mean_num_OG, na.rm = TRUE),
      overall_median_num = median(median_num, na.rm = TRUE),
      overall_median_num_OG = median(median_num_OG, na.rm = TRUE),
      overall_mean_diff_num = mean(mean_diff_num, na.rm = TRUE),
      overall_mean_ratio_num = mean(mean_ratio_num[is.finite(mean_ratio_num)], na.rm = TRUE),
      min_mean_ratio_num = min(mean_ratio_num[is.finite(mean_ratio_num)], na.rm = TRUE),
      max_mean_ratio_num = max(mean_ratio_num[is.finite(mean_ratio_num)], na.rm = TRUE),
      
      total_missing_wt = sum(n_missing_wt, na.rm = TRUE),
      total_missing_wt_OG = sum(n_missing_wt_OG, na.rm = TRUE),
      total_missing_num = sum(n_missing_num, na.rm = TRUE),
      total_missing_num_OG = sum(n_missing_num_OG, na.rm = TRUE),
      .groups = "drop"
    )  |>
    rename_with(~ gsub("wt", wtName, .x), contains("wt")) |>
    rename_with(~ gsub("num", numName, .x), contains("num")) |>
    arrange(SPEC, FROM_VESSEL) |>
    ungroup()
  
  df_by_year <- df|>
    rename_with(~ gsub("wt", wtName, .x), contains("wt")) |>
    rename_with(~ gsub("num", numName, .x), contains("num")) |>
    arrange(SPEC, FROM_VESSEL) |>
    arrange(SPEC, YEAR, FROM_VESSEL) |>
    ungroup()
  
  results<- list()
  results$df_by_year <- df_by_year
  results$df_overall <- df_overall
  
  return(results)
}