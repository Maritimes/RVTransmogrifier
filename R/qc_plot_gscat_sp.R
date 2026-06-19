#' Multi-panel QA Plot for Vessel Conversion (GSCAT): Weight and Number, faceted by SPEC
#' 
#' @param summary_df   Output from qc_summary()$df_by_year
#' @param spec         Value of SPEC to filter/plot
#' @return Patchwork object, ready for print or save
qc_plot_gscat_sp <- function(
    summary_df, spec,
    detailed_df = NULL
) {
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(tidyr)
  
  detailed_df <- detailed_df  |> 
    mutate(
      YEAR = as.integer(substr(MISSION, 4, 7)),
      FROM_VESSEL = case_when(
                  substr(conv$GSCAT$MISSION, 1,3) == "ATC" ~ "ATC_HAM",
                  substr(conv$GSCAT$MISSION, 1,3) == "HAM" ~ "ATC_HAM",
                  substr(conv$GSCAT$MISSION, 1,3) == "CAR" ~ "CAR_CAB",
                  substr(conv$GSCAT$MISSION, 1,3) == "CAB" ~ "CAR_CAB",
                  substr(conv$GSCAT$MISSION, 1,3) == "NED" ~ "NED_TEM",
                  substr(conv$GSCAT$MISSION, 1,3) == "TEM" ~ "NED_TEM",
                  substr(conv$GSCAT$MISSION, 1,3) == "TEL" ~ "TEL_VEN",
                  substr(conv$GSCAT$MISSION, 1,3) == "VEN" ~ "TEL_VEN",
                 .default = NA_character_
               )
      )

  # filter for relevant species
  plot_data <- summary_df |> filter(SPEC == spec)
  # mean ratio over time - weights
  # p1 <- ggplot(plot_data, aes(x = YEAR, y = mean_ratio_TOTWGT, color = FROM_VESSEL)) +
  #   geom_line() + geom_point() +
  #   geom_hline(yintercept=1, color="red", linetype="dashed") +
  #   labs(title = "Mean Weight Ratio", x = "Year", y = "Converted/Original") +
  #   theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  # # mean ratio over time - numbers
  # p2 <- ggplot(plot_data, aes(x = YEAR, y = mean_ratio_TOTNO, color = FROM_VESSEL)) +
  #   geom_line() + geom_point() +
  #   geom_hline(yintercept=1, color="red", linetype="dashed") +
  #   labs(title = "Mean Number Ratio", x = "Year", y = "Converted/Original") +
  #   theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  # # ratio of means over time - weights
  if ("ratio_of_means_TOTWGT" %in% names(plot_data)) {
    p3 <- ggplot(plot_data, aes(x = YEAR, y = ratio_of_means_TOTWGT, color = FROM_VESSEL)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, color="red", linetype="dashed") +
      labs(title = "Ratio of Mean Weights", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  } else {
    p3 <- ggplot() + theme_void() + labs(title = "Ratio of Mean Weights\n(need to add to summary)")
  }
  # ratio of means over time - numbers
  if ("ratio_of_means_TOTNO" %in% names(plot_data)) {
    p4 <- ggplot(plot_data, aes(x = YEAR, y = ratio_of_means_TOTNO, color = FROM_VESSEL)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, color="red", linetype="dashed") +
      labs(title = "Ratio of Mean Numbers", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  } else {
    p4 <- ggplot() + theme_void() + labs(title = "Ratio of Mean Numbers\n(need to add to summary)")
  }
  
  # boxplot of sample-level converted/original ratios (weights)
  if (!is.null(detailed_df) && all(c("TOTWGT","TOTWGT_OG") %in% names(detailed_df))) {
    detailed_weights <- detailed_df |>
      filter(SPEC == spec, !is.na(TOTWGT_OG), TOTWGT_OG != 0) |>
      mutate(ratio = TOTWGT / TOTWGT_OG)
    p5 <- ggplot(detailed_weights, aes(x = factor(YEAR), y = ratio, fill = FROM_VESSEL)) +
      geom_boxplot(outlier.size=0.5) +
      labs(title = "Sample Ratio (Weights) by Year", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position="none", plot.title=element_text(size=10))
  } else {
    p5 <- ggplot() + theme_void() + labs(title = "Sample Ratio (Weights)")
  }
  # boxplot of sample-level converted/original ratios (numbers)
  if (!is.null(detailed_df) && all(c("TOTNO","TOTNO_OG") %in% names(detailed_df))) {
    detailed_numbers <- detailed_df |>
      filter(SPEC == spec, !is.na(TOTNO_OG), TOTNO_OG != 0) |>
      mutate(ratio = TOTNO / TOTNO_OG)
    p6 <- ggplot(detailed_numbers, aes(x = factor(YEAR), y = ratio, fill = FROM_VESSEL)) +
      geom_boxplot(outlier.size=0.5) +
      labs(title = "Sample Ratio (Numbers) by Year", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position="none", plot.title=element_text(size=10))
  } else {
    p6 <- ggplot() + theme_void() + labs(title = "Sample Ratio (Numbers)")
  }

  # aggregated sum over time - weights
  total_sums_wt <- detailed_df  |>
    filter(SPEC == spec) |>
    group_by(YEAR, FROM_VESSEL) |>
    summarise(TOTWGT = sum(TOTWGT, na.rm=TRUE), TOTWGT_OG = sum(TOTWGT_OG, na.rm=TRUE), .groups="drop") |>
    pivot_longer(cols = c(TOTWGT, TOTWGT_OG), names_to="Type", values_to="Value")
  p7 <- ggplot(total_sums_wt, aes(x=YEAR, y=Value, color=FROM_VESSEL, linetype = Type,
                                  group = interaction(FROM_VESSEL, Type))) +
    geom_line() + geom_point() +
    #facet_wrap(~FROM_VESSEL) +
    labs(title = "Sum of Weights (Converted vs. Original)", x = "Year", y = "Sum") +
    theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  
  # aggregated sum over time - numbers
  total_sums_num <- detailed_df |>
    filter(SPEC == spec) |>
    group_by(YEAR, FROM_VESSEL) |>
    summarise(TOTNO = sum(TOTNO, na.rm=TRUE), TOTNO_OG = sum(TOTNO_OG, na.rm=TRUE), .groups="drop") |>
    pivot_longer(cols = c(TOTNO, TOTNO_OG), names_to="Type", values_to="Value")
  p8 <- ggplot(total_sums_num, aes(x=YEAR, y=Value, color=FROM_VESSEL, linetype = Type,
                                   group = interaction(FROM_VESSEL, Type))) +
    geom_line() + geom_point() +
   #facet_wrap(~FROM_VESSEL) +
    labs(title = "Sum of Numbers (Converted vs. Original)", x = "Year", y = "Sum") +
    theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  

  pw <-  (p3 ) / (p5 ) / (p7 ) +
    patchwork::plot_annotation(title = paste("SPEC", spec, " TOTWGT (GSCAT)"), theme = theme(plot.title = element_text(size=14, hjust=0.5)))
  pn <- (p4 ) / (p6 ) / (p8 ) +
    patchwork::plot_annotation(title = paste("SPEC", spec, " TOTNO (GSCAT)"), theme = theme(plot.title = element_text(size=14, hjust=0.5)))
  res<- list()
  res$weights <- pw
  res$numbers <- pn
  return(res)
}
