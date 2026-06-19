#' Multi-panel QA Plot for Vessel Conversion (GSDET): Weight and Number, faceted by SPEC
#' 
#' @param summary_df   Output from qc_summary()$df_by_year
#' @param spec         Value of SPEC to filter/plot
#' @return Patchwork object, ready for print or save
qc_plot_gsdet_sp <- function(
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
        substr(MISSION, 1,3) == "ATC" ~ "ATC_HAM",
        substr(MISSION, 1,3) == "HAM" ~ "ATC_HAM",
        substr(MISSION, 1,3) == "CAR" ~ "CAR_CAB",
        substr(MISSION, 1,3) == "CAB" ~ "CAR_CAB",
        substr(MISSION, 1,3) == "NED" ~ "NED_TEM",
        substr(MISSION, 1,3) == "TEM" ~ "NED_TEM",
        substr(MISSION, 1,3) == "TEL" ~ "TEL_VEN",
        substr(MISSION, 1,3) == "VEN" ~ "TEL_VEN",
        .default = NA_character_
      )
    )
  # filter for relevant species
  plot_data <- summary_df |> filter(SPEC == spec)
  
  # ratio of means over time - weights (FWT)
  if ("ratio_of_means_FWT" %in% names(plot_data)) {
    p3 <- ggplot(plot_data, aes(x = YEAR, y = ratio_of_means_FWT, color = FROM_VESSEL)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, color="red", linetype="dashed") +
      labs(title = "Ratio of Mean Weights", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  } else {
    p3 <- ggplot() + theme_void() + labs(title = "Ratio of Mean Weights\n(need to add to summary)")
  }
  # ratio of means over time - numbers (CLEN)
  if ("ratio_of_means_CLEN" %in% names(plot_data)) {
    p4 <- ggplot(plot_data, aes(x = YEAR, y = ratio_of_means_CLEN, color = FROM_VESSEL)) +
      geom_line() + geom_point() +
      geom_hline(yintercept=1, color="red", linetype="dashed") +
      labs(title = "Ratio of Mean Numbers", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  } else {
    p4 <- ggplot() + theme_void() + labs(title = "Ratio of Mean Numbers\n(need to add to summary)")
  }
  
  # boxplot of sample-level converted/original ratios (weights) - FWT
  if (!is.null(detailed_df) && all(c("FWT","FWT_OG") %in% names(detailed_df))) {
    detailed_weights <- detailed_df |>
      filter(SPEC == spec, !is.na(FWT_OG), FWT_OG != 0) |>
      mutate(ratio = FWT / FWT_OG)
    p5 <- ggplot(detailed_weights, aes(x = factor(YEAR), y = ratio, fill = FROM_VESSEL)) +
      geom_boxplot(outlier.size=0.5) +
      labs(title = "Sample Ratio (Weight) by Year", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position="none", plot.title=element_text(size=10))
  } else {
    p5 <- ggplot() + theme_void() + labs(title = "Sample Ratio (Weight)")
  }
  # boxplot of sample-level converted/original ratios (numbers) - CLEN
  if (!is.null(detailed_df) && all(c("CLEN","CLEN_OG") %in% names(detailed_df))) {
    detailed_numbers <- detailed_df |>
      filter(SPEC == spec, !is.na(CLEN_OG), CLEN_OG != 0) |>
      mutate(ratio = CLEN / CLEN_OG)
    p6 <- ggplot(detailed_numbers, aes(x = factor(YEAR), y = ratio, fill = FROM_VESSEL)) +
      geom_boxplot(outlier.size=0.5) +
      labs(title = "Sample Ratio (Numbers) by Year", x = "Year", y = "Converted/Original") +
      theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position="none", plot.title=element_text(size=10))
  } else {
    p6 <- ggplot() + theme_void() + labs(title = "Sample Ratio (Numbers)")
  }
  
  # aggregated sum over time - weights (FWT)
  total_sums_wt <- detailed_df  |>
    filter(SPEC == spec) |>
    group_by(YEAR, FROM_VESSEL) |>
    summarise(FWT = sum(FWT, na.rm=TRUE), FWT_OG = sum(FWT_OG, na.rm=TRUE), .groups="drop") |>
    pivot_longer(cols = c(FWT, FWT_OG), names_to="Type", values_to="Value")
  p7 <- ggplot(total_sums_wt, aes(
    x=YEAR, y=Value, color=FROM_VESSEL, linetype=Type,
    group=interaction(FROM_VESSEL, Type))) +
    geom_line() + geom_point() +
    labs(title = "Sum of Weights (Converted vs. Original)", x = "Year", y = "Sum") +
    theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  
  # aggregated sum over time - numbers (CLEN)
  total_sums_num <- detailed_df |>
    filter(SPEC == spec) |>
    group_by(YEAR, FROM_VESSEL) |>
    summarise(CLEN = sum(CLEN, na.rm=TRUE), CLEN_OG = sum(CLEN_OG, na.rm=TRUE), .groups="drop") |>
    pivot_longer(cols = c(CLEN, CLEN_OG), names_to="Type", values_to="Value")
  p8 <- ggplot(total_sums_num, aes(
    x=YEAR, y=Value, color=FROM_VESSEL, linetype=Type,
    group=interaction(FROM_VESSEL, Type))) +
    geom_line() + geom_point() +
    labs(title = "Sum of Numbers (Converted vs. Original)", x = "Year", y = "Sum") +
    theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
  
  pw <-  (p3 ) / (p5 ) / (p7 ) +
    patchwork::plot_annotation(title = paste("SPEC", spec, "  FWT (GSDET)"), theme = theme(plot.title = element_text(size=14, hjust=0.5)))
  pn <- (p4 ) / (p6 ) / (p8 ) +
    patchwork::plot_annotation(title = paste("SPEC", spec, " CLEN (GSDET)"), theme = theme(plot.title = element_text(size=14, hjust=0.5)))
  res<- list()
  res$weights <- pw
  res$numbers <- pn
  return(res)
}