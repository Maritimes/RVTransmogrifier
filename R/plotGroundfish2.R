plotRVSummary <- function(
    code = NULL,
    taxa = NULL,
    survey= NULL,
    strata = c(484, 485, 492:495), # 4X
    types = 1,
    species_name = NULL,
    stock = NULL,
    cxn = NULL,
    years = NULL,
    use_new_areas = F
    
) {
  library(dplyr)
  library(ggplot2)
  # Optional validation to avoid conflicting parameters
  if (!is.null(stock)) {
    if (!is.null(code) || !is.null(taxa) || !is.null(strata) || !is.null(types)) {
      warning("When 'stock' is provided, 'code', 'taxa', 'strata', and 'types' will be ignored.")
    }
    code <- NULL
    taxa <- NULL
    strata <- NULL
    types <- NULL
  }
  
  message("draft function!")
  
  # Load data (now includes years parameter)
  results_all <- loadRVData(
    cxn = cxn,
    stock = stock,
    code = code,
    taxa = taxa,
    survey = survey,
    strata = strata,
    types = types,
    years = years # Changed here
  )

# add this bit to allow use of updated UTMZ20N areas --------------------------------------------------------------

  
  newStrataAreas <- read.csv(file = "c:/Users/mcmahonm/OneDrive - DFO-MPO/Support/!group_Groundfish/strataInvestigation/GSSTRATUM_COMPARE.csv")
  
  results_all$GSINF <- 
    results_all$GSINF |> 
    rename(AREA_KM2_GSSTRATUM = AREA_KM2) |> 
    left_join(newStrataAreas[,c("STRAT", "strata_2014_UTM20_M2")], by = "STRAT") |> 
    mutate(AREA_KM2_CALCULATED = round(strata_2014_UTM20_M2/1000000,4))
  
  if (use_new_areas){
    print("using new")
    results_strat  <- stratify_simple(tblList = results_all, towDist_NM = 1.75, areaField = "AREA_KM2_CALCULATED", areaFieldUnits = "KM2")
  }else{
    print("using original")
    
    results_strat <- stratify_simple(tblList = results_all, towDist_NM = 1.75, areaField = "AREA_KM2_GSSTRATUM", areaFieldUnits = "KM2")
  }
  res<- list()
  res$results_strat <- results_strat
  # Run stratify_simple

  # Summarize by year

  results_str <- results_strat$overall|>
    # mutate(YEAR = as.integer(stringr::str_extract(MISSION, "\\d{4}")))|>
    group_by(YEAR)|>
    summarize(
      BIOMASS = sum(BIOMASS, na.rm = TRUE),
      ABUNDANCE = sum(ABUNDANCE, na.rm = TRUE),
      .groups = "drop"
    )|>
    arrange(YEAR)
  
  # Filter by years vector, if provided
  if (!is.null(years)) {
    if (!is.numeric(years)) {
      stop("`years` must be a numeric vector (e.g., years = 1985:2021 or years = c(1985, 1990)).")
    }
    results_str <- results_str|> filter(YEAR %in% years)
  }
  res$summary <- results_str
  
  # Calculate long-term geometric mean (1987-2020) for each metric
  geometric_mean <- function(x) exp(mean(log(x[x > 0]), na.rm = TRUE)) # Avoid log(0)
  mean_BIOMASS_1987_2020 <- geometric_mean(results_str|> filter(YEAR >= 1987 & YEAR <= 2020)|> pull(BIOMASS))
  mean_ABUNDANCE_1987_2020 <- geometric_mean(results_str|> filter(YEAR >= 1987 & YEAR <= 2020)|> pull(ABUNDANCE))
  
  # Add 3-year rolling geometric mean for each metric
  results_str <- results_str|>
    mutate(
      GEO_MEAN_BIOMASS = zoo::rollapply(BIOMASS, width = 3, FUN = geometric_mean, fill = NA, align = "center"),
      GEO_MEAN_ABUNDANCE = zoo::rollapply(ABUNDANCE, width = 3, FUN = geometric_mean, fill = NA, align = "center")
    )
  
  # Pick an informative default species name if not specified
  if (is.null(species_name)) {
    if (!is.null(stock)) {
      species_name <- paste("Stock:", stock)
    } else if (!is.null(taxa)) {
      species_name <- taxa
    } else if (!is.null(code)) {
      species_name <- paste("Species code", code)
    } else {
      species_name <- "Species"
    }
    species_name <- paste0(species_name, " (", survey,
                           if (!is.null(strata)) paste0("; strata=", paste(strata, collapse = ' ')), "",
                           "; stratified)")
  }
  
  # Make plots
  results_str <- results_str|> mutate(YEAR = as.integer(YEAR))
  results_str <- results_str |> filter(!is.na(GEO_MEAN_BIOMASS) & !is.na(GEO_MEAN_ABUNDANCE))
  # Plot for Total Weight
  p_biomass <- ggplot2::ggplot(results_str, aes(x = YEAR, y = BIOMASS)) +
    geom_point(size = 2) +
    geom_line(aes(y = GEO_MEAN_BIOMASS), linewidth = 1, color = "black") +  # Geometric mean line
    geom_hline(yintercept = mean_BIOMASS_1987_2020 * 0.8, linetype = "dotted", color = "red") +  # 80% line
    geom_hline(yintercept = mean_BIOMASS_1987_2020 * 0.4, linetype = "dotted", color = "red") +  # 40% line
    labs(title = paste0("Total Biomass of ", species_name, " by Year (", survey, ")"),
         x = "Year",
         y = "Total Biomass") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  # Plot for Total Number
  p_abundance <- ggplot2::ggplot(results_str, aes(x = YEAR, y = ABUNDANCE)) +
    geom_point(size = 2) +
    geom_line(aes(y = GEO_MEAN_ABUNDANCE), size = 1, color = "black") +  # Geometric mean line
    geom_hline(yintercept = mean_ABUNDANCE_1987_2020 * 0.8, linetype = "dotted", color = "red") +  # 80% line
    geom_hline(yintercept = mean_ABUNDANCE_1987_2020 * 0.4, linetype = "dotted", color = "red") +  # 40% line
    labs(title = paste0("Total Abundance of ", species_name, " by Year (", survey, ")"),
         x = "Year",
         y = "Total Abundance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  # Combine plots vertically
  res$plot <- cowplot::plot_grid(p_biomass, p_abundance, ncol = 1, align = 'v')

  return(res)
}
  
# For code, survey, strata
# plotRVSummary(
#   code = 11,
#   survey = "SUMMER",
#   strata = c("5Z1", "5Z2", "5Z3", "5Z4"),
#   types = 1,
#   species_name = "Haddock"
# )
# 
# # For taxa, survey, strata
# plotRVSummary(
#   taxa = "GADIFORMES",
#   survey = "GEORGES",
#   strata = c("5Z1", "5Z2", "5Z3", "5Z4"),
#   types = 1
# )
# 
# # For code only (no strata)
# plotRVSummary(
#   code = 23,
#   survey = "SUMMER",
#   types = 1,
#   species_name = "Redfish"
# )
# 
# # For taxa only (no strata)
# plotRVSummary(
#   taxa = "MYCTOPHIFORMES",
#   survey = "SUMMER",
#   types = 1
# )
# 
# plotRVSummary(
#   code = 40,
#   survey = "SUMMER",
#   types = 1
# )
