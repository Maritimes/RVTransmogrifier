#' @title plotRVSummary
#' @param cxn  the default is \code{NULL}. A valid Oracle connection object. This parameter allows you to
#' pass an existing connection, reducing the need to establish a new connection
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param code  the default is \code{NULL}. If data should be limited to a particular species, enter
#' the species code here
#' @param taxa  the default is \code{NULL}. Any value found in any of "SPEC", "KINGDOM",
#' "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})
#' @param survey   the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are "SPRING", "SUMMER", "FALL", and "4VSW".  A value of NULL will result in products being
#' generated for all 4 different surveys.
#' @param strata  the default is \code{NULL}. A vector of 1 or more strata can be passed to this function, and the
#' data will be filtered to only that/those strata (via GSSTRATUM$STRAT)
#' @param types  the default is \code{1}.  A vector of 1 or more set "types" can be passed to this function, and the
#' data will be filtered to only that/those set types (via GSINF$TYPE)
#' @param species_name   the default is \code{NULL}. This will be used in the plot title.
#' @param stock  the default is \code{NULL}. A valid stock name from `stock_map`. If provided, it overrides
#' manually specified parameters like `code`, `months`, `strata`, etc.
#' @param years  the default is \code{NULL}.
#' @param use_new_areas  the default is \code{FALSE}. This is a development parameter.  It won't work for most users.
#' @param show_error  the default is \code{FALSE}.  If confidence intervals should be shown on the plot, set this to TRUE.
#'
#' @returns a list including a plot and the stratified biomass/abundance data behind the plot
#' @export

plotRVSummary <- function(
  code = NULL,
  taxa = NULL,
  survey = NULL,
  strata = NULL,
  types = 1,
  species_name = NULL,
  stock = NULL,
  cxn = NULL,
  years = NULL,
  show_error = FALSE,
  use_new_areas = FALSE
) {
  # Optional validation to avoid conflicting parameters
  if (!is.null(stock)) {
    if (
      !is.null(code) || !is.null(taxa) || !is.null(strata) || !is.null(types)
    ) {
      warning(
        "When 'stock' is provided, 'code', 'taxa', 'strata', and 'types' will be ignored."
      )
    }
    code <- NULL
    taxa <- NULL
    strata <- NULL
    types <- NULL
  }

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

  if (use_new_areas) {
    message("The new areas currently exist only on Mike's computer")
    newStrataAreas <- utils::read.csv(
      file = "c:/Users/mcmahonm/OneDrive - DFO-MPO/Support/!group_Groundfish/strataInvestigation/GSSTRATUM_COMPARE.csv"
    )

    results_all$GSINF <-
      results_all$GSINF |>
      dplyr::rename(AREA_KM2_GSSTRATUM = AREA_KM2) |>
      dplyr::left_join(
        newStrataAreas[, c("STRAT", "strata_2014_UTM20_M2")],
        by = "STRAT"
      ) |>
      dplyr::mutate(
        AREA_KM2_CALCULATED = round(strata_2014_UTM20_M2 / 1000000, 4)
      )
    results_strat <- stratify_simple(
      tblList = results_all,
      towDist_NM = 1.75,
      areaField = "AREA_KM2_CALCULATED",
      areaFieldUnits = "KM2"
    )
  } else {
    results_all$GSINF <-
      results_all$GSINF |>
      dplyr::rename(AREA_KM2_GSSTRATUM = AREA_KM2)

    results_strat <- stratify_simple(
      tblList = results_all,
      towDist_NM = 1.75,
      areaField = "AREA_KM2_GSSTRATUM",
      areaFieldUnits = "KM2"
    )
  }

  res <- list()
  # # Calculate long-term geometric mean (1987-2020) for each metric
  geometric_mean <- function(x) exp(mean(log(x[x > 0]), na.rm = TRUE)) # Avoid log(0)
  
  mean_BIOMASS_1987_2020 <- geometric_mean(
    results_strat$results_by_year |>
      dplyr::filter(YEAR >= 1987 & YEAR <= 2020) |>
      dplyr::pull(BIOMASS)
  ) |> round(0)
  
  mean_ABUNDANCE_1987_2020 <- geometric_mean(
    results_strat$results_by_year |>
      dplyr::filter(YEAR >= 1987 & YEAR <= 2020) |>
      dplyr::pull(ABUNDANCE)
  ) |> round(0)

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
    species_name <- species_name
  }

  results_strat$results_by_year <- results_strat$results_by_year |>
    dplyr::mutate(
      GEO_MEAN_BIOMASS = round(
        zoo::rollapply(
          BIOMASS,
          width = 3,
          FUN = geometric_mean,
          fill = NA,
          align = "center"
        ),
        0
      ),
      GEO_MEAN_ABUNDANCE = round(
        zoo::rollapply(
          ABUNDANCE,
          width = 3,
          FUN = geometric_mean,
          fill = NA,
          align = "center"
        ),
        0
      )
    )
  
  # Make plots
  p_biomass <- ggplot2::ggplot(
    results_strat$results_by_year,
    ggplot2::aes(x = YEAR, y = BIOMASS)
  ) +
    {if (show_error) 
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = BIOMASS_LOW, ymax = BIOMASS_HIGH),
      fill = "blue",
      alpha = 0.2
    ) else NULL} +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(
      ggplot2::aes(y = GEO_MEAN_BIOMASS),
      linewidth = 1,
      color = "black"
    ) + # Geometric mean line
    ggplot2::geom_hline(
      yintercept = mean_BIOMASS_1987_2020 * 0.8,
      linetype = "dotted",
      color = "red"
    ) + # 80% line
    ggplot2::geom_hline(
      yintercept = mean_BIOMASS_1987_2020 * 0.4,
      linetype = "dotted",
      color = "red"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = paste0(
        "Total Biomass of ",
        species_name,
        " by Year (",
        survey,
        ")"
      ),
      x = "Year",
      y = "Total Biomass (tons)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

  # Plot for Total Number
  p_abundance <- ggplot2::ggplot(
    results_strat$results_by_year,
    ggplot2::aes(x = YEAR, y = ABUNDANCE)
  ) +
    {if (show_error) 
      ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = ABUNDANCE_LOW,
        ymax = ABUNDANCE_HIGH
      ),
      fill = "blue",
      alpha = 0.2
      ) else NULL} +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(
      ggplot2::aes(y = GEO_MEAN_ABUNDANCE),
      linewidth = 1,
      color = "black"
    ) + # Geometric mean line
    ggplot2::geom_hline(
      yintercept = mean_ABUNDANCE_1987_2020 * 0.8,
      linetype = "dotted",
      color = "red"
    ) + # 80% line
    ggplot2::geom_hline(
      yintercept = mean_ABUNDANCE_1987_2020 * 0.4,
      linetype = "dotted",
      color = "red"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = paste0("Total Abundance of ", species_name," by Year (", survey, ")"),
      x = "Year",
      y = "Total Abundance"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

  # Combine plots vertically
  res$results_strat <- results_strat
  res$plot <- cowplot::plot_grid(p_biomass, p_abundance, ncol = 1, align = 'v')

  return(res)
}
