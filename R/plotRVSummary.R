#' @title plotRVSummary
#' @description Generate time series plots of stratified survey data (biomass or abundance) for one or more species. When multiple species are present in the data, the function creates separate plots for each species and returns them as a named list.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT (or GSCAT_CONV), GSDET, GSMISSIONS, and GSSTRATUM.
#' @param code the default is \code{NULL}. A vector of one or more species codes (SPEC) or taxonomic identifiers (TAXA_) to plot. If NULL, all species found in the data will be plotted. If provided, only the specified species will be plotted.
#' @param valueField the default is \code{"BIOMASS"}. The field to plot on the y-axis. Common options include "BIOMASS", "ABUNDANCE", "TOTWGT_MEAN", "TOTNO_MEAN".
#' @param plotTitle the default is \code{NULL}. Optional custom title for the plots. The species code will be appended to this title for each plot.
#' @param minYear the default is \code{NULL}. Minimum year to include in the plot. If NULL, all years are included.
#' @param maxYear the default is \code{NULL}. Maximum year to include in the plot. If NULL, all years are included.
#' @param show_gmean the default is \code{TRUE}. If TRUE, adds a horizontal line showing the geometric mean.
#' @param show_ci the default is \code{TRUE}. If TRUE, adds confidence interval ribbons around the data.
#' @param show_mean the default is \code{TRUE}. If TRUE, adds a horizontal line showing the arithmetic mean.
#' @param legend_position the default is \code{"right"}. Position of the legend. Valid ggplot2 legend positions include "right", "left", "top", "bottom", or "none".
#' @return A named list of ggplot objects, one per species. List names are formatted as "SPEC_##" or "TAXA_##" depending on the species column used. Returns NULL with a warning if no plots are generated.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
plotRVSummary <- function(
    tblList = NULL,
    code = NULL,
    taxa = NULL,
    aphiaid = NULL,
    valueField = "BIOMASS",
    plotUnits = "ton",
    plotTitle = NULL,
    minYear = NULL,
    maxYear = NULL,
    show_gmean = TRUE,
    show_ci = TRUE,
    show_mean = TRUE,
    legend_position = "right"
) {

  tblList <- filterSpecies(tblList = tblList, code = code, aphiaid = aphiaid, taxa = taxa)
  tblList <- aggregateByTaxa(tblList = tblList)
  tblList <- propagateChanges(tblList = tblList)
  
  # Determine species column name
  if ("GSCAT_CONV" %in% names(tblList)) {
    theCat <- tblList$GSCAT_CONV
  } else {
    theCat <- tblList$GSCAT
  }
  
  if (!is.null(taxa)) {
    tblList <- aggregateByTaxa(tblList = tblList, taxa = taxa)
    # message("aggregated!")
  }else{
    # message("did not aggregate!")
  }

  if ("SPEC" %in% names(theCat)) {
    species_col <- "SPEC"
  } else if ("TAXA_" %in% names(theCat)) {
    species_col <- "TAXA_"
    tblList <- aggregateByTaxa(tblList)
  } else {
    stop("Either SPEC or TAXA_ must exist in the data")
  }
  
  # Filter by species if code parameter is provided
  if (!is.null(code)) {
    if ("GSCAT_CONV" %in% names(tblList)) {
      tblList$GSCAT_CONV <- tblList$GSCAT_CONV[tblList$GSCAT_CONV[[species_col]] %in% code, ]
    } else {
      tblList$GSCAT <- tblList$GSCAT[tblList$GSCAT[[species_col]] %in% code, ]
    }
    
    # Check if any data remains after filtering
    remaining_data <- if ("GSCAT_CONV" %in% names(tblList)) {
      nrow(tblList$GSCAT_CONV)
    } else {
      nrow(tblList$GSCAT)
    }
    
    if (remaining_data == 0) {
      stop("No data found for the specified species: ", paste(code, collapse = ", "))
    }
  }
  
  # Stratify the data (returns list by species)
  results_strat <- stratify_simple(
    tblList = tblList,
    totwgt_units = plotUnits
  )
  
  # Get list of species to plot
  species_to_plot <- names(results_strat)
  
  # Create a list to store plots
  plot_list <- list()
  
  # Loop through each species and create a plot
  for (sp_name in species_to_plot) {
    # Extract species code from name (e.g., "SPEC_10" -> "10")
    sp_code <- sub(paste0(species_col, "_"), "", sp_name)
    
    # Extract data for this species
    sp_data <- results_strat[[sp_name]]$results_by_year
    
    # Apply year filters if specified
    if (!is.null(minYear)) {
      sp_data <- sp_data[sp_data$YEAR >= minYear, ]
    }
    if (!is.null(maxYear)) {
      sp_data <- sp_data[sp_data$YEAR <= maxYear, ]
    }
    
    # Check if data exists after filtering
    if (nrow(sp_data) == 0) {
      warning(paste0("No data for ", sp_name, " after year filtering. Skipping plot."))
      next
    }
    

    # Calculate geometric mean if requested
    if (show_gmean) {
      non_zero_values <- sp_data[[valueField]][sp_data[[valueField]] > 0]
      if (length(non_zero_values) > 0) {
        geometric_mean <- exp(mean(log(non_zero_values)))
      } else {
        geometric_mean <- NA
        warning(paste0("No non-zero values found for geometric mean calculation for ", sp_name))
      }
    }
    
    if (grepl(pattern="WGT|MASS", x = valueField, ignore.case = T)){
      plotField = paste0(" (", plotUnits, ")")
    }else{
      plotField = ""
    }
    
    # Create the plot (keeping original styling)
    p <- ggplot2::ggplot(sp_data, ggplot2::aes(x = YEAR, y = .data[[valueField]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = "Year",
        y = paste0(valueField, plotField),
        title = if (!is.null(plotTitle)) {
          paste0(plotTitle, " - ", species_col, ": ", sp_code)
        } else {
          paste0(valueField, " - ", species_col, ": ", sp_code)
        }
      ) +
      ggplot2::theme(legend.position = legend_position)
    
    # Add confidence intervals if requested
    if (show_ci) {
      ci_low_field <- paste0(valueField, "_LOW")
      ci_high_field <- paste0(valueField, "_HIGH")
      
      if (ci_low_field %in% names(sp_data) && ci_high_field %in% names(sp_data)) {
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = .data[[ci_low_field]], ymax = .data[[ci_high_field]]),
          alpha = 0.3,
          fill = "blue"
        )
      }
    }
    
    # Add mean line if requested
    if (show_mean) {
      mean_value <- mean(sp_data[[valueField]], na.rm = TRUE)
      p <- p + ggplot2::geom_hline(
        yintercept = mean_value,
        linetype = "dashed",
        color = "red",
        size = 0.8
      ) +
        ggplot2::annotate(
          "text",
          x = min(sp_data$YEAR),
          y = mean_value,
          label = paste0("Mean: ", round(mean_value, 2)),
          hjust = 0,
          vjust = -0.5,
          color = "red",
          size = 3
        )
    }
    
    # Add geometric mean line if requested
    if (show_gmean && !is.na(geometric_mean)) {
      p <- p + ggplot2::geom_hline(
        yintercept = geometric_mean,
        linetype = "dotted",
        color = "darkgreen",
        size = 0.8
      ) +
        ggplot2::annotate(
          "text",
          x = max(sp_data$YEAR),
          y = geometric_mean,
          label = paste0("G.Mean: ", round(geometric_mean, 2)),
          hjust = 1,
          vjust = -0.5,
          color = "darkgreen",
          size = 3
        )
    }
    
    # Store plot in list with species name as key
    plot_list[[sp_name]]$plot <- p
    plot_list[[sp_name]]$data <- sp_data
  }
  
  # Return list of plots
  if (length(plot_list) == 0) {
    warning("No plots were generated.")
    return(NULL)
  }
  
  return(plot_list)
}