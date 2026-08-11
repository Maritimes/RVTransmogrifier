#' @title easyFlatten
#' @description Merge GSINF, GSCAT, and GSSTRATUM tables into a single flat data frame for analysis. If conversion
#' factors have been applied (i.e. GSCAT_CONV exists), they will be used, rather than GSCAT.  This function Handles
#' both species-level (SPEC) and taxa-level (TAXA_) data.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, and GSSTRATUM.
#' @param keep_nullsets the default is \code{TRUE}. If TRUE, retains set information even when no catch occurred.
#' @return A merged data frame containing set, catch, and stratum information
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
easyFlatten <- function(tblList = NULL, keep_nullsets = T) {
  theFields <- c("MISSION", "SETNO", "TOTWGT", "TOTNO")
  if ("GSCAT_CONV" %in% names(tblList)) {
    theCat <- tblList$GSCAT_CONV
  } else {
    theCat <- tblList$GSCAT
  }
  if ("SPEC" %in% names(theCat)) {
    theFields <- c(theFields, "SPEC")
    species_col <- "SPEC"
  } else if ("TAXA_" %in% names(theCat)) {
    theFields <- c(theFields, "TAXA_", "TAXARANK_")
    species_col <- "TAXA_"
  }else {
    stop("Either SPEC or TAXA_ must exist in GSCAT")
  }

  all_sets <- tblList$GSINF[, c("MISSION", "SETNO", "STRAT", "DIST", 
                                "TYPE", "GEAR", "DEPTH", "AREA_KM2", 
                                "WINGSPREAD_FT")]
  # Get all unique species from GSCAT
  if (species_col == "SPEC") {
    all_species <- unique(theCat[!is.na(theCat$SPEC), "SPEC", drop = FALSE])
  } else {
    all_species <- unique(theCat[!is.na(theCat$TAXA_), 
                                 c("TAXA_", "TAXARANK_"), drop = FALSE])
  }
  
  # Create complete grid: every set × every species
  if (keep_nullsets) {
    complete_grid <- merge(all_sets, all_species)
    
    # Merge with actual catch data
    this <- merge(
      complete_grid,
      theCat[, theFields],
      by = c("MISSION", "SETNO", species_col),
      all.x = TRUE
    )
  } else {
    # Just merge what exists
    this <- merge(
      tblList$GSINF,
      theCat[, theFields],
      by = c("MISSION", "SETNO"),
      all.x = FALSE
    )
  }
  
  return(this)
}
