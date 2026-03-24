#If strata includes 5z3, 5z4, 5z8, 5Z5, 5z6, 5z7, likely should include areas 523 and 524
stock_map <- list(
  GAMO_4Vn =                         list(code =   10, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:442), 
                                          spec_name = "Atlantic Cod", 
                                          stock_name = "Atlantic Cod  NAFO 4Vn (Resident)"),
  GAMO_4VsW =                        list(code =   10, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(443:466), 
                                          spec_name = "Atlantic Cod", 
                                          stock_name = "Atlantic Cod  NAFO 4VsW (Eastern Scotian Shelf)"),
  GAMO_4X5Y =                        list(code =   10, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Atlantic Cod", 
                                          stock_name = "Atlantic Cod  NAFO 4X5Y (Scotian Shelf and Bay of Fundy)"),
  GAMO_5Zjm =                        list(code =   10, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Atlantic Cod", 
                                          stock_name = "Atlantic Cod  NAFO 5Zjm (Eastern Georges Bank)"),
  MEAE_4X5Y =                        list(code =   11, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Haddock", 
                                          stock_name = "Haddock  NAFO 4X5Y"),
  MEAE_4TVW =                        list(code =   11, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Haddock", 
                                          stock_name = "Haddock 4TVW (*Eastern Scotian Shelf)"),
  MEAE_5Zjm =                        list(code =   11, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), spec_name = "Haddock", 
                                          stock_name = "Haddock  NAFO 5Zjm"),
  URTE_4VW =                         list(code =   12, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "White Hake", 
                                          stock_name = "White Hake  NAFO 4VW (Eastern Scotian Shelf)"),
  URTE_4X5Zc =                       list(code =   12, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "White Hake", 
                                          stock_name = "White Hake  NAFO 4X5Zc (Western Scotian Shelf, Bay of Fundy and Northern Georges Bank)"),
  MEBI_4VWX =                        list(code =   14, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:483), 
                                          spec_name = "Silver Hake", 
                                          stock_name = "Silver Hake  NAFO 4VWX (X doesn't include BoF)"),
  MEBI_4Xwest =                      list(code =   14, 
                                          months = c(5, 6, 7, 8), strata = c(484:495), 
                                          spec_name = "Silver Hake", 
                                          stock_name = "Silver Hake  NAFO 4X West (BoF)"),
  POVI_4VWXmn =                      list(code =   16, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:473, 475, 477, 478), 
                                          spec_name = "Pollock", 
                                          stock_name = "Pollock  NAFO 4VWXmn (Eastern Component)"),
  POVI_4X5 =                         list(code =   16, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(474, 476, 480:495, "5Z1", "5Z2", "5Z9"), 
                                          spec_name = "Pollock", 
                                          stock_name = "Pollock  NAFO 4X5 (Western Component)"),
  SEFA_4VW =                         list(code =   23, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:456, 464), spec_name = "Redfish", 
                                          stock_name = "Redfish Component of Units 1 and 2"),
  SEFA_Un3 =                         list(code =   23, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(456, 458:495), 
                                          spec_name = "Redfish", 
                                          stock_name = "Redfish Unit 3"),
  HIHI_3NOPs4VWX_5 =                 list(code =   30, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466, 470:495), 
                                          spec_name = "Atlantic Halibut", 
                                          stock_name = "Atlantic Halibut - NAFO 3NOPs4VWX+5"),
  HIPL_4X =                          list(code =   40, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "American Plaice", 
                                          stock_name = "American Plaice - NAFO 4X"),
  HIPL_4VW =                         list(code =   40, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "American Plaice", 
                                          stock_name = "American Plaice - NAFO 4VW (Scotian Shelf)"),
  GLCY_4X =                          list(code =   41, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Witch Flounder", 
                                          stock_name = "Witch flounder NAFO 4X"),
  GLCY_4VW =                         list(code =   41, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Witch Flounder", 
                                          stock_name = "Witch flounder NAFO 4VW"),
  PSAM_4X =                          list(code =   43, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Winter Flounder", 
                                          stock_name = "Winter Flounder - NAFO 4X"),
  PSAM_4VW =                         list(code =   43, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Winter Flounder", 
                                          stock_name = "Winter Flounder - NAFO 4VW"),
  LIFE_4X =                          list(code =   42, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Yellowtail Flounder", 
                                          stock_name = "Yellowtail Flounder - NAFO 4X"),
  LIFE_4VW =                         list(code =   42, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Yellowtail Flounder", 
                                          stock_name = "Yellowtail Flounder - NAFO 4VW"),
  LIFE_5Z =                          list(code =   42, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4"), 
                                          areas = c(523, 524), 
                                          spec_name = "Yellowtail Flounder", 
                                          stock_name = "Yellowtail Flounder - NAFO 5Z"),
  DILA_4X =                          list(code =  200, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Barndoor Skate", 
                                          stock_name = "Barndoor Skate - NAFO 4X"),
  DILA_4VW =                         list(code =  200, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Barndoor Skate", 
                                          stock_name = "Barndoor Skate - NAFO 4VW"),
  DILA_5Z =                          list(code =  200, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Barndoor Skate", 
                                          stock_name = "Barndoor Skate - NAFO 5Z"),
  LEER_4X =                          list(code =  203, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Little Skate", 
                                          stock_name = "Little Skate NAFO 4X"),
  LEER_4VW =                         list(code =  203, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Little Skate", 
                                          stock_name = "Little Skate NAFO 4VW"),
  LEER_5Z =                          list(code =  203, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Little Skate", 
                                          stock_name = "Little Skate  NAFO 5Z"),
  MASE_4X =                          list(code =  202, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Smooth Skate", 
                                          stock_name = "Smooth Skate  NAFO 4X"),
  MASE_4VW =                         list(code =  202, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Smooth Skate", 
                                          stock_name = "Smooth Skate  NAFO 4VW"),
  MASE_5Z =                          list(code =  202, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Smooth Skate",
                                          stock_name = "Smooth Skate  NAFO 5Z"),
  AMRA_4X =                          list(code =  201, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Thorny Skate", 
                                          stock_name = "Thorny Skate  NAFO 4X"),
  AMRA_4VW =                         list(code =  201, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Thorny Skate", 
                                          stock_name = "Thorny Skate  NAFO 4VW"),
  AMRA_5Z =                          list(code =  201, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Thorny Skate", 
                                          stock_name = "Thorny Skate  NAFO 5Z"),
  LEOC_4X =                          list(code =  204, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Winter Skate", 
                                          stock_name = "Winter Skate  NAFO 4X"),
  LEOC_4VW =                         list(code =  204, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Winter Skate", 
                                          stock_name = "Winter Skate  NAFO 4VW"),
  LEOC_5Z =                          list(code =  204, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Winter Skate", 
                                          stock_name = "Winter Skate  NAFO 5Z"),
  ANLU_4X =                          list(code =   50, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Striped Atlantic Wolffish", 
                                          stock_name = "Atlantic Wolffish 4X"),
  ANLU_4VW =                         list(code =   50, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Striped Atlantic Wolffish", 
                                          stock_name = "Atlantic Wolffish 4VW"),
  LOAM_4X =                          list(code =  400, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Monkfish", 
                                          stock_name = "Monkfish  NAFO 4X"),
  LOAM_4VW =                         list(code =  400, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Monkfish", 
                                          stock_name = "Monkfish NAFO 4VW"),
  MYOC_4X =                          list(code =  300, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Longhorn sculpin", 
                                          stock_name = "Longhorn Sculpin NAFO 4X"),
  MYOC_4VW =                         list(code =  300, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Longhorn sculpin", 
                                          stock_name = "Longhorn Sculpin NAFO 4VW"),
  Longhorn_Sculpin_5Z =              list(code =  300, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Longhorn sculpin", 
                                          stock_name = "Longhorn Sculpin 5Z"),
  SQA =                              list(code =  220, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466, 470:495), 
                                          spec_name = "Spiny dogfish", 
                                          stock_name = "Spiny Dogfish NAFO 4VWNX+5 (Atlantic)"),
  URCH_4X =                          list(code =   13, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Red Hake", 
                                          stock_name = "Red Hake NAFO 4X"),
  URCH_4VW =                         list(code =   13, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Red Hake", 
                                          stock_name = "Red Hake NAFO 4VW"),
  HEAM_4X =                          list(code =  320, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Sea Raven", 
                                          stock_name = "Sea Raven NAFO 4X"),
  HEAM_4VW =                         list(code =  320, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Sea Raven", 
                                          stock_name = "Sea Raven NAFO 4VW"),
  ZOAM_4X =                          list(code =  640, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(470:495), 
                                          spec_name = "Ocean Pout", 
                                          stock_name = "Ocean Pout NAFO 4X"),
  ZOAM_4VW =                         list(code =  640, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466), 
                                          spec_name = "Ocean Pout", 
                                          stock_name = "Ocean Pout NAFO 4VW"),
  Ocean_Pout_NAFO_5Z =               list(code =  640, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Ocean Pout", 
                                          stock_name = "Ocean Pout NAFO 5Z"),
  HEDA_4VWX5c =                      list(code =  123, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466, 470:495), 
                                          spec_name = "Blackbelly Rosefish", 
                                          stock_name = "Blackbelly Rosefish NAFO 4VWX5c"),
  ZEOC_4VWX5c =                      list(code =  704, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466, 470:495), 
                                          spec_name = "American John Dory", 
                                          stock_name = "American John Dory NAFO 4VWX5c"),
  ILIL_34 =                          list(code = 4511, 
                                          months = c(5, 6, 7, 8), 
                                          strata = c(440:466, 470:495), 
                                          spec_name = "Northern Shortfin Squid", 
                                          stock_name = "Northern Shortfin Squid NAFO 3 + 4"),
  American_Lobster =                 list(code = 2550, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "American Lobster", 
                                          stock_name = "American Lobster"),
  Juvenile_Little_and_Winter_Skate = list(code = 1191, 
                                          months = c(1, 2, 3, 4), 
                                          strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), 
                                          areas = c(523, 524), 
                                          spec_name = "Juvenile Little and Winter Skate", 
                                          stock_name = "Juvenile Little and Winter Skate 5Z")
)

#' Retrieve stock information
#'
#' This function retrieves detailed information about a specific stock from the `stock_map`.
#'
#' @param stock A string representing the name of the stock. This must be one of the names in the `stock_map`.
#'
#' @return A list of stock details, including `code`, `months`, `strata`, `spec_name`, and `stock_name`.
#' @export
#'
#' @examples
#' # Example: Retrieve information about the "GAMO_4Vn" stock
#' stock_lookup("GAMO_4Vn")
stock_lookup <- function(stock) {
  if (!stock %in% names(stock_map)) {
    stop("Invalid stock")
  }
  stock_map[[stock]]
}

#' Get a list of all stock names
#'
#' This function returns all the stock names (keys) in the `stock_map` object.
#'
#' @return A character vector of stock names.
#' @export
#'
#' @examples
#' # Get all available stock names
#' stock_list()
stock_list <- function() {
  names(stock_map)
}

#' Search for stocks by a keyword
#'
#' This function searches for stocks whose `spec_name` or `stock_name` contains a specified pattern.
#'
#' @param pattern A string pattern to search for (case-insensitive).
#'
#' @return A character vector of matching stock names, formatted as "Element_Name (Stock_Name)". 
#' Returns `NULL` if no match is found.
#' @export
#'
#' @examples
#' # Search for stocks containing "Halibut" in their `spec_name` or `stock_name`
#' stock_search("Halibut")
#'
#' # Search for stocks containing "Skate"
#' stock_search("Skate")
stock_search <- function(pattern) {
  matches <- names(stock_map)[
    sapply(stock_map, function(entry) {
      grepl(pattern, entry$spec_name, ignore.case = TRUE) ||
        grepl(pattern, entry$stock_name, ignore.case = TRUE)
    })
  ]
  
  if (length(matches) == 0) {
    return(NULL)
  }
  
  formatted_results <- unname(sapply(matches, function(match) {
    stock_name <- stock_map[[match]]$stock_name
    paste0(match, " (", stock_name, ")")
  }))
  
  return(formatted_results)
}

#' Validate and retrieve stock information
#'
#' This function checks whether a given stock name is valid. If valid, it returns the stock's details. 
#' If invalid, it tries to find the closest matches (based on approximate matching) in the stock names, 
#' `spec_name`, or `stock_name`. If no close matches are found, an error is raised.
#'
#' @param stock A string representing the stock name to validate.
#'
#' @return A list of stock details if the input is valid. If the input is invalid, an error is raised 
#' with suggestions for similar stock names, if available.
#' @export
#'
#' @examples
#' # Validate a known stock name
#' stock_validate("GAMO_4Vn")
#'
#' # Attempt to validate an incorrect stock name
#' # stock_validate("GAMO_4VX")  # Returns error: "Did you mean: GAMO_4Vn?"
stock_validate <- function(stock) {
  if (stock %in% names(stock_map)) {
    return(stock_map[[stock]])
  }
  # Find approximate name matches
  closest_matches_name <- agrep(
    stock,
    names(stock_map),
    value = TRUE,
    max.distance = 0.2
  )
  
  # Find approximate matches in spec_name or stock_name
  closest_matches_attrs <- names(stock_map)[
    sapply(stock_map, function(entry) {
      any(agrep(
        stock,
        c(entry$spec_name, entry$stock_name),
        value = FALSE,
        max.distance = 0.2
      ))
    })
  ]
  # Combine and deduplicate matches
  all_matches <- unique(c(closest_matches_name, closest_matches_attrs))
  
  if (length(all_matches) > 0) {
    stop(paste(
      "Invalid stock. Did you mean:",
      paste(all_matches, collapse = ", ")
    ))
  } else {
    stop("Invalid stock. No similar stock names found.")
  }
}
# ILIL_34 <- lookup_stock("ILIL_34")
# test <- loadRVData(cxn = NULL, code = ILIL_34$code, years= c(2014), months=ILIL_34$months, strata=ILIL_34$strata, types=1)
# plotRV(tblList = test, plotSets = "TOTNO")
#
# Ocean_Pout_NAFO_5Z <- lookup_stock("Ocean_Pout_NAFO_5Z")
# test <- loadRVData(cxn = NULL, code = Ocean_Pout_NAFO_5Z$code, years= c(2014), months=Ocean_Pout_NAFO_5Z$months, strata=Ocean_Pout_NAFO_5Z$strata, areas=Ocean_Pout_NAFO_5Z$areas, types=1)
# plotRV(tblList = test, plotSets = "TOTNO")
