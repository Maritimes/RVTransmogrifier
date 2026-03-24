

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
