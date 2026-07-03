#' @title aggregateByTaxa
#' @description This function .
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param ... other arguments passed to methods (e.g. 'debug' and 'quiet')
#' @returns #' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
aggregateByTaxa <- function(tblList = NULL, ...) {
  args <- list(...)
  # #All data arranged by code, so no need to aggregate if code was specified
  if (!is.null(args$code)) {
    message("No aggregation to be done")
    return(tblList)
  }else{
    message("aggregateByTaxa running")
  }
  tblList$GSCAT$SPEC <- tblList$GSDET$SPEC <- NULL
  tblList$GSCAT <- tblList$GSCAT |>
    dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT, -SAMPWGT))) |>
    dplyr::summarise(
      SAMPWGT = sum(SAMPWGT),
      TOTWGT = sum(TOTWGT),
      TOTNO = sum(TOTNO),
      .groups = "keep"
    ) |>
    as.data.frame()

  tblList$GSDET <- tblList$GSDET |>
    dplyr::group_by(dplyr::across(c(-CLEN, -FWT))) |>
    dplyr::summarise(
      CLEN = sum(CLEN), 
      FWT = sum(FWT), 
      .groups = "keep") |>
    as.data.frame()
  
  if ("GSCAT_CONV" %in% names(tblList)) {
    tblList$GSCAT_CONV$SPEC <- tblList$GSDET_CONV$SPEC <- NULL
    
    tblList$GSCAT_CONV <- tblList$GSCAT_CONV |>
      dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT, -SAMPWGT_OG, -TOTNO_OG, -TOTWGT_OG))) |>
      dplyr::summarise(
        TOTWGT = sum(TOTWGT),
        TOTNO = sum(TOTNO),
        SAMPWGT_OG = sum(SAMPWGT_OG),
        TOTWGT_OG = sum(TOTWGT_OG),
        TOTNO_OG = sum(TOTNO_OG),
        .groups = "keep"
      ) |>
      as.data.frame()
    
    tblList$GSDET_CONV <- tblList$GSDET_CONV |>
      dplyr::group_by(dplyr::across(c(-CLEN, -FWT, -CLEN_OG, -FWT_OG))) |>
      dplyr::summarise(
        CLEN = sum(CLEN), 
        FWT = sum(FWT),
        CLEN_OG = sum(CLEN_OG), 
        FWT_OG = sum(FWT_OG), 
        .groups = "keep") |>
      as.data.frame()
  } 
  
  return(tblList)
}
