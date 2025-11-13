#' @title aggregateByTaxa
#' @description This function .
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param ... other arguments passed to methods (e.g. 'debug' and 'quiet')
#' @returns #' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
aggregateByTaxa <- function(tblList = NULL,...){

  args <- list(...)
  # #All data arranged by code, so no need to aggregate if code was specified
  if(!is.null(args$code))return(tblList)
  tblList$GSCAT$SPEC <- tblList$GSDET$SPEC <- NULL
  tblList$GSCAT <- tblList$GSCAT |>
    dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT))) |>
    dplyr::summarise(TOTWGT=sum(TOTWGT),
                     TOTNO=sum(TOTNO),
                     .groups = "keep")|>
    as.data.frame()

  tblList$GSDET <- tblList$GSDET |>
    dplyr::group_by(dplyr::across(c(-CLEN))) |>
    dplyr::summarise(CLEN=sum(CLEN),
                     .groups = "keep")|>
    as.data.frame()
  
  return(tblList)
}
