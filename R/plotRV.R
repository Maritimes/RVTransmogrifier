#' @title plotRV
#' @description This function can plot the list data resulting from \code{get_survey()} or from 
#' \code{propagateChanges()}.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param plotSets the default is \code{"TOTWGT"}, but can also be \code{"TOTWGT"}.  If set to \code{NULL},
#' no catch data points will be plotted.
#' @param plotNullSets   the default is \code{TRUE}.  If TRUE, sets where the specified species was not
#' caught will be plotted as '+'.  If FALSE, null sets will not be plotted.
#' @param plotCatchStrata the default is \code{"MEAN_WGT"}, but can be any of \code{"TOTNO"}, \code{"TOTWGT"}, 
#' \code{"MEAN_WGT"}, \code{"MEAN_NO"}, \code{"BIOMASS"}, or \code{"ABUND"}. If set to \code{NULL},
#' no strata will be plotted by catch.
#' @param catchStrataData the default is \code{NULL}. This is a stratified object resulting from running 
#' \code{stratify}.  \code{stratify()} can generate multiple dfs within a list, but this parameter should
#' be pointed to a single dataframe.
#' @param plotStrata the default is \code{TRUE}. Should Maritimes strata be shown?
#' @param labelStrata the default is \code{TRUE}. If they are shown, should Maritimes strata be labelled?
#' @param plotNAFO the default is \code{TRUE}. Should NAFO areas be shown?
#' @param labelNAFO the default is \code{TRUE}. If they are shown, should NAFO areas be labelled?
#' @param plotBathy the default is \code{"FILL"}, but can be any of \code{"FILL"}, \code{"LINES"} or \code{NULL}.  
#' FILL results in progressively darker blue areas as depth increases. 
#' LINES results in contour lines.
#' NULL does not show any bathymetric information.
#' @param bathyIntervals the default is \code{100} (m).  This is the difference in depth between contour lines.
#' @param ... other arguments passed to methods (e.g.'taxa', 'code', 'aphiaid', debug' and 'quiet')
#' @returns a ggplot object showing the positions of the data.  Null sets will be shown as + signs.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
plotRV <- function(tblList = NULL, 
                   plotSets = "TOTWGT", plotNullSets = TRUE,
                   plotCatchStrata = "MEAN_WGT", catchStrataData = NULL,
                   plotStrata = TRUE,labelStrata=TRUE,
                   plotBathy = "FILL", bathyIntervals=200,
                   plotNAFO=FALSE, labelNAFO = FALSE, ...){
  args <- list(...)
  if (dev.cur() == 0) png(filename = "temp.png", width = 800, height = 600)

  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)
  
  if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
    tblList      <- filterSpecies(tblList,
                                  taxa = args$taxa,
                                  code = args$code,
                                  aphiaid = args$aphiaid)
    if (inherits(tblList,"numeric"))stop("Requested filter removed all species")
    tblList      <- aggregateByTaxa(tblList = tblList,
                                    code = args$code)
  }
  
  if (!is.null(tblList) & !is.null(plotSets)) {
    #strata are used to set plot bounds (filtered to sampled strata (from GSINF))
    limits1 <- sort(getBbox(filterVals = unique(tblList$GSINF$STRAT)))
  }
  
  if (!is.null(catchStrataData)) {
    if (inherits(catchStrataData,"list") & length(catchStrataData)>1){
      message("catchStrataData can contain multiple data frames, but only the first of these will be plotted (i.e.",names(catchStrataData[1]),").  To plot a different one, please set catchStrataData to the specific data frame you want to plot")
      catchStrataData <- catchStrataData[[1]]
    } else if (inherits(catchStrataData,"list")){
      catchStrataData <- catchStrataData[[1]]
    }
    
    if (is.null(plotSets)) limits1 <- sort(getBbox(filterVals = unique(catchStrataData$STRAT)))
    #if we have stratified catch data, we can't have filled contours
    if(plotBathy == "FILL")plotBathy <- "LINES"
  }
  
  limits <- c(roundDD2Min(limits1[1],nearestMin=15, how = "floor"), 
              roundDD2Min(limits1[2],nearestMin=15, how = "ceiling"), 
              roundDD2Min(limits1[3],nearestMin=15, how = "floor"), 
              roundDD2Min(limits1[4],nearestMin=15, how = "ceiling"))
  
  #add plot elements to this list.
  ggItems <- list()
  sf::sf_use_s2(FALSE)
  #set up the basic plot
  
  p <-ggplot2::ggplot() + ggplot2::theme_bw()
  # ggItems[["bathy"]]      <- ggBathy(plotBathy=plotBathy, bathyIntervals=bathyIntervals)
  ggItems[["bkgdStrata"]] <- suppressWarnings({ggStrata(plotStrata=plotStrata, plotLabels=labelStrata, filter=unique(tblList$GSINF$STRAT))})
   ggItems[["bkgdNAFO"]]   <- suppressWarnings({ggNAFO(plotNAFO=plotNAFO, plotLabels=labelNAFO, filter=NULL)})
  if (!is.null(catchStrataData) & plotCatchStrata %in% c("TOTNO", "TOTWGT", "MEAN_WGT", "MEAN_NO", "BIOMASS",  "ABUNDANCE")){
    #if (plotCatchStrata == "BIOMASS")plotCatchStrata="BIOMASS_T"
    #   #can't plot bkgrd strata if the strata are to be plotted by catch
    ggItems[["bkgdStrata"]] <- NULL
    ggItems[["gg_stratData"]] <- suppressWarnings({ggStrataData(catchStrataData = catchStrataData, plotField = plotCatchStrata, filter=unique(tblList$GSINF$STRAT))})
  }else{
    ggItems[["gg_stratData"]]<- NULL
  } 

  #ensure specified variables are plottable
  if (is.null(plotSets)){
    #don't plot sets
    catLeg<- NULL
  }else if(!is.null(plotSets) & plotSets %in% c("TOTNO", "TOTWGT", "ALL")){
    catches <- tblList$GSCAT
    if ("SPEC" %in% names(catches)){
      catches <- merge(catches, tblList$GSSPECIES_NEW[,c("CODE","SPEC", "COMM", "APHIA_ID")], by.x="SPEC", by.y="CODE")
      colnames(catches)[colnames(catches)=="SPEC.y"] <- "SCI"
      t_field <- c("SCI", "SPEC")
    }else{
      #if 2 fields sent, the format will be "t_field1 (t_field2)"
      t_field <- "TAXA_"
    }

    catches <- merge(catches, tblList$GSINF[,c("MISSION", "SETNO",'SLONG_DD', 'SLAT_DD')], by=c("MISSION", "SETNO"),all.y=T)

    
    if (plotSets %in% c("TOTNO", "TOTWGT")) {
      catLeg <- ifelse(plotSets == "TOTWGT", "Total Weight (kgs)","Total Number")
      ggItems[["catchPts"]] <- suppressWarnings({ggCatchPts(catchdata = catches, sizeVar=plotSets, colourVar = t_field, return="CATCHES")})
      if (plotNullSets)  ggItems[["nullSets"]]   <- suppressWarnings({ggCatchPts(catchdata = catches, sizeVar="TOTNO", colourVar = t_field[1], return="NULLSETS")})
      
    }else if (plotSets == "ALL"){
      catLeg <- NULL
      ggItems[["allPts"]] <- suppressWarnings({ggCatchPts(catchdata = catches, sizeVar="TOTNO", colourVar = NULL, return="ALLSETS")})
    }
     ggItems[["catchLabels"]] <- ggplot2::guides(color = ggplot2::guide_legend(title = "Taxa", order=1), size = ggplot2::guide_legend(title = catLeg, order=2))
  }
  
  ggItems[["land"]]   <- ggplot2::geom_sf(data = Mar.data::coast_lores_sf, fill = "darkgrey", color = NA)
  ggItems[["extent"]] <- ggplot2::coord_sf(xlim = c(limits[1],limits[2]), ylim = c(limits[3], limits[4]))
  ggItems[["labels"]] <- suppressWarnings({ggplot2::labs(x="Longitude", y="Latitude")})
  p<-p+ggItems
  if (exists("temp_device")) dev.off()
  print(p)
  invisible(p)
}