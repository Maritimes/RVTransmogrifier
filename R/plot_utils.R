roundDD2Min<-function(x=NULL, how = "round", nearestMin = 1, digits=4){
  minDD = 0.016666666666 #this is 1 min in DD
  base = nearestMin*minDD
  if (how =="round"){
    res <- base * round(x/base)
  }else if (how =="ceiling"){
    res <- base * ceiling(x/base)
  }else if (how=="floor"){
    res <- base * floor(x/base)
  }
  res <- round(res,digits)
  return(res)
}

getBbox <- function(filterField = "STRATA_ID", filterVals=NULL){
  if(all(filterVals %in% c(396:411))){
    strata <- RVSurveyData::strataMar4VSW_sf 
  }else{
    strata <- RVSurveyData::strataMar_sf
  }
  strata <-  sf::st_transform(strata, crs = 4326)
  if(!is.null(filterField)&& !is.null(filterVals)) strata<-strata[strata[[filterField]] %in% filterVals,]
  bbox <- sf::st_bbox(strata, na.rm=T)
  return(bbox)
}

ggBathy<- function(plotBathy = "FILL", bathyIntervals = 100){
  ggItems<- list()
  if (!is.null(plotBathy)){
    bathyBreaks <- seq(0,-6000, by=-bathyIntervals)
    depths <- grDevices::colorRampPalette(c("#EBF3FB","#215B93"))
    
    if (plotBathy == "FILL"){
      ggItems[["bathy"]] <- ggplot2::geom_contour_filled(data = RVSurveyData::maritimesBathy, ggplot2::aes(x=x, y=y, z=z), breaks=bathyBreaks,  show.legend = F)
      ggItems[["bathy"]] <- c(ggItems[["bathy"]],ggplot2::scale_fill_manual(values=depths(length(bathyBreaks))))
    }else if (plotBathy == "LINES"){
      ggItems[["bathy"]] <- ggplot2::geom_contour(data = RVSurveyData::maritimesBathy, ggplot2::aes(x=x, y=y, z=z), alpha= 2/3, breaks=seq(-bathyIntervals,-6000, by=-bathyIntervals), size=c(0.6), colour="steelblue", show.legend = T)
    }
  } else{
    ggItems[["bathy"]] <- NULL
  }
  return(ggItems)
}

ggNAFO <- function(plotNAFO = TRUE, plotLabels = TRUE, filter= NULL){
  ggItems<- list()
  nafo <- RVSurveyData::nafo_sf
  nafo <- sf::st_transform(nafo, crs = 4326)
  if (plotNAFO){
    ggItems[["nafo"]] <- ggplot2::geom_sf(data = nafo,fill=NA, color="#88888833")
    if(plotLabels) ggItems[["nafo"]] <- c(ggItems[["nafo"]],ggplot2::geom_sf_text(data = nafo,ggplot2::aes(label = NAFO), fun.geometry = sf::st_centroid, fontface = "italic", color="#88888880", size = 4))
  }else{
    ggItems[["nafo"]] <- NULL
  }
  #filter is not yet sorted out
  return(ggItems)
}

ggStrata <- function(plotStrata=TRUE, plotLabels=TRUE, filter=NULL){
  ggItems<- list()
  if(all(filter %in% c(396:411))){
    strata <- RVSurveyData::strataMar4VSW_sf 
  }else{
    strata <- RVSurveyData::strataMar_sf
  }
  strata <- sf::st_transform(strata, crs = 4326)
  if(!is.null(filter)) strata<-strata[strata$STRATA_ID %in% filter,]
  if (plotStrata){
    ggItems[["strata"]] <- ggplot2::geom_sf(data = strata,fill=NA, color="#00669933")
    if(plotLabels) {
      strata<- sf::st_set_geometry(strata, 'centroids')
      ggItems[["strata"]] <- c(ggItems[["strata"]],ggplot2::geom_sf_text(data = strata,ggplot2::aes(label = STRATA_ID), fontface = "italic", color="#88888880", size = 4))##00669933
    }
  }else{
    ggItems[["strata"]] <- NULL
  }
  return(ggItems)
}
ggCatchPts <- function(catchdata = NULL, sizeVar=NULL, colourVar = NULL, return=NULL){
  ggItems<- list()
  if (return=="CATCHES"){
    if(length(colourVar)>1){
      ggItems[["catchPts"]] <- suppressWarnings({ggplot2::geom_point(data=catchdata[!is.na(catchdata[,sizeVar]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = sizeVar, colour=colourVar), alpha = 0.6)})
      ggItems[["catchPts"]] <- c(ggItems[["catchPts"]],ggplot2::scale_color_discrete(name=colourVar[1], labels=paste0(catchdata[!is.na(catchdata[,sizeVar]),colourVar[1]]," (",catchdata[!is.na(catchdata[,sizeVar]),colourVar[2]],")")))
    }else{
      ggItems[["catchPts"]] <- suppressWarnings({ggplot2::geom_point(data=catchdata[!is.na(catchdata[,sizeVar]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = sizeVar, colour=colourVar), alpha = 0.6)})
    }
    ggItems[["catchPts"]] <- c(ggItems[["catchPts"]], ggplot2::scale_size_continuous(range = c(2,8)))
  }
  if (return=="NULLSETS"){
    ggItems[["catchPts"]] <- suppressWarnings({ggplot2::geom_point(data=catchdata[is.na(catchdata[,sizeVar]),], ggplot2::aes(SLONG_DD, SLAT_DD),  shape=3, size= 2, color="black")})
  }
  if (return=="ALLSETS"){
    ggItems[["catchPts"]] <- suppressWarnings({ggplot2::geom_point(data=catchdata, ggplot2::aes(SLONG_DD, SLAT_DD), size= 2, color="#333333CC")})
  }
  return(ggItems)
}

ggStrataData <- function(catchStrataData = NULL, plotField = NULL, filter=NULL){
  if ("SPEC" %in% names(catchStrataData)){
    plotLabel <- paste0("SPEC: ",catchStrataData$SPEC[1])
  } else if ("TAXA_" %in% names(catchStrataData)){
    plotLabel <- catchStrataData$TAXA_[1]
  }
  if (is.null(filter)){
    strata <- RVSurveyData::strataMar_sf
  }
  if(all(filter %in% c(396:411))){
    strata <- RVSurveyData::strataMar4VSW_sf 
  }else{
    strata <- RVSurveyData::strataMar_sf
  }
  strata <- sf::st_transform(strata, crs = 4326)
  catchStrataData <- merge(strata, catchStrataData, by.x="STRATA_ID", by.y="STRAT")
  ggItems<- list()
  ggItems[["ggStratData"]] <- ggplot2::geom_sf(data = catchStrataData, ggplot2::aes_string(fill=plotField))
  ggItems[["ggStratData"]] <- c(ggItems[["ggStratData"]],ggplot2::scale_fill_continuous(name = paste0(plotField, "\n(",plotLabel,")"), direction = -1,type = "viridis",na.value = NA))
  return(ggItems)
}