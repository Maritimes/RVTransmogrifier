#' @title extractFGP
#' @description This function generates the products (i.e. csvs and shapefiles)used by the FGP
#' services related to https://open.canada.ca/data/en/dataset/8ddcaeea-b806-4958-a79f-ba9ab645f53b.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are "SPRING", "SUMMER", "FALL", and "4VSW".  A value of NULL will result in products being
#' generated for all 4 different surveys.
#' @param years the default is \code{NULL}. This parameter allows you to generate datasets for one or
#' more specific years.  A value of NULL will result in products being generated for all years for
#' which data exists, and a vector of years will result in dataset that include the specified years.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
extractFGP <- function(cxn=NULL, survey = NULL, years=NULL, path =NULL, debug = FALSE){
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Created directory: ", path)
  }
  
  ts <-  format(Sys.time(), "%Y")
  
  if (is.null(path))path<-getwd()
  
  if (is.null(survey)){
    survey <- c("SPRING", "SUMMER", "FALL", "4VSW")
  }else{
    survey <- toupper(survey)
  }
  for (i in 1:length(survey)){
    if (is.null(years)){
      fn <- paste0(survey[i],"_",ts)
    }else{
      fn <- paste0(years[1],"_",survey[i],"_",ts)
    }
    
    this <- loadRVData(cxn=cxn, survey = survey[i], years = years, types = 1, debug = debug)
    if (is.numeric(this))stop("Your query did not return valid results")
    
    this$GSINF <- merge(this$GSINF, this$GSGEAR, all.x = T)
    this$GSINF$GEAR <- NULL
    
    # replace maturity code with maturity desc
    this$GSDET_DETS <- merge(this$GSDET_DETS, this$GSMATURITY, all.x = T, by.x="FMAT", by.y="CODE")
    colnames(this$GSDET_DETS)[colnames(this$GSDET_DETS)=="DESCRIPTION"] <- "MATURITY"
    
    # replace sex code with sex desc
    this$GSDET_DETS <- merge(this$GSDET_DETS, this$GSSEX, all.x = T, by.x="FSEX", by.y="CODE")
    colnames(this$GSDET_DETS)[colnames(this$GSDET_DETS)=="DESCRIPTION"] <- "SEX"
    
    #grab the depths (in fathoms).  if no value for DEPTH, average dmin and dmax, take the result, and convert to meters
    this$GSINF$DEPTH_M <- NA
    this$GSINF$DEPTH_M <- rowMeans(this$GSINF[,c("DMIN","DMAX")], na.rm = F) #first do average
    this$GSINF[!is.na(this$GSINF$DEPTH),"DEPTH_M"]<- this$GSINF[!is.na(this$GSINF$DEPTH),"DEPTH"] #overwrite w depth, where avail
    this$GSINF$DEPTH_M <- fathomsToMeters(this$GSINF$DEPTH_M)
    
    this$GSMISSIONS <- this$GSMISSIONS |> 
      dplyr::select(MISSION, VESEL,	CRUNO, YEAR, SEASON)
    
    this$GSINF <- this$GSINF |>
      dplyr::mutate(SDATE = as.Date(SDATE)) |>
      dplyr::rename(
        SURF_TEMP = SURFACE_TEMPERATURE,
        BOTT_TEMP = BOTTOM_TEMPERATURE,
        BOTT_SAL = BOTTOM_SALINITY
      ) |>
      dplyr::select(
        MISSION, SETNO, SDATE, TIME, STRAT, SLAT_DD, SLONG_DD, ELAT_DD, ELONG_DD,
        DUR, DIST, SPEED, DEPTH_M, SURF_TEMP, BOTT_TEMP, BOTT_SAL, GEARDESC
      ) |>
      dplyr::rename(DEPTH = DEPTH_M) |> 
      dplyr::arrange(MISSION, SETNO)
    
    this$GSCAT      <-  this$GSCAT  |> 
      dplyr::select(MISSION,	SETNO,	SPEC,	TOTWGT,	TOTNO) |> 
      dplyr::arrange(MISSION,	SETNO)
    
    this$GSDET_LF <- this$GSDET_LF |> 
      dplyr::select(MISSION, SETNO, SPEC, FSEX, FLEN, CLEN) |> 
      dplyr::arrange(MISSION, SETNO)
    
    this$GSDET_DETS      <- this$GSDET_DETS |> 
      dplyr::select(MISSION, SETNO, SPEC, FLEN, FWT, MATURITY, SEX, AGE, SPECIMEN_ID) |> 
      dplyr::arrange(MISSION, SETNO)
    
    
    this$GSSPECIES_NEW  <- this$GSSPECIES_NEW |> 
      dplyr::rename(SCI_NAME = SPEC) |> 
      dplyr::select(CODE, SCI_NAME,	COMM,	APHIA_ID) |> 
      dplyr::arrange(CODE)
    
    Mar.utils::df_sf_to_gpkg(df=this$GSINF,lat.field = "SLAT_DD",lon.field = "SLONG_DD",
                             gpkgName = "FGP.gpkg",
                             path = path,
                             layerName = paste0(fn,"_FGP"))
    
    utils::write.csv(this$GSMISSIONS, file = file.path(path,paste0(fn,"_GSMISSIONS.csv")), row.names = F)
    utils::write.csv(this$GSINF, file = file.path(path,paste0(fn,"_GSINF.csv")), row.names = F)
    utils::write.csv(this$GSCAT, file = file.path(path,paste0(fn,"_GSCAT.csv")), row.names = F)
    utils::write.csv(this$GSDET_LF, file = file.path(path,paste0(fn,"_LF.csv")), row.names = F)
    utils::write.csv(this$GSDET_DETS, file = file.path(path,paste0(fn,"_DETS.csv")), row.names = F)
    utils::write.csv(this$GSSPECIES_NEW, file = file.path(path,paste0(fn,"_GSSPECIES.csv")), row.names = F)
  }
  return(this)
}
