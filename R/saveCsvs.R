saveCsvs <-  function(tblList = NULL, desc = NULL, path = NULL, debug = FALSE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Created directory: ", path)
  }
  
  ts <- format(Sys.time(), "%Y")
  if (is.null(path)) {
    path <- getwd()
  }
  
  this <- tblList
    
  this$GSMISSIONS <- this$GSMISSIONS |>
    dplyr::select(MISSION, VESEL, CRUNO, YEAR, SEASON)
  
  this$GSINF <- this$GSINF |>
    dplyr::left_join(this$GSGEAR, by = intersect(colnames(this$GSINF), colnames(this$GSGEAR))) |>
    dplyr::select(-GEAR) |>
    dplyr::mutate(
      DEPTH_M = coalesce(DEPTH, rowMeans(this$GSINF[, c("DMIN", "DMAX")], na.rm = TRUE)),
      DEPTH_M = fathomsToMeters(DEPTH_M)
    ) |>
    dplyr::mutate(SDATE = as.Date(SDATE)) |>
    dplyr::rename(
      NAFO_AREA_ID = AREA,
      STRAT_AREA_KM2 = AREA_KM2,
      SURF_TEMP = SURFACE_TEMPERATURE,
      BOTT_TEMP = BOTTOM_TEMPERATURE,
      BOTT_SAL = BOTTOM_SALINITY
    ) |>
    dplyr::select(
      MISSION,
      SETNO,
      SDATE,
      TIME,
      NAFO_AREA_ID,
      STRAT,
      STRAT_AREA_KM2,
      SLAT_DD,
      SLONG_DD,
      ELAT_DD,
      ELONG_DD,
      DUR,
      DIST,
      SPEED,
      DEPTH_M,
      SURF_TEMP,
      BOTT_TEMP,
      BOTT_SAL,
      GEARDESC
    ) |>
    dplyr::rename(DEPTH = DEPTH_M) |>
    dplyr::arrange(substr(MISSION, 4, 7), SETNO)

  this$GSCAT <- this$GSCAT |>
    dplyr::select(MISSION, SETNO, SPEC, SIZE_CLASS,TOTWGT, TOTNO) |>
    drop_size_class() |> 
    dplyr::arrange(substr(MISSION, 4, 7), SETNO,SPEC)
  
  this$GSDET <- this$GSDET |>
    dplyr::left_join(this$GSMATURITY, by = c("FMAT" = "CODE")) |>
    dplyr::mutate(MATURITY = DESCRIPTION) |>
    dplyr::select(-DESCRIPTION) |>
    dplyr::left_join(this$GSSEX, by = c("FSEX" = "CODE")) |>
    dplyr::mutate(SEX = DESCRIPTION) |>
    dplyr::select(-DESCRIPTION) |>
    dplyr::select(
      MISSION,
      SETNO,
      SPEC,
      FSHNO,
      FLEN,
      FWT,
      AGE,
      CLEN,
      SPECIMEN_ID,
      MATURITY,
      SEX
    ) |>
    dplyr::arrange(substr(MISSION, 4, 7), SETNO, SPEC, FLEN, FWT, MATURITY, SEX)
  
  if ("GSDET_CONV" %in% names(this)) {
    this$GSDET_CONV <- this$GSDET_CONV |>
      dplyr::left_join(this$GSMATURITY, by = c("FMAT" = "CODE")) |>
      dplyr::mutate(MATURITY = DESCRIPTION) |>
      dplyr::select(-DESCRIPTION) |>
      dplyr::left_join(this$GSSEX, by = c("FSEX" = "CODE")) |>
      dplyr::mutate(SEX = DESCRIPTION) |>
      dplyr::select(-DESCRIPTION) |>
      dplyr::select(
        MISSION,
        SETNO,
        SPEC,
        FSHNO,
        FLEN,
        FWT,
        AGE,
        CLEN,
        SPECIMEN_ID,
        MATURITY,
        SEX
      ) |>
      dplyr::arrange(substr(MISSION, 4, 7), SETNO, SPEC, FLEN, FWT, MATURITY, SEX)
  }

  if ("GSCAT_CONV" %in% names(this)) {
    this$GSCAT_CONV <- this$GSCAT_CONV |>
      dplyr::select(MISSION, SETNO, SPEC, TOTWGT, TOTNO, TOTWGT_OG, TOTNO_OG) |>
      dplyr::arrange(substr(MISSION, 4, 7), SETNO, SPEC)
  }
  
  this$GSSPECIES_NEW <- this$GSSPECIES_NEW |>
    dplyr::rename(SCI_NAME = SPEC) |>
    dplyr::select(CODE, SCI_NAME, COMM, APHIA_ID) |>
    dplyr::arrange(CODE)
  
  utils::write.csv(
    this$GSINF,
    file = file.path(path, paste0(desc, "_GSINF.csv")),
    row.names = F
  )
  
  this$GSINF <- this$GSINF[complete.cases(this$GSINF$SLAT_DD, this$GSINF$SLONG_DD), ]
  Mar.utils::df_sf_to_gpkg(
    df = this$GSINF,
    lat.field = "SLAT_DD",
    lon.field = "SLONG_DD",
    gpkgName = "OpenData.gpkg",
    path = path,
    layerName = paste0(desc, "_OpenData")
  )
  
  utils::write.csv(
    this$GSMISSIONS,
    file = file.path(path, paste0(desc, "_GSMISSIONS.csv")),
    row.names = F
  )

  utils::write.csv(
    this$GSCAT,
    file = file.path(path, paste0(desc, "_GSCAT.csv")),
    row.names = F
  )
  if ("GSCAT_CONV" %in% names(this)) {
    utils::write.csv(
      this$GSCAT_CONV,
      file = file.path(path, paste0(desc, "_GSCAT_CONV.csv")),
      row.names = F
    )
  }
  utils::write.csv(
    this$GSDET,
    file = file.path(path, paste0(desc, "_GSDET.csv")),
    row.names = F
  )
  if ("GSDET_CONV" %in% names(this)) {
    utils::write.csv(
      this$GSDET_CONV,
      file = file.path(path, paste0(desc, "_GSDET_CONV.csv")),
      row.names = F
    )
  }
  utils::write.csv(
    this$GSSPECIES_NEW,
    file = file.path(path, paste0(desc, "_GSSPECIES.csv")),
    row.names = F
  )
  return(this)
}