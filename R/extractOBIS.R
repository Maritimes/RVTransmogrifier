#' @title extractOBIS
#' @description This function generates an OBIS-compliant dataset, including "core", "occurrences"
#' and "emof" objects for a given survey.
#' @param survey the default is \code{NULL}. Valid values are "SPRING", "SUMMER", "FALL", and "4VSW". 
#' @param years the default is \code{NULL}. Valid values range from 1970-present.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca} \cr Heidi van Vliet
#' \email{Heidi.vanVliet@@dfo-mpo.gc.ca}
#' @export
extractOBIS <- function(cxn = NULL, survey = NULL, years = NULL, path =NULL, debug = FALSE){

  results <- list()
  #all of the generic information about the rights of the data and which org produced it
  dataInfo <- function(thisSurv = NULL){
    infoBlock <- data.frame(rightsHolder = 'His Majesty the King in right of Canada, as represented by the Minister of Fisheries and Oceans',
                            geodeticDatum = 'WGS 84',
                            license = 'https://creativecommons.org/licenses/by/4.0/legalcode',
                            institutionID = 'https://edmo.seadatanet.org/report/1811',
                            institutionCode = 'Bedford Institute of Oceanography (BIO)',
                            datasetName = paste0('MARITIMES ', toupper(thisSurv),' RESEARCH VESSEL SURVEY')
    )
    return(infoBlock)
  }
  #generate taxonomic df
  
  doOBIS <- function(data = NULL, thisSurv = NULL, thisYear = NULL, debug = F){

    taxonomicInfo <- function(data = NULL,SPEC = NULL,...){
      species_taxa <- data[data$CODE %in% SPEC &
                             !is.na(data$APHIA_ID) ,c("SPEC",       #"scientificName"
                                                 "COMM",            #"vernacularName"
                                                 "CODE",            #"SPEC"
                                                 "APHIA_ID",         #"AphiaID"
                                               #  "AUTHORITY",       #"scientificNameAuthorship"
                                                 "KINGDOM",         #"kingdom"
                                                 "PHYLUM",          #"phylum"
                                                 "CLASS",           #"class"
                                                 "ORDER",           #"order"
                                                 "FAMILY",          #"family"
                                                 "GENUS",            #"genus"
                                                 "RANK"
                             )]
      species_taxa$scientificNameID <- paste0("urn:lsid:marinespecies.org:taxname:",species_taxa$APHIA_ID)
      species_taxa$specificEpithet <- sub('^.* ([[:alnum:]]+)$', '\\1', species_taxa$SPEC)
      species_taxa[which(species_taxa$SPEC == species_taxa$specificEpithet),"specificEpithet"] <- NA
      species_taxa$scientificNameAuthorship <- NA
      colnames(species_taxa)[colnames(species_taxa)=="SPEC"] <- "scientificName"
      colnames(species_taxa)[colnames(species_taxa)=="COMM"] <- "vernacularName"
      # colnames(species_taxa)[colnames(species_taxa)=="AUTHORITY"] <- "scientificNameAuthorship"
      colnames(species_taxa)[colnames(species_taxa)=="APHIA_ID"] <- "AphiaID"
      colnames(species_taxa)[colnames(species_taxa)=="CODE"] <- "SPEC"
      colnames(species_taxa)[colnames(species_taxa)=="KINGDOM"] <- "kingdom"
      colnames(species_taxa)[colnames(species_taxa)=="PHYLUM"]  <- "phylum"
      colnames(species_taxa)[colnames(species_taxa)=="CLASS"]   <- "class"
      colnames(species_taxa)[colnames(species_taxa)=="ORDER"]   <- "order"
      colnames(species_taxa)[colnames(species_taxa)=="FAMILY"]  <- "family"
      colnames(species_taxa)[colnames(species_taxa)=="GENUS"]   <- "genus"
      colnames(species_taxa)[colnames(species_taxa)=="RANK"]    <- "taxonRank"
      species_taxa$lifeStage <- NA
      return(species_taxa)
    }
    
    results   <- list()

    #Make a version of GSINF that has OBIS fields that would otherwise need to be calculated repeatedly
    setsPreProcessed <- data$GSINF |>
      dplyr::mutate(eventDate = lubridate::with_tz(SDATE, "America/Halifax"),
                    theSet = paste(MISSION, formatC(SETNO, width = 3, flag=0), sep=":"), 
                    id = MISSION,
                    DMIN_M = fathomsToMeters(DMIN), 
                    DMAX_M = fathomsToMeters(DMAX), 
                    DEPTH_M = fathomsToMeters(DEPTH), 
                    START_DEPTH_M = fathomsToMeters(START_DEPTH), 
                    END_DEPTH_M = fathomsToMeters(END_DEPTH))  |> 
      dplyr::select(-c(MISSION, SETNO, SDATE, TIME, REMARKS, DMIN, DMAX,DEPTH,START_DEPTH, END_DEPTH))|>
      as.data.frame() 
    
    #Make a version of GSCAT that has OBIS fields that would otherwise need to be calculated repeatedly
    catPreProcessed <- data$GSCAT |>
      dplyr::mutate(theCatch = paste(MISSION,
                                     formatC(SETNO, width = 3, flag=0),
                                     formatC(SPEC, width = 4, flag=0),sep = ":"),
                    theSet = paste(MISSION,
                                   formatC(SETNO, width = 3, flag=0),sep = ":")) |>
      dplyr::select(-c(MISSION, SETNO,SPEC)) |>
      as.data.frame()
    catPreProcessed <- merge(catPreProcessed, setsPreProcessed[,c("theSet","DIST")])
    
    catPreProcessed <- catPreProcessed |>
      dplyr::rename(TOTNO_RAW = TOTNO, TOTWGT_RAW = TOTWGT) |>
      dplyr::mutate(TOTWGT = TOTWGT_RAW * (1.75 / DIST),
                    TOTNO = TOTNO_RAW * (1.75 / DIST)) |>
      dplyr::select(-c(DIST))
    
    #Make a version of dataDETS that has OBIS fields that would otherwise need to be calculated repeatedly
    #was going to use FSHNO and/or SPECIMEN_ID as a unique ID, but both fields have blanks
    #instead, will make ID using rowId but sorting carefully first
    detPreProcessed <- data$GSDET |>
      dplyr::mutate(theSet = paste(MISSION,
                                   formatC(SETNO, width = 3, flag=0),sep = ":"),
                    theCatch = paste(MISSION,
                                     formatC(SETNO, width = 3, flag=0),
                                     formatC(SPEC, width = 4, flag=0),sep = ":")
      ) |>
      dplyr::select(-c(MISSION, SETNO,SPEC)) |>
      dplyr::arrange(theCatch, FSHNO, SPECIMEN_ID) |>
      dplyr::mutate(detID = dplyr::row_number()) |> 
      as.data.frame()
    #figure out how to add an id at this stage - maybe just a rowid
    
    #make BBOX for each mission
    missionWKT <-  setsPreProcessed[,c("id","SLAT_DD", "SLONG_DD")] |>
      sf::st_as_sf(coords=c('SLONG_DD','SLAT_DD'),crs=4326) |>
      dplyr::group_by(id) |>
      dplyr::summarize(footprintWKT = sf::st_as_text(sf::st_as_sfc(sf::st_bbox(geometry)), crs = 4326)) |>
      sf::st_drop_geometry() |>
      dplyr::mutate(footprintSRS = "epsg:4326") |>
      as.data.frame()
    
    #make WKT for each set
    setWKT <-  setsPreProcessed[,c("theSet","SLAT_DD", "SLONG_DD", "ELAT_DD", "ELONG_DD")] |>
      dplyr::mutate(footprintSRS = "epsg:4326",
                    footprintWKT = ifelse(!is.na(ELAT_DD) & !is.na(ELONG_DD),
                                          paste("LINESTRING(",SLONG_DD," ",SLAT_DD,", ",ELONG_DD," ",ELAT_DD,")",sep=""),
                                          paste("POINT(",SLONG_DD," ",SLAT_DD,")",sep=""))) |>
      dplyr::select(theSet, footprintSRS, footprintWKT) |>
      as.data.frame()
    #get the earliest eventDate for each mission
    missionEvents <- setsPreProcessed |>
      dplyr::mutate(type= "Mission level information",
                    eventID = id,
                    parentEventID = NA,
                    eventRemarks = "eventDate is the date of the first set of a mission",
                    locationRemarks	 = "Scotian Shelf; decimalLatitude and decimalLongitude are centroids of the area",
                    coordinateUncertaintyInMeters	 = NA,
                    sampleSizeValue = NA,
                    sampleSizeUnit = NA,
                    decimalLatitude = NA,
                    decimalLongitude = NA) |>
      dplyr::group_by(id, type, eventID, 
                      parentEventID, 
                      eventRemarks, 
                      locationRemarks, 
                      sampleSizeValue, 
                      sampleSizeUnit, 
                      coordinateUncertaintyInMeters, 
                      decimalLatitude, decimalLongitude) |>
      dplyr::summarize(.groups = "keep", eventDate = min(eventDate, na.rm=TRUE),
                       # decimalLatitude = round(mean(SLAT_DD),6),
                       # decimalLongitude = round(mean(SLONG_DD),6),
                       minimumDepthInMeters = min(DMIN_M),
                       maximumDepthInMeters = max(DMAX_M)) |>
      as.data.frame()
    missionEvents <- merge(missionEvents, missionWKT)
    # missionEvents$decimalLongitude = wellknown::wkt_centroid(missionEvents$footprintWKT)[[1]]
    # missionEvents$decimalLatitude = sf::::wkt_centroid(missionEvents$footprintWKT)[[2]]
    
    missionEvents$footprintWKT_sf <- sf::st_as_sfc(missionEvents$footprintWKT)
    missionEvents$centroid <- sf::st_centroid(missionEvents$footprintWKT_sf)
    missionEvents$decimalLongitude <- sf::st_coordinates(missionEvents$centroid)[, "X"]
    missionEvents$decimalLatitude <- sf::st_coordinates(missionEvents$centroid)[, "Y"]
    
    rm("missionWKT")
    
    missionEMOF <- missionEvents[,c("eventID"), FALSE] |>
      dplyr::mutate(emofID=eventID,
                    # occurrenceID = NA,
                    measurementType = "Vessel name",
                    measurementRemarks= NA,
                    measurementUnit= NA,
                    measurementValue = substr(missionEvents$eventID,1,3)) |>
      dplyr::select(c("emofID","measurementType","measurementValue", "measurementRemarks","measurementUnit")) |>  #,"measurementRemarks")) |>
      as.data.frame()

    missionEMOF$measurementValue <- ifelse(missionEMOF$measurementValue=="CAB","JOHN CABOT",
                                           ifelse(missionEMOF$measurementValue=="CAR","JACQUES CARTIER",
                                                  ifelse(missionEMOF$measurementValue=="VEN","MERSEY VENTURE",
                                                         ifelse(missionEMOF$measurementValue=="NED","ALFRED NEEDLER",
                                                                ifelse(missionEMOF$measurementValue=="TEL","TELEOST",
                                                                       ifelse(missionEMOF$measurementValue=="TEM","W. TEMPLEMAN",
                                                                              ifelse(missionEMOF$measurementValue=="ATC","A. T. Cameron",
                                                                                     ifelse(missionEMOF$measurementValue=="HAM","LADY HAMMOND","ERROR"))))))))

    gearDets <- setsPreProcessed |>
      dplyr::distinct(id, GEAR) |>
      as.data.frame()
    # gearDets <- merge(gearDets, data$GSGEAR)
    gearDets$WSPREAD <-NA
    gearDets$GEARDESC <-NA
    gearDets[gearDets$GEAR ==  3,"WSPREAD"] <- 10.7    #31.1' 10.7m
    gearDets[gearDets$GEAR ==  9,"WSPREAD"] <- 12.5    #41.0' 12.5m
    gearDets[gearDets$GEAR == 15,"WSPREAD"] <- 12.6    #41.3' 12.6m
    gearDets[gearDets$GEAR ==  3,"GEARDESC"] <- "Yankee" #36 otter trawl  
    gearDets[gearDets$GEAR ==  9,"GEARDESC"] <- "Western IIA trawl"  
    gearDets[gearDets$GEAR == 15,"GEARDESC"] <- "US 4 seam 3 bridle survey trawl"   
    setArea <- setsPreProcessed |>
      dplyr::distinct(id, theSet, DIST) |>
      as.data.frame()

    setArea <- merge(setArea, gearDets[,c("id","GEAR", "WSPREAD")], by="id")
    setArea$AREA_M2 <- round((setArea$DIST*1852) * setArea$WSPREAD,4)  #convert nm to m
    setArea$GEAR <- setArea$DIST <- setArea$id <- NULL
    
    setEvents <- setsPreProcessed |>
      dplyr::rename(parentEventID=id) |>
      dplyr::mutate(type = "Trawl level information",
                    id= theSet,
                    eventID= theSet,
                    eventRemarks = paste0("Trawl ",substr(eventID, nchar(eventID)-2,  nchar(eventID))),
                    locationRemarks = paste0("Strata: ", STRAT,"; decimalLatitude and decimalLongitude are the start positions of each set"),
                    HOWD	 = HOWD,
                    sampleSizeUnit = paste0("square metres"),
                    decimalLatitude = SLAT_DD,
                    decimalLongitude = SLONG_DD)|>
      dplyr::group_by(id, type, eventID, parentEventID, eventRemarks, locationRemarks, sampleSizeUnit, HOWD, decimalLatitude, decimalLongitude) |>
      dplyr::summarize(.groups = "keep", eventDate = min(eventDate, na.rm=TRUE),
                       minimumDepthInMeters = min(DMIN_M),
                       maximumDepthInMeters = max(DMAX_M)) |>
      as.data.frame()
    setEvents$coordinateUncertaintyInMeters <- NA
    # setEvents[setEvents$HOWD== 0, "coordinateUncertaintyInMeters"] <- 100
    # setEvents[setEvents$HOWD== 0 & as.numeric(substr(setEvents$eventDate,1,4))>2019, "coordinateUncertaintyInMeters"] <- 30
    # setEvents[setEvents$HOWD== 1,"coordinateUncertaintyInMeters"] <- NA #Ships log
    # setEvents[setEvents$HOWD== 2,"coordinateUncertaintyInMeters"] <- NA # Radar buoy
    # setEvents[setEvents$HOWD== 3,"coordinateUncertaintyInMeters"] <- 1852 # Decca https://en.wikipedia.org/wiki/Decca_Navigator_System#Range_and_accuracy
    # setEvents[setEvents$HOWD== 4,"coordinateUncertaintyInMeters"] <- 4500 # Loran https://en.wikipedia.org/wiki/LORAN#Range_and_accuracy
    # setEvents[setEvents$HOWD== 7,"coordinateUncertaintyInMeters"] <- NA # Decca and Loran
    # setEvents[setEvents$HOWD== 8,"coordinateUncertaintyInMeters"] <- NA # Satellite Navigation
    # setEvents[setEvents$HOWD== 9,"coordinateUncertaintyInMeters"] <- NA # # calculated
    setEvents$HOWD <- NULL                        
    
    setEvents <- merge(setEvents, setArea, by.x="id", by.y="theSet") |>
      dplyr::rename(sampleSizeValue = AREA_M2)
    setEvents$WSPREAD <- NULL
    setEvents <- merge(setEvents, setWKT, by.x="eventID", by.y="theSet")
    rm("setWKT")
    
    gearDets$gearType <- "trawl"

    gearEMOF <- gearDets[,c("id", "gearType", "GEARDESC", "WSPREAD")] |>
      tidyr::pivot_longer(c(GEARDESC,WSPREAD, gearType),
                          names_to = "measurementType",
                          values_to = "measurementValue",
                          values_transform = as.character,
                          values_drop_na = T) |>
      dplyr::mutate(measurementType =NA,
                    measurementUnit =NA,
                    measurementRemarks =NA) |> 
      dplyr::rename(emofID = id) |> 
      as.data.frame()
    gearEMOF[gearEMOF$measurementValue %in% gearDets$GEARDESC,c("measurementType","measurementUnit","measurementRemarks")] <- as.data.frame(t(as.matrix(c("trawl name", NA, "Name of fishing gear deployed" ))))                                       
    gearEMOF[gearEMOF$measurementValue %in% gearDets$gearType,c("measurementType","measurementUnit","measurementRemarks")] <- as.data.frame(t(as.matrix(c("Sampling protocol", NA, "Type of fishing gear deployed"))))
    gearEMOF[gearEMOF$measurementValue %in% gearDets$WSPREAD, c("measurementType","measurementUnit","measurementRemarks")] <- as.data.frame(t(as.matrix(c("trawl wingspread","metres","the horizontal distance between the wings"))))
    
    
    setsPreProcessed <- merge(setsPreProcessed, setArea)
    
    setEMOF <- setsPreProcessed[,!names(setsPreProcessed) %in% c("eventDate","id",
                                                                 "AREA", "GEAR","SLAT_DD",
                                                                 "SLONG_DD","FORCE","WIND"
                                                                 , "CURNT", 
                                                                 "ELAT_DD","ELONG_DD",
                                                                 "AUX","HOWD", "HOWS",
                                                                 "STATION", "START_DEPTH_M",
                                                                 "END_DEPTH_M","STRAT", "TYPE", "AREA_M2")] |>
      tidyr::pivot_longer(c(BOTTOM_SALINITY,BOTTOM_TEMPERATURE,DIST,
                            DMAX_M,DMIN_M,DEPTH_M, DUR,
                            SPEED, SURFACE_TEMPERATURE),
                          names_to = "measurementType",
                          values_to = "measurementValue",
                          values_transform = as.character,
                          values_drop_na = T) |>
      dplyr::mutate(measurementRemarks = NA) |> 
      dplyr::rename(emofID=theSet) |>
      as.data.frame()
    setEMOF[setEMOF$measurementType == "DIST",    c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("length of sampling track","nautical miles"))))
    setEMOF[setEMOF$measurementType == "DMAX_M",  c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("maximum (deepest) bottom depth observed during a tow","metres"))))
    setEMOF[setEMOF$measurementType == "DMIN_M",  c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("minimum (shallowest) bottom depth observed during a tow","metres"))))
    setEMOF[setEMOF$measurementType == "DUR",     c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("sample duration","minutes"))))
    setEMOF[setEMOF$measurementType == "SPEED",   c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("average tow speed","knots"))))
    setEMOF$WSPREAD <- NULL

    catchBiomassEmof <- catPreProcessed |>
      dplyr::mutate(#eventID = theSet,
        #occurrenceID = theCatch,
        #id= theSet,
        emofID = theCatch,
        measurementType = "total biomass",
        measurementUnit = "kilograms",
        measurementValue = TOTWGT,
        measurementRemarks = "This weight has been adjusted to be representative of a standard 1.75NM tow",
        measurementRemarksSupplemental = ifelse(TOTWGT==0,"Small catches can appear as 0 (they were rounded to the nearest kg)", NA)) |>
      dplyr::select(-c("TOTNO_RAW", "TOTWGT_RAW","TOTNO", "TOTWGT","theSet", "theCatch")) |>
      as.data.frame()
    
    catchAbundanceEmof <- catPreProcessed |>
      dplyr::mutate(emofID = theCatch,
                    measurementType = "individualCount",
                    measurementUnit = NA,
                    measurementValue = TOTNO,
                    measurementRemarks = "This count has been adjusted to be representative of a standard 1.75NM tow",
                    measurementRemarksSupplemental = NA) |>
      dplyr::select(-c("TOTNO_RAW", "TOTWGT_RAW","TOTNO", "TOTWGT","theSet", "theCatch")) |>
      as.data.frame()
    catchEMOF <- rbind.data.frame(catchBiomassEmof, catchAbundanceEmof)
    rm(list=c("catchAbundanceEmof", "catchBiomassEmof"))
    
    catchEMOF$measurementRemarks <- apply(catchEMOF[, c("measurementRemarks", "measurementRemarksSupplemental")], 1,
                                          function(i){ paste(stats::na.omit(i), collapse = "; ") })
    catchEMOF$measurementRemarksSupplemental <- NULL
    
    detPad<- nchar(max(detPreProcessed$detID))
    detEMOF <- detPreProcessed[,!names(detPreProcessed) %in% c("AGMAT")] |>
      tidyr::pivot_longer(c(FSEX, FWT, FLEN, FMAT, AGE),
                          names_to = "measurement",
                          values_to = "measurementValue",
                          values_transform = as.character,
                          values_drop_na = T) |>
      dplyr::mutate(SPEC = as.numeric(substr(theCatch, 16, 19),
                                      measurementType = NA,
                                      measurementUnit = NA),
                    measurementRemarks = NA
      ) |>
      dplyr::arrange(theCatch, FSHNO, SPECIMEN_ID, measurement, measurementValue) |> 
      dplyr::mutate(emofID = paste(theCatch,
                                   formatC(detID, width = detPad, flag=0),sep = ":")) |> 
      dplyr::select(-c("theSet", "FSHNO", "SPECIMEN_ID","theCatch","detID")) |>
      as.data.frame()
    detEMOF[detEMOF$measurement == "FWT",c("measurementType",  "measurementUnit")] <- as.data.frame(t(as.matrix(c("round weight of the specimen","grams"))))
    detEMOF[detEMOF$measurement == "AGE",c("measurementType",  "measurementUnit")] <- as.data.frame(t(as.matrix(c("age", "years"))))
    detEMOF[detEMOF$measurement == "FSEX",c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("sex", NA))))
    detEMOF[detEMOF$measurement == "FSEX" & detEMOF$measurementValue ==3,"measurementValue"] <- "Female (berried)"
    detEMOF[detEMOF$measurement == "FSEX" & detEMOF$measurementValue ==2,"measurementValue"] <- "Female"
    detEMOF[detEMOF$measurement == "FSEX" & detEMOF$measurementValue ==1,"measurementValue"] <- "Male"
    detEMOF[detEMOF$measurement == "FSEX" & detEMOF$measurementValue ==0,"measurementValue"] <- "Unknown"
    detEMOF[detEMOF$measurement == "FMAT",c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("maturity",NA))))
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==0,"measurementValue"] <- "Undetermined"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==1,"measurementValue"] <- "Immature"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==2,"measurementValue"] <- "Ripening 1"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==3,"measurementValue"] <- "Ripening 2"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==4,"measurementValue"] <- "Ripe (Mature)"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==5,"measurementValue"] <- "Spawning (Running)"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==6,"measurementValue"] <- "Spent"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==7,"measurementValue"] <- "Recovering"
    detEMOF[detEMOF$measurement == "FMAT" & detEMOF$measurementValue ==8,"measurementValue"] <- "Resting"
    
    #add a default values for FLEN that we'll overwrite for specific species
    detEMOF[detEMOF$measurement == "FLEN",c("measurementType","measurementUnit")] <- as.data.frame(t(as.matrix(c("Length (fork length)","centimeters"))))
    # scallops
    detEMOF[detEMOF$SPEC %in% c(4320,4321,4322,4324,4325) & detEMOF$measurement == "FLEN", c("measurementType","measurementUnit")] <- as.data.frame(t(as.matrix(c("shell height","millimeters"))))
    # squid
    detEMOF[detEMOF$SPEC %in% c(4511,4512,4514,4664) & detEMOF$measurement == "FLEN", c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("mantle length","centimeters"))))
    # lobsters
    detEMOF[detEMOF$SPEC %in% c(2550:2553,8145) & detEMOF$measurement == "FLEN", c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("carapace length","millimeters"))))
    #crabs
    detEMOF[detEMOF$SPEC %in% c(2506:2547,6006) & detEMOF$measurement == "FLEN", c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("carapace length", "millimeters"))))
    #herring
    detEMOF[detEMOF$SPEC %in% c(60) & detEMOF$measurement == "FLEN", c("measurementType", "measurementUnit")] <- as.data.frame(t(as.matrix(c("Length (fork length)","millimeters"))))
    detEMOF[detEMOF$SPEC %in% c(60) & detEMOF$measurement == "FLEN", c("measurementRemarks")] <- "Starting in summer 2016, herring length is measured in mm. Prior measurements have been converted from cm."
    #grenadiers
    detEMOF[detEMOF$SPEC %in% c(409:414,416, 1269, 1270, 998) & detEMOF$measurement == "FLEN",  c("measurementType","measurementUnit")] <- as.data.frame(t(as.matrix(c("pre-anal fin length","centimeters"))))
    detEMOF[detEMOF$SPEC %in% c(409:414,416, 1269, 1270, 998) & detEMOF$measurement == "FLEN", c("measurementRemarks")] <- "Some grenadier species are now measured using pre-anal instead of fork-length."
    detEMOF$SPEC <- detEMOF$measurement <- NULL

    sppOccurrence <-   catPreProcessed |>
      dplyr::rename(occurrenceID = theCatch,
                    # parentEvent = theSet,
                    individualCount = TOTNO,
                    organismQuantity = TOTWGT
      ) |>
      dplyr::mutate(SPEC = as.numeric(substr(occurrenceID,16,19)),
                    occurrenceStatus= "present",
                    individualCount = ifelse(individualCount>0,individualCount,NA),
                    organismQuantity = ifelse(organismQuantity>0,organismQuantity,NA),
                    basisOfRecord = "HumanObservation",
                    organismQuantityType = "Kilograms",
                    occurrenceRemarks = "The values for individualCount and organismQuantity have been adjusted to be representative of a standard 1.75NM tow") |>
      dplyr::select(-c(TOTNO_RAW, TOTWGT_RAW,theSet, CALWT, SAMPWGT)) |> 
      as.data.frame()
    
    species_taxa <- taxonomicInfo(data = this$GSSPECIES, SPEC = unique(sppOccurrence$SPEC))
    
    occurrence<- merge(sppOccurrence, species_taxa[,!names(species_taxa) %in% c("AphiaID", "SPEC")])
    rm(list=c("sppOccurrence", "species_taxa"))
    setEvents$footprintWKT_sf <- sf::st_as_sfc(setEvents$footprintWKT)

    setEvents$centroid <- sf::st_centroid(setEvents$footprintWKT_sf)
    
    eventCore <- rbind.data.frame(missionEvents, setEvents)
    eventCore <- cbind(eventCore,dataInfo(thisSurv = thisSurv))
    eventCore$footprintWKT_sf <- NULL
    results$eventCore <- eventCore
    results$occurrence <- occurrence
    
    # EMOF <- rbind.data.frame(missionEMOF, gearEMOF)
    # EMOF <- rbind.data.frame(EMOF, setEMOF)
    # EMOF <- rbind.data.frame(EMOF, catchEMOF)
    # EMOF <- rbind.data.frame(EMOF, detEMOF)
    emof_fields <- c("emofID","measurementRemarks","measurementType", "measurementUnit", "measurementValue")
    EMOF <- rbind.data.frame(missionEMOF[,emof_fields], gearEMOF[,emof_fields])
    EMOF <- rbind.data.frame(EMOF, setEMOF[,emof_fields])
    EMOF <- rbind.data.frame(EMOF, catchEMOF[,emof_fields])
    EMOF <- rbind.data.frame(EMOF, detEMOF[,emof_fields])
    colnames(EMOF)[colnames(EMOF)=="emofID"] <- "measurementID"
    
    # EMOF <- EMOF[with(EMOF,order(eventID, measurementType, measurementValue)),]
    results$EMOF <- EMOF
    return(results)
  }
  
  for (s in 1:length(survey)){
    for (y in 1:length(years)){
      message("Working on  ",survey[s],": ",years[y])
      this <- loadRVData(cxn = cxn, survey = survey[s], years = years[y], types = 1, debug = debug)
      if (is.numeric(this)){
        message("No data found for ",survey[s],": ",years[y])
        next
      }
      name <- paste0(survey[s],"_",years[y])
      obis <- doOBIS(this, thisSurv = survey[s], thisYear = years[y])
      results[[name]]<-obis
      obis<- NULL
    }
  }
  
  return(results)
}
