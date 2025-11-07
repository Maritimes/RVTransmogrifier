#' @title Mar_DATRAS
#' @description This function generates ICES DATRAS-compatible files directly from the Maritimes
#' groundfish database.
#' @param years default is \code{NULL}. This specifies the year(s) for which you'd like to generate
#' HH files. Single years are fine, as are vectors (e.g. \code{c(2011,1015)}, \code{2015:2019})
#' @param survey default is \code{NULL}. This specifies the survey(s) for which you'd like to generate
#' HH files.  Valid values are
#' \itemize{
#' \item \code{4X} - Type 1; Spring (i.e. months 1:4); 2008+; not strata 5Z* 
#' \item \code{GEORGES} - Type 1; Spring (i.e. months 1:4);  2008+; strata 5Z*
#' \item \code{SPRING} - Type 1; Spring (i.e. months 1:4); pre-2008; specific strata 
#' \item \code{4VSW}  - Type 1; Spring (i.e. months 1:4); 4VSW strata;  
#' \item \code{SUMMER} - Type 1; Summer (i.e. months 5:8); specific strata
#' \item \code{FALL} - Type 1; Fall (i.e. months 9:12)
#' }
#' @param csv default is \code{TRUE}.  If \code{TRUE}, csv files are generated for each HH code.  If
#' \code{FALSE}, the output exists only in the resultant list.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param data.dir  The default is \code{NULL}. This is the path to your Mar.datawrangling
#' rdata files
#' @param debug  The default is \code{F}. Setting this to TRUE will limit the 
#' results to a single set for a single species. 
#' @return a list containing (named) objects - 1 for each generated HH file
#' @family DATRAS
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#'
extractDATRAS <- function(years=NULL, survey=NULL, csv =T,
                       cxn = NULL,
                       data.dir = NULL,
                       debug = F){
  stop(
    "extractDATRAS has not yet been enabled"
  )
  timestamp<-format(Sys.time(), "%Y%m%d_%H%M")
  Sys.setenv(TZ = "America/Halifax")
  scratch_env = new.env()
  results<-list()
  ord_HH <- c("RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR", "MONTH","DAY",
              "TIMESHOT","DEPTHSTRATUM","HAULDUR","DAYNIGHT", "SHOOTLAT","SHOOTLONG",
              "HAULLAT","HAULLONG","STATREC","DEPTH", "HAULVAL","HYDROSTNO",
              "STDSPECRECCODE","BYSPECRECCODE","DATATYPE","NETOPENING",
              "RIGGING","TICKLER","DISTANCE","WARPLNGT","WARPDIA", "WARPDEN",
              "DOORSURFACE","DOORWGT","DOORSPREAD","WINGSPREAD","BUOYANCY",
              "KITEDIM","WGTGROUNDROPE","TOWDIR","GROUNDSPEED","SPEEDWATER",
              "SURCURDIR","SURCURSPEED","BOTCURDIR","BOTCURSPEED","WINDDIR",
              "WINDSPEED","SWELLDIR","SWELLHEIGHT","SURTEMP","BOTTEMP","SURSAL",
              "BOTSAL","THERMOCLINE","THCLINEDEPTH","CODENDMESH","SECCHIDEPTH",
              "TURBIDITY","TIDEPHASE","TIDESPEED","PELSAMPTYPE", 
              "MINTRAWLDEPTH","MAXTRAWLDEPTH")
  ord_HL <- c("RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT", 
              "GEAREXP", "DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE", 
              "SPECCODE","SPECVAL","SEX","TOTALNO","CATIDENTIFIER","NOMEAS",
              "SUBFACTOR","SUBWGT","CATCATCHWGT","LNGTCODE","LNGTCLASS",
              "HLNOATLNGT","DEVSTAGE","LENMEASTYPE") #"GEAREXP",
  ord_CA <- c("RECORDTYPE", "QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
              "GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE",
              "SPECCODE","AREATYPE","AREACODE","LNGTCODE","LNGTCLASS","SEX",
              "MATURITY","PLUSGR","AGERINGS","CANOATLNGT","INDWGT",
              "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
              "AGEPREPMET","OTGRADING","PARSAMP") #"GEAREXP",
  round2 = function(x, n) {
    #this function ensures that values ending in 0.5 are round up to the next integer - not down to zero (R's default)
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^n
    z*posneg
  }
  
  addLenMeasInfo <- function(df = NULL){
    #set LENMEASTYPE to standard length (2) for all, then overwrite weirdies
    #set LNGTCODE to 1cm for all, then overwrite weirdies
    df$LENMEASTYPE <- 2
    df$LNGTCODE <- 1
    #crabs -     7 "Carapace Width"; in mm
    df[(df$SPEC >= 2506 & df$SPEC <= 2547) | df$SPEC == 6006,c("LENMEASTYPE", "LNGTCODE")]<-data.frame(7,0.1)
    #lobsters -  6 "Carapace Length"; mm
    df[df$SPEC %in% c(2550,2551,2552,2553,8145),c("LENMEASTYPE", "LNGTCODE")]<-data.frame(6,0.1)
    #scallops -  9 "Shell Height"; mm
    df[df$SPEC %in% c(4320,4321,4322,4324,4325),c("LENMEASTYPE", "LNGTCODE")]<-data.frame(9,0.1)
    #squid -     5 "Mantle Length"
    df[df$SPEC %in% c(4511,4512,4514,4664),"LENMEASTYPE"]<-5 #and LNGTCODE is default (cm)
    #herring recorded in mm
    df[df$SPEC ==60 ,"LNGTCODE"]<-0.1
    return(df)
  }
  
  addLNGTCLASS <- function(df=NULL){
    df$LNGTCLASS <- NA
    df$LNGTCLASS<- ceiling(df$FLEN/df$LNGTCODE) * df$LNGTCODE
    return(df)
  }
  
  cat("\n", "Extracting Data")
  

  
  getRaw<-function(years=NULL, survey=NULL,
                   cxn = cxn,
                   data.dir = data.dir){
    scratch_env <- loadRVData(cxn = cxn, survey = survey, years = years, types = 1, debug = debug)    

   tmpE <- new.env()
    Mar.utils::get_data_tables(schema = "GROUNDFISH",cxn =cxn,  tables = c("GS_LV1_OBSERVATIONS"), data.dir = data.dir, env = tmpE)
    scratch_env[["GS_LV1_OBSERVATIONS"]] <- tmpE[["GS_LV1_OBSERVATIONS"]]
    scratch_env$GSMISSIONS = scratch_env$GSMISSIONS[scratch_env$GSMISSIONS$YEAR == years,]
    scratch_env <- propagateChanges(tblList = scratch_env)
   
    # unkSpp <-  scratch_env$GSSPECIES_NEW[!(scratch_env$GSSPECIES_NEW$CODE %in% scratch_env$GSSPECIES_CODES[!is.na(scratch_env$GSSPECIES_CODES$APHIAID),"CODE"]), c("CODE", "COMM", "SPEC")]
    # badSpp1 <- unique(scratch_env$GSCAT[scratch_env$GSCAT$SPEC %in% unkSpp$CODE,"SPEC"])
    # badSpp2 <- unique(scratch_env$GSDET[scratch_env$GSDET$SPEC %in% unkSpp$CODE,"SPEC"])
    # allBad <- unique(c(badSpp1, badSpp2))
    # if (length(allBad)>0){
    #   fullnmSpp <- gsub(".csv", "_sppMissing.csv", fullnm)
    #   theSppFile <- file.create(fullnmSpp)
    #   suppressWarnings(utils::write.table(x = scratch_env$GSSPECIES_NEW[scratch_env$GSSPECIES_NEW$CODE %in% allBad, c("CODE", "COMM", "SPEC")], file = fullnmSpp, row.names = F, col.names = T, quote = FALSE, sep = ","))
    #   message("\nA file was generated containing species names reported in the catch that don't have aphiaids (", fullnmSpp,")")
    #   scratch_env$GSSPECIES_NEW<-scratch_env$GSSPECIES_NEW[!(scratch_env$GSSPECIES_NEW$CODE %in% allBad),]
    #   scratch_env$GSCAT<-scratch_env$GSCAT[!(scratch_env$GSCAT$SPEC %in% allBad),]
    #   scratch_env$GSDET<-scratch_env$GSDET[!(scratch_env$GSDET$SPEC %in% allBad),]
    #   Mar.datawrangling::self_filter(keep_nullsets = T, env = scratch_env, quiet = T)
    # }
    return(scratch_env)
  }
  
  Mar_HH <- function(scratch_env = NULL, survey = NULL){
    cat("\n","Generating HH...")
    
    df= merge(scratch_env$GSINF, scratch_env$GSMISSIONS, all.x = T, by = c("MISSION"))
    df = merge(df, unique(scratch_env$GSWARPOUT), all.x=T, by = c("MISSION","SETNO"))
    df = df[with(df,order(SDATE)),]
    df$haulno <- seq(1:nrow(df))
    # drop unneeded fields, and make original cols lowercase - finals will be uppercase
    df = df[,c("MISSION","BOTTOM_SALINITY","BOTTOM_TEMPERATURE","SDATE",
               "DEPTH","DMIN", "DMAX", "DIST","GEAR","DUR","SETNO","TYPE","SLAT_DD",
               "SLONG_DD", "ELAT_DD", "ELONG_DD", "STRAT", "CURNT",
               "SURFACE_TEMPERATURE","TIME","WIND","FORCE","SPEED", "WARPOUT", "VESEL", "YEAR", "SEASON", "haulno" )]  #,"DMIN","DMAX")]
    names(df) <- tolower(names(df))
    df$HAULNO <- df$haulno
    df$haulno <- NULL
    calcValues<-function(df=NULL){
      # Define some functions
      processTimes<-function(df=NULL){
        # Generate some additional values from our datetime -----------------------
        df$YEAR<-lubridate::year(df$sdate)
        df$MONTH<-lubridate::month(df$sdate)
        df$DAY<-lubridate::day(df$sdate)
        df$HOUR <- as.integer(substr(sprintf('%04d',df$time),1,2))
        df$MIN <- as.integer(substr(sprintf('%04d',df$time),3,4))
        df$DATETIME <- lubridate::make_datetime(year = df$YEAR, month = df$MONTH, day = df$DAY, hour =df$HOUR, min = df$MIN, sec=0, tz = "Canada/Atlantic")
        #line below added iin response to encountering case where reported time happened EXACTLT at start of daylight savings time
        df[is.na(df$DATETIME),"DATETIME"] <- lubridate::make_datetime(year = df[is.na(df$DATETIME),"YEAR"], month = df[is.na(df$DATETIME),"MONTH"], day = df[is.na(df$DATETIME),"DAY"], hour =df[is.na(df$DATETIME),"HOUR"]+1, min = df[is.na(df$DATETIME),"MIN"], sec=0, tz = "Canada/Atlantic")
        #necessary to "force" quarter such that each survey is self-consistent and can be downloaded together
        df$QUARTER<- ifelse(survey == "SUMMER", 3,
                            ifelse(survey %in% c("SPRING", "GEORGES", "4X"), 1,
                                   ifelse(survey == "FALL", 4, -999)))
        
        df$TIMESHOT<-sprintf('%04d',df$time)
        df$sdate <- NULL
        df$time <- NULL
        df$HOUR <- NULL
        df$MIN <- NULL


        df <- df |>
          dplyr::rowwise() |>
          dplyr::mutate(
            tmp = list(suncalc::getSunlightTimes(date = as.Date(DATETIME), lat = slat_dd, lon = slong_dd)),
            DAYNIGHT = ifelse(DATETIME > tmp$sunrise & DATETIME < tmp$sunset, "D", "N")
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-tmp)
        
        df$DATETIME<- NULL
        return(df)
      }
      processTowDets<-function(df=NULL){
        getTowDir <- function(df=NULL){
          # df[,"latitude"] <- round(df[,"latitude"],4)
          # df[,"longitude"] <- round(df[,"longitude"],4)
          # df[!is.na(df$elatitude),"elatitude"] <- round(df[!is.na(df$elatitude),"elatitude"],4)
          # df[!is.na(df$elongitude),"elongitude"] <- round(df[!is.na(df$elongitude),"elongitude"],4)
          df$TOWDIR<-NA
          df[which(!is.na(df$slat_dd) & !is.na(df$slong_dd) &
                     !is.na(df$elat_dd) &!is.na(df$elong_dd)) ,"TOWDIR"] <- round(geosphere::bearingRhumb(p1 = df[which(!is.na(df$slat_dd) & !is.na(df$slong_dd) &
                                                                                                                              !is.na(df$elat_dd) &!is.na(df$elong_dd)) ,c("slong_dd","slat_dd")],
                                                                                                              p2 = df[which(!is.na(df$slat_dd) & !is.na(df$slong_dd) &
                                                                                                                              !is.na(df$elat_dd) &!is.na(df$elong_dd)) ,c("elong_dd","elat_dd")]),0)
          colnames(df)[colnames(df)=="slat_dd"] <- "SHOOTLAT"
          colnames(df)[colnames(df)=="slong_dd"] <- "SHOOTLONG"
          colnames(df)[colnames(df)=="elat_dd"] <- "HAULLAT"
          colnames(df)[colnames(df)=="elong_dd"] <- "HAULLONG"
          return(df)
        }
        getHaulDets<-function(df= NULL){
          # Decode TYPE to haulval  https://vocab.ices.dk/?ref=1
          df$HAULVAL<- NA
          df$HAULVAL <- ifelse(df$type == 1, "V", ifelse(df$type == 3, "I", NA))
          df$type <-NULL
          return(df)
        }
        df <- getTowDir(df)
        df <- getHaulDets(df)
        df$STATREC <- -9
        return(df)
      }
      processCurrents<-function(df=NULL){
        # CURNT/SurCurDir - transformation ----------------------------------------
        # we record the direction of the current relative to the ship (eg to bow,
        # to starboard, etc), but ICES wants it in a compass direction.
        # I use the tow direction (determined above) combined with the
        # value stored in GSINF.CURNT  to derive the value.
        # For example, if the tow was found to be West to East (90deg), and the CURNT value was
        # "to starboard", the combined angle would be 90+90 = 180 (or South)
        df$SURCURDIR<-NA
        df$SURCURDIR <- ifelse(df$curnt == 1, 0,
                               ifelse(df$curnt == 2, 90,
                                      ifelse(df$curnt == 3, -90,
                                             ifelse(df$curnt == 4, 180,
                                                    ifelse(df$curnt == 5, 999,
                                                           ifelse(df$curnt == 6, NA,
                                                                  NA))))))
        df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"] <-
          df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"]+
          df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"TOWDIR"]
        #round determined SURCURDIR to nearest 90-deg
        df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"] <- 45*round(df[!is.na(df$TOWDIR) & (!is.na(df$SURCURDIR) & df$SURCURDIR < 999),"SURCURDIR"]/45)
        df[!is.na(df$SURCURDIR) & df$SURCURDIR == 999,"SURCURDIR"] <- 0
        df[!is.na(df$SURCURDIR) & df$SURCURDIR<0,"SURCURDIR"]<-df[!is.na(df$SURCURDIR) & df$SURCURDIR<0,"SURCURDIR"]+360
        df[!is.na(df$SURCURDIR) & df$SURCURDIR>360 & df$SURCURDIR < 999,"SURCURDIR"]<-df[!is.na(df$SURCURDIR) & df$SURCURDIR>360 & df$SURCURDIR < 999,"SURCURDIR"]-360
        df$curnt <- NULL
        return(df)
      }
      processWind<-function(df=NULL){
        # Turn force column into WINDSPEED ----------------------------------------
        df$WINDSPEED<-NA
        # force holds a value of 1-8, each of which is decoded into to the lower bound of the beaufort scale range
        df$WINDSPEED <- ifelse(df$force == 0, 0,
                               ifelse(df$force == 1, 1,
                                      ifelse(df$force == 2, 4,
                                             ifelse(df$force == 3, 7,
                                                    ifelse(df$force == 4, 11,
                                                           ifelse(df$force == 5, 17,
                                                                  ifelse(df$force == 6, 22,
                                                                         ifelse(df$force == 7, 28,
                                                                                ifelse(df$force == 8, 34,
                                                                                       NA)))))))))
        df$WINDSPEED = round(df$WINDSPEED*0.514444,0) #knots to  m/s
        df$force<-NULL
        colnames(df)[colnames(df)=="wind"] <- "WINDDIR"                         #dir in 360deg
        df$WINDDIR[df$WINDDIR == 0] <- 360 #DATRAS complains about 0 as wind direction
        return(df)
      }
      df <- processTimes(df)
      df <- processTowDets(df)
      df <- processCurrents(df)
      df <- processWind(df)
      return(df)
    }
    addPlatformDets <- function(df=NULL){
      # A = AT CAMERON
      # H = Lady Hammond
      # N = Needler
      # S = Teleost
      # V = Venture
      # T = Templeman
      addGearDets<-function(df=NULL){

        # Add Gear details --------------------------------------------------------
        #set all gear params to -9 
        df[,c("GEAR","BUOYANCY","WINGSPREAD","DOORSPREAD ","DOORSURFACE",
              "DOORTYPE","DOORWGT","GEAREXP","KITEDIM","NETOPENING","RIGGING",
              "TICKLER","WARPDEN","WARPDIA","WGTGROUNDROPE","SWEEPLNGT")] <- -9
        df$GEAR <- as.character(df$GEAR)
        df$DOORTYPE <- as.character(df$DOORTYPE)
        df$RIGGING <- as.character(df$RIGGING)
        # Universal Maritime Gear Constants ---------------------------------------
        df[,c("KITEDIM","TICKLER", "GEAREXP","RIGGING")] <- data.frame(-9,-9,-9,"BM")
        # Universal Western 2A Constants ------------------------------------------
        if (nrow(df[df$gear %in% 9, ]) > 0) df[df$gear ==9,c("GEAR","WINGSPREAD","DOORSPREAD", "DOORSURFACE","DOORWGT","NETOPENING","DOORTYPE")] <- data.frame("W2A",12.5,42.3672,4,950,4.57,"PE") #Doortype should be "PORTUGUESE"
        # year-specific changes for W2A -------------------------------------------
        if (nrow(df[df$gear == 9 & df$YEAR >= 1982 & df$YEAR < 2007, ]) > 0)  df[df$gear ==9 & (df$YEAR >= 1982 &  df$YEAR < 2007), c("WARPDIA")] <-28.6      
        if (nrow(df[df$gear == 9 & df$YEAR >= 2007 & df$YEAR < 2012, ]) > 0)  df[df$gear ==9 & (df$YEAR >= 2007 &  df$YEAR < 2012), c("WARPDIA","WARPDEN","WGTGROUNDROPE")] <-data.frame(25.4,3.17,52.6694)
        if (nrow(df[df$gear == 9 & df$YEAR < 2007, ]) > 0)   df[df$gear ==9 & df$YEAR <  2007, c("SWEEPLNGT","BUOYANCY")] <-data.frame(round(37.7952,0), round(195.16,0))
        if (nrow(df[df$gear == 9 & df$YEAR >= 2007, ]) > 0) df[df$gear ==9 & df$YEAR >= 2007, c("SWEEPLNGT","BUOYANCY")] <-data.frame(round(33.8328,0), round(199.68))
        
        # Yankee 36 Constants -----------------------------------------------------
        if (nrow(df[df$gear %in% 3, ]) > 0) df[df$gear ==3,c("GEAR","WINGSPREAD","DOORSPREAD", "DOORSURFACE","DOORWGT","NETOPENING","DOORTYPE","SWEEPLNGT", "BUOYANCY","WARPDIA","WARPDEN","WGTGROUNDROPE")] <- data.frame( "Y36",10.7,36.6,2.9,450,2.7,"WR", 36.6, 97.58, 20, -9, -9)  
        # US 4 Seam Constants -----------------------------------------------------
        if (nrow(df[df$gear %in% 15, ]) > 0) df[df$gear ==15,c("GEAR","WINGSPREAD","DOORSPREAD", "DOORSURFACE","DOORWGT","NETOPENING","DOORTYPE","SWEEPLNGT", "BUOYANCY","WARPDIA","WARPDEN","WGTGROUNDROPE")] <- data.frame( "US4",12.6,33.5,2.2,550,3.7,"PI" ,36.6,-9, -9, -9, -9)  #Doortype should be "PolyIceOval"
        if (nrow(df)>0) df$gear <- NULL #by the time we get here, we can delete this field
        return(df)
      }
      addShipDets<-function(df=NULL){
        df$SHIP <- substr(df$mission,1,3)                                       #grab ship from Mission
        df[df$SHIP == "NED","SHIP"]<-"18NE"
        df[df$SHIP == "TEM","SHIP"]<-"181C"
        df[df$SHIP == "ATC","SHIP"]<-"18AT"
        df[df$SHIP == "HAM","SHIP"]<-"18LH"
        df[df$SHIP == "TEL","SHIP"]<-"18TL"
        df[df$SHIP == "VEN","SHIP"]<-"188O"
        df[df$SHIP == "CAR","SHIP"]<- "18QL"
        df[df$SHIP == "PRINCE","SHIP"]<- "NEWID2"
        return(df)
      }
      df <- addGearDets(df)
      df <- addShipDets(df)
      return(df)
    }
    addICESStrata<-function(df=NULL){
      # decided we could use our own strata rather than the CAN1-4 DEPTHSTRATA
      colnames(df)[colnames(df)=="STRATUM"] <- "DEPTHSTRATUM"
      return(df)
    }
    addICESFields<-function(df=NULL){
      # Adopt ICES field names where possible -----------------------------------
      colnames(df)[colnames(df)=="bottom_salinity"] <- "BOTSAL"
      df$BOTSAL[df$BOTSAL == 0]<- -9                                        #treat salinity of 0 as missing
      colnames(df)[colnames(df)=="bottom_temperature"] <- "BOTTEMP"           #temp already in Cel
      colnames(df)[colnames(df)=="dur"] <- "HAULDUR"                          #already in minutes
      colnames(df)[colnames(df)=="setno"] <- "STNO"
      # df$HAULNO <- df$STNO                                                    #ICES has STNO and HAULNO - examples show them as same
      colnames(df)[colnames(df)=="strat"] <- "STRATUM"
      colnames(df)[colnames(df)=="surface_temperature"] <- "SURTEMP"
      # Populate other required fields ------------------------------------------
      df$HYDROSTNO <- -9
      df$SPEEDWATER <- -9
      df$SURCURSPEED <- -9
      df$BOTCURDIR <- -9
      df$BOTCURSPEED <- -9
      df$SWELLDIR <- -9
      df$SWELLHEIGHT <- -9
      df$SURSAL <- -9
      df$THERMOCLINE <- -9
      df$THCLINEDEPTH <- -9
      df$DATATYPE <- 'R'
      df$RECORDTYPE <- "HH"
      #df$SURVEY <- "Can-Mar"    #make up a ficticious survey name
      df$COUNTRY <- "CA"
      return(df)
    }
    getDepths<-function(df=NULL){      
      # #grab the depths (in fathoms).  if no value for DEPTH, average dmin and dmax, take the result, and convert to meters
      df$DEPTH <- NA
      df$DEPTH <- rowMeans(df[,c("dmin","dmax")], na.rm = F) #first do average
      if(length(df[!is.na(df$depth),"depth"])>0) df[!is.na(df$depth),"DEPTH"]<- df[!is.na(df$depth),"depth"]
      df$dmin <- df$dmax <- df$depth <- NULL
      df$DEPTH <- round2(df$DEPTH,0)
      return(df)
    }
    
    convertUnits <- function(df=NULL){
      # Convert units as necessary ----------------------------------------------
      if(length(df[!is.na(df$warpout),"warpout"])>0) df[!is.na(df$warpout),"warpout"]<- round(df[!is.na(df$warpout),"warpout"] * 1.8288,0)          #depth from fathoms to meters (non NA)
      if(length(df[!is.na(df$dist),"dist"])>0) df[!is.na(df$dist),"dist"]<- round(df[!is.na(df$dist),"dist"] * 1852,0)        #distance from NM to meters (non NA)
      if(length(df[!is.na(df$speed),"speed"])>0) df[!is.na(df$speed),"speed"]<- round(df[!is.na(df$speed),"speed"],1)
      colnames(df)[colnames(df)=="warpout"] <- "WARPLNGT"                     #pretty sure it's in meters\

      if(length(df[!is.na(df$DEPTH),"DEPTH"])>0) df[!is.na(df$DEPTH),"DEPTH"] <- round(df[!is.na(df$DEPTH),"DEPTH"]* 1.8288,0)
      colnames(df)[colnames(df)=="dist"] <- "DISTANCE"
      colnames(df)[colnames(df)=="speed"] <- "GROUNDSPEED"
      return(df)
    }
    addSp <- function(df=NULL){
      df$BYSPECRECCODE <- 1
      df[df$YEAR<2005,"BYSPECRECCODE"]<-6
      df$STDSPECRECCODE <- 1                                                      #(https://vocab.ices.dk/?ref=89)                                                    
      #Maybe we can use this to store information related to the changes in species ID over time?
      return(df)
    }
    finalClean <- function(df=NULL){
      df = df[,c('RECORDTYPE','QUARTER','COUNTRY','SHIP','GEAR','SWEEPLNGT',
                 'GEAREXP','DOORTYPE','STNO','HAULNO','YEAR','MONTH','DAY',
                 'TIMESHOT','DEPTHSTRATUM','HAULDUR','DAYNIGHT','SHOOTLAT',
                 'SHOOTLONG','HAULLAT','HAULLONG','STATREC','DEPTH','HAULVAL',
                 'HYDROSTNO','STDSPECRECCODE','BYSPECRECCODE','DATATYPE',
                 'NETOPENING','RIGGING','TICKLER',
                 'DISTANCE','WARPLNGT',
                 'WARPDIA','WARPDEN','DOORSURFACE','DOORWGT','DOORSPREAD',
                 'WINGSPREAD','BUOYANCY','KITEDIM','WGTGROUNDROPE','TOWDIR',
                 'GROUNDSPEED','SPEEDWATER','SURCURDIR','SURCURSPEED',
                 'BOTCURDIR','BOTCURSPEED','WINDDIR','WINDSPEED','SWELLDIR',
                 'SWELLHEIGHT','SURTEMP','BOTTEMP','SURSAL','BOTSAL',
                 'THERMOCLINE','THCLINEDEPTH','mission')]
      df[is.na(df)]<- -9
      return(df)
    }
    # Get all of the requested data
    tmp_HH <- calcValues(df)
    tmp_HH <- addPlatformDets(tmp_HH)
    tmp_HH <- addICESFields(tmp_HH)
    tmp_HH <- addICESStrata(tmp_HH)
    tmp_HH <- getDepths(tmp_HH)
    tmp_HH <- convertUnits(tmp_HH)
    tmp_HH <- addSp(tmp_HH)
    tmp_HH <- finalClean(tmp_HH)
    
    cat("Done")
    return(tmp_HH)
  }
  Mar_HL <- function(scratch_env = NULL){
    # Our "std spp" are:
    # c(10, 11, 12, 14, 16, 23, 30, 40, 41, 42, 43, 60, 70)
    # Our known 67 spp since 1970  are:
    # c(10,11,12,13,14,15,16,17,23,30,31,40,41,42,43,44,50,51,52,60,61,62,64,70,94,112,114,118,122,
    #   123,141,142,143,160,200,201,202,203,204,216,220,221,240,241,300,304,320,350,400,409,410,411,
    #   412,413,414,501,502,610,622,623,630,637,640,647,701,704,2550
    cat("\n","Generating HL... ")
    
    handleGSINF<-function(){
      df<-scratch_env$GSINF
      df <- df[,c("MISSION","SETNO", "STRAT","TYPE")]
      colnames(df)[colnames(df)=="STRAT"] <- "AREACODE"
      return(df)
    }
    handleGSCAT<-function(){
      df<-scratch_env$GSCAT
      doSubFact <- function(df = NULL){
        # SubFactor – sub-sampling factor by haul, species, sex, length. Value = or > 1. 
        # Make sure that TotalNo =  NoMeas x SubFactor
        df$SUBFACTOR <- -9
        df[df$TOTWGT > 0 & df$SAMPWGT > 0,"SUBFACTOR"] <- df[df$TOTWGT > 0 & df$SAMPWGT > 0,"TOTWGT"]/df[df$TOTWGT > 0 & df$SAMPWGT > 0,"SAMPWGT"]
        
        return(df)
      }
      #rename some fields to match ICES
      df = doSubFact(df)
      colnames(df)[colnames(df)=="TOTNO"] <- "TOTALNO"
      colnames(df)[colnames(df)=="SAMPWGT"] <- "SUBWGT"
      #   # make subwgt -9 for all cases where no subsampling occurred
      if (nrow(df[df$TOTWGT == df$SUBWGT,])>0) df[df$TOTWGT == df$SUBWGT,"SUBWGT"]<--9
      if (length(df[which(df$SUBWGT == 0),"SUBWGT"])>0) df[which(df$SUBWGT == 0),"SUBWGT"] <--9
      df$SUBWGT <- round2(df$SUBWGT,0)
      return(df)
    }
    handleGSDET<-function(){
      addSexCodes <- function(df = NULL){
        # change to ICES sex codes
        df$SEX<-NA
        df[which(is.na(df$FSEX)),"SEX"]<--9                 #undetermined
        df[which(df$FSEX==0),"SEX"]<-"U"                    #unknown (0)
        df[which(df$FSEX==1),"SEX"]<-"M"                    #male (1)
        df[which(df$FSEX==2),"SEX"]<-"F"                    #female (2)
        df[which(df$FSEX==3),"SEX"]<-"B"                    #berried female (3)
        df$FSEX<-NULL
        return(df)
      }
      addDevStage <- function(df = NULL){
        # change to ICES devstage codes
        df$DEVSTAGE<--9
        return(df)
      }
      handleDetSpecies<-function(df = NULL){
        #herring were recorded in mm starting SUMMER 2016 
        #want to convert all measurements to mm (Summer 2016 ie.NED2016016 was in mm, Spring was in cm)
        
        # - this converts to cm (same units as LGRP) MISSION reqd to avoid Spring surveys, where they were recorded in cm
        if (nrow(df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <=2016 & (!df$MISSION %in% c("NED2016016")))),])>0)
          df[df$SPEC == 60 & (substr(df$MISSION, 4,7)<=2016 & (!df$MISSION %in% c("NED2016016"))),"FLEN"] <- df[df$SPEC == 60 & (substr(df$MISSION, 4,7)<=2016 & (!df$MISSION %in% c("NED2016016"))),"FLEN"]*10
        return(df)
      }
      df<-scratch_env$GSDET[,names(scratch_env$GSDET) %in% c("MISSION", "SETNO", "SPEC", "FMAT", "FLEN", "CLEN","FWT", "FSEX", "SIZE_CLASS", "SPECIMEN_ID")]
      colnames(df)[colnames(df)=="SIZE_CLASS"] <- "CATIDENTIFIER"
      df <- addSexCodes(df)
      df <- addDevStage(df)
      df <- handleDetSpecies(df)
      # if (nrow(df[which(is.na(df$FWT)),])>0) df[which(is.na(df$FWT)),"FWT"]<-0
      # # df$FWT[is.na(df$FWT)] <- 1
      df$FMAT <- NULL
      return(df)
    }
    handleSpecies<-function(){
      
      #get all of the species - the CODE, APHIAID, LGRP
      SPP <- sort(unique(c(unique(scratch_env$GSCAT$SPEC), unique(scratch_env$GSDET$SPEC))))
      SPP <- data.frame(SPEC = SPP)
      SPP <- addLenMeasInfo(SPP)
    }
    GSDET <- handleGSDET()
    GSCAT <- handleGSCAT()
    GSINF <- handleGSINF()
    SPP <- handleSpecies()
    
    #subfactor - maybe this needs to be determined first?
    forAgg <- merge(SPP, GSDET[,c("SPEC", "MISSION","SETNO", "FLEN", "FWT", "SEX","CATIDENTIFIER")], by = "SPEC")

    forAgg <- addLNGTCLASS(forAgg)
    # forAgg$LNGTCLASS<- ceiling(forAgg$FLEN/forAgg$LNGTCODE) * forAgg$LNGTCODE
    forAgg$FLEN<-NULL
    
    tmp_CATCATCHWGT <- stats::aggregate(
      x = list(CATCATCHWGT = forAgg$FWT),
      by = list(SPEC = forAgg$SPEC,
                MISSION = forAgg$MISSION,
                SETNO = forAgg$SETNO,
                SEX = forAgg$SEX,
                CATIDENTIFIER = forAgg$CATIDENTIFIER
      ),
      sum
    )
    tmp_CATCATCHWGT$CATCATCHWGT <- round2(tmp_CATCATCHWGT$CATCATCHWGT,0)
    tmp_HLNOATLNGT <- stats::aggregate(
      x = list(HLNOATLNGT = forAgg$LNGTCLASS),
      by = list(SPEC = forAgg$SPEC,
                MISSION = forAgg$MISSION,
                SETNO = forAgg$SETNO,
                SEX = forAgg$SEX,
                CATIDENTIFIER = forAgg$CATIDENTIFIER,
                LNGTCLASS = forAgg$LNGTCLASS
      ),
      length
    )
    
    # tmp_HL1 <- getHLNOATLNGT(forAgg)
    tmp_HL2 <- merge(tmp_CATCATCHWGT, tmp_HLNOATLNGT, all.y=T)
    tmp_HL2 <- tmp_HL2[,c("SPEC", "MISSION","SETNO","SEX","LNGTCLASS","CATIDENTIFIER","HLNOATLNGT", "CATCATCHWGT")]
    
    # tmp_HL$DEVSTAGE <- -9 #devstage (i.e. maturity) not used at a haul-level (i.e. GSCAT)
    tmp_HL2 = merge(tmp_HL2, GSCAT, all.x = T)
    
    tmp_NoMeas= unique(as.data.frame(as.list(stats::aggregate(
      x = list(NOMEAS = tmp_HL2$HLNOATLNGT),
      by = list(SPEC = tmp_HL2$SPEC,
                MISSION = tmp_HL2$MISSION,
                SETNO = tmp_HL2$SETNO,
                CATIDENTIFIER = tmp_HL2$CATIDENTIFIER,
                SEX = tmp_HL2$SEX
      ),
      sum
    ))))
    
    
    tmp_HL2 = merge(tmp_HL2, tmp_NoMeas)
    
    tmp_HL2 <- merge(tmp_HL2, SPP)
    tmp_HL3 <- merge(tmp_HL2, GSINF)
    
    tmp_HL3$SPECVAL <- NA
    tmp_HL3$SPECVAL <- ifelse(tmp_HL3$TYPE == 3, 0,
                              ifelse(tmp_HL3$LNGTCLASS == -9, 7,
                                     ifelse(tmp_HL3$TOTALNO <= 0 & tmp_HL3$TOTWGT >0 , 6, 1)))
    tmp_HL3$TOTWGT<-tmp_HL3$TYPE <- NULL
    tmp_HL3[which(tmp_HL3$CATCATCHWGT == 0),"CATCATCHWGT"] <- -9
    tmp_HL3[is.na(tmp_HL3)]<- -9 #catch all to turn all NAs to -9
    tmp_HL3[which(tmp_HL3$CATCATCHWGT==-9 & tmp_HL3$TOTALNO ==9),"SPECVAL"]<- 5
    
    cat("Done")
    return(tmp_HL3)
  }
  Mar_CA <- function(scratch_env = NULL){
    cat("\n","Generating CA... ")
    handleGSDET<-function(){
      addSexCodes <- function(df = NULL){
        # change to ICES sex codes
        df$SEX<-NA
        df[which(is.na(df$FSEX)),"SEX"]<--9                 #undetermined
        df[which(df$FSEX==0),"SEX"]<-"U"                    #unknown (0)
        df[which(df$FSEX==1),"SEX"]<-"M"                    #male (1)
        df[which(df$FSEX==2),"SEX"]<-"F"                    #female (2)
        df[which(df$FSEX==3),"SEX"]<-"B"                    #berried female (3)
        df$FSEX<-NULL
        return(df)
      }
      addMaturity<-function(df = NULL){
        df$MATURITYSCALE<- "M6"
        df$MATURITY<- -9
        if (nrow(df[which(is.na(df$FMAT)),])>0)         df[which(is.na(df$FMAT)),"MATURITY"]<--9
        if (nrow(df[which(df$FMAT==0),])>0)             df[which(df$FMAT==0),"MATURITY"]<--9 #DClark indicates that 0 should be NULL
        if (nrow(df[which(df$FMAT==9),])>0)             df[which(df$FMAT==9),"MATURITY"]<--9 #DClark indicates that 9 is probably an error
        if (nrow(df[which(df$FMAT %in% c(1)),])>0)      df[which(df$FMAT %in% c(1)),"MATURITY"]<-61
        if (nrow(df[which(df$FMAT %in% c(2,3,4)),])>0)  df[which(df$FMAT %in% c(2,3,4)),"MATURITY"]<-62
        if (nrow(df[which(df$FMAT %in% c(5)),])>0)      df[which(df$FMAT %in% c(5)),"MATURITY"]<-63
        if (nrow(df[which(df$FMAT %in% c(6)),])>0)      df[which(df$FMAT %in% c(6)),"MATURITY"]<-64
        if (nrow(df[which(df$FMAT %in% c(7,8)),])>0)   df[which(df$FMAT %in% c(7,8)),"DEVSTAGE"]<-65
        df$FMAT <- NULL
        return(df)
      }
      addAges <- function(df = NULL){
        df$PLUSGR <- -9
        df$AGERINGS <- df$AGE
        df[which(is.na(df$AGERINGS)),"AGERINGS"]<--9 
        
        df$AGESOURCE <- -9
        if (nrow(df[which(df$AGMAT==1),])>0)             df[which(df$AGMAT==1),"AGESOURCE"]<-"OT"
        if (nrow(df[which(df$AGMAT==2),])>0)             df[which(df$AGMAT==2),"AGESOURCE"]<-"SC"
        if (nrow(df[which(df$AGMAT==4),])>0)             df[which(df$AGMAT==4),"AGESOURCE"]<-"VR"
        #no ICES codes for "rays", or "otolith AND scale"
        df$AGMAT <- NULL
        df$OTGRADING <- -9  #Grading of the otolith reading
        df$AGEPREPMET <- -9 #Age reading preparation method
        return(df)
      }
      # addPreStomSamp <-function(df=NULL){
      #   return(df)
      #   
      # }
      handleDetSpecies<-function(df = NULL){
        #herring were recorded in mm starting SUMMER 2016 - this converts cm to mm
        if (nrow(df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <2016 | (df$MISSION %in% c("TEL2016002","TEL2016003")))),])>0){
          df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <2016 | (df$MISSION %in% c("TEL2016002","TEL2016003")))),"FLEN"]<-
            df[which(df$SPEC == 60 & (substr(df$MISSION, 4,7) <2016 | (df$MISSION %in% c("TEL2016002","TEL2016003")))),"FLEN"]*10
        }
        return(df)
      }
      
      df<-scratch_env$GSDET[,names(scratch_env$GSDET) %in% c("MISSION", "SETNO", "SPEC", "FSHNO", "FMAT", "FLEN", "CLEN","FWT", "FSEX", "SIZE_CLASS","AGE","SPECIMEN_ID")]
      # colnames(df)[colnames(df)=="SIZE_CLASS"] <- "CATIDENTIFIER"
      df <- addSexCodes(df)
      df <- addMaturity(df)
      df <- addAges(df)
      df <- handleDetSpecies(df)
      if (nrow(df[which(is.na(df$FWT)),])>0) df[which(is.na(df$FWT)),"FWT"]<-0
      df$FMAT <- NULL
      return(df)
    }
    handleSpecimens<- function(){    
      
      df<-scratch_env$GS_LV1_OBSERVATIONS[paste0(scratch_env$GS_LV1_OBSERVATIONS$MISSION,"_",
                                                 scratch_env$GS_LV1_OBSERVATIONS$SETNO,"_",
                                                 scratch_env$GS_LV1_OBSERVATIONS$SPEC,"_",
                                                 scratch_env$GS_LV1_OBSERVATIONS$SIZE_CLASS) %in%
                                            paste0(GSDET$MISSION,"_",
                                                   GSDET$SETNO,"_",
                                                   GSDET$SPEC,"_",
                                                   GSDET$SIZE_CLASS),  c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "LV1_OBSERVATION")]
      if (nrow(df)==0)return(NULL)
      df$GENSAMP <- -9 #Flag whether genetic sample was taken
      
      if (nrow(df[grep(pattern = "Genetic",x = df$LV1_OBSERVATION,ignore.case = T),])>0) df[grep(pattern = "Genetic",x = df$LV1_OBSERVATION,ignore.case = T),"GENSAMP"]<-"Y"
      df$STOMSAMP <- -9 #Flag whether stomach sampling was performed
      
      if (nrow(df[grep(pattern = "Stomach Number",x = df$LV1_OBSERVATION,ignore.case = T),])>0) df[grep(pattern = "Stomach Number",x = df$LV1_OBSERVATION,ignore.case = T),"STOMSAMP"]<-"Y"
      df$PARSAMP <- -9 #Flag whether parasites sampling was performed    
      #colnames(df)[colnames(df)=="FSHNO"] <- "FISHID" #these are numbered per mission/set/species
      # colnames(df)[colnames(df)=="FWT"] <- "INDWGT"
      df$LV1_OBSERVATION<-NULL
      df = unique(df)
      return(df)
    }
    #make a df
    GSDET <- handleGSDET()
    
    GSspecimens <- handleSpecimens() 
    if (is.null(GSspecimens)){
      GSDET$GENSAMP <- NA
      GSDET$STOMSAMP <- NA
      GSDET$PARSAMP <- NA
    }else{
      GSDET <- merge(GSDET, unique(GSspecimens[GSspecimens$GENSAMP=="Y", c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "GENSAMP")]), all.x=T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID"))
      GSDET <- merge(GSDET, unique(GSspecimens[GSspecimens$STOMSAMP=="Y", c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "STOMSAMP")]), all.x=T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID"))
      GSDET <- merge(GSDET, unique(GSspecimens[GSspecimens$PARSAMP=="Y", c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID", "PARSAMP")]), all.x=T, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SPECIMEN_ID"))
    }
    GSDET$AREATYPE <- 27
    
    # if GSDET$INDWGT[GSDET$INDWGT == 0] <- -9 #datras doesn't like values of 0 for wgt
    colnames(GSDET)[colnames(GSDET)=="SIZE_CLASS"] <- "CATIDENTIFIER"
    
    colnames(GSDET)[colnames(GSDET)=="MISSION"] <- "mission"
    colnames(GSDET)[colnames(GSDET)=="SETNO"] <- "STNO"
    
    forAgg <- unique(GSDET[,c("mission","STNO","SPEC","SEX","MATURITY", "AGERINGS", "FWT", "FLEN")])
    
    forAgg <- addLenMeasInfo(forAgg)
    
    forAgg <- addLNGTCLASS(forAgg)
    forAgg$FLEN <- NULL
    #if any have na for FWT, they should be zero or aggregate will drop them
    if (length(forAgg[is.na(forAgg$FWT),"FWT"])>0) forAgg[is.na(forAgg$FWT),"FWT"]<-0
    #Amount of fish (CANOATLNGT)and mean wt (INDWGT)at the given category (per haul, species, length class, sex, maturity, age)
    forAgg_cnt= unique(as.data.frame(as.list(stats::aggregate(
      x = list(CANOATLNGT = forAgg$mission),
      by = list(SPEC = forAgg$SPEC,
                mission = forAgg$mission,
                STNO = forAgg$STNO,
                LNGTCLASS = forAgg$LNGTCLASS, 
                LNGTCODE = forAgg$LNGTCODE,
                SEX = forAgg$SEX,
                MATURITY = forAgg$MATURITY,
                AGERINGS = forAgg$AGERINGS
      ),
      length
    ))))
    forAgg_indwgt= unique(as.data.frame(as.list(stats::aggregate(
      x = list(INDWGT = forAgg$FWT),
      by = list(SPEC = forAgg$SPEC,
                mission = forAgg$mission,
                STNO = forAgg$STNO,
                LNGTCLASS = forAgg$LNGTCLASS,
                LNGTCODE = forAgg$LNGTCODE,
                SEX = forAgg$SEX,
                MATURITY = forAgg$MATURITY,
                AGERINGS = forAgg$AGERINGS
      ),
      mean
    ))))
    #turn the zeroes back to -9
    if (length(forAgg_indwgt[forAgg_indwgt$INDWGT==0,"INDWGT"])>0) forAgg_indwgt[forAgg_indwgt$INDWGT==0,"INDWGT"]<- -9
    
    aggRes <- merge(forAgg_indwgt, forAgg_cnt)
    #all
    # CA <- c("RECORDTYPE", "QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT",
    #         "DOORTYPE","STNO","HAULNO","YEAR","SPECCODETYPE",
    #         "SPECCODE","AREATYPE","AREACODE","LNGTCODE","LNGTCLASS","SEX",
    #         "MATURITY","PLUSGR","AGERINGS","CANOATLNGT","INDWGT",
    #         "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
    #         "AGEPREPMET","OTGRADING","PARSAMP")
    #from HH/HL"mission","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","DEPTHSTRATUM"
    # CA <- c("SPECCODETYPE", "SPECCODE","AREATYPE","AREACODE","PLUSGR",
    #         "MATURITYSCALE","FISHID","GENSAMP","STOMSAMP","AGESOURCE",
    #         "AGEPREPMET","OTGRADING","PARSAMP")
    
    #aggRes has "LNGTCODE","LNGTCLASS","SEX", "MATURITY","AGERINGS","CANOATLNGT","INDWGT",
    #GSspecimens has 
    
    # GSDET$SPECIMEN_ID<- GSDET$FSHNO<- GSDET$CLEN<- GSDET$DEVSTAGE<-GSDET$FLEN<-GSDET$AGE <- NULL
    aggRes$FISHID <- -9
    aggRes$RECORDTYPE<-"CA"
    cat("Done")
    return(aggRes)
  }
  
  # Get all of the requested data
  for (y in 1:length(years)){
    for (s in 1:length(survey)){
      cat(paste0("\n","Working on ", years[y], " ",survey[s]))
      nm = paste0(survey[s],"_",years[y])
      fullnm <- paste0(nm,"_",timestamp,".csv")
      tmp_env <- getRaw(years=years[y], survey = survey[s],
                        cxn = cxn,
                        data.dir = data.dir)
      
      #convert gscat values to grams (gsdet already in g)
      if (nrow(tmp_env$GSINF)==0){
        message("\nNo data found matching parameters")
        theFile <- file.create(fullnm)
        results[[nm]]<-NA
        utils::write.csv(x = NA, file = paste0(fullnm,"_noResults.csv"), row.names = F)
        next
      }
      tmp_env$GSCAT$SAMPWGT <- tmp_env$GSCAT$SAMPWGT*1000
      tmp_env$GSCAT$TOTWGT <- tmp_env$GSCAT$TOTWGT*1000

      tmp_HH <- Mar_HH(scratch_env = tmp_env, survey = survey)
      tmp_HL <- Mar_HL(scratch_env = tmp_env)
      tmp_HL<-merge(tmp_HH[,c("mission","RECORDTYPE","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR")],
                    tmp_HL, all.y = T, by.x=c("mission", "STNO"), by.y=c("MISSION","SETNO"))
      tmp_HL$RECORDTYPE <- "HL"
      
      tmp_CA <- Mar_CA(scratch_env = tmp_env)
      tmp_CA<-merge(tmp_HH[,c("mission","QUARTER","COUNTRY","SHIP","GEAR","SWEEPLNGT","GEAREXP","DOORTYPE","STNO","HAULNO","YEAR","DEPTHSTRATUM")], tmp_CA, all.y = T)
      
      tmp_HL$LNGTCODE <- as.character(tmp_HL$LNGTCODE) 
      tmp_HL$LNGTCODE <- gsub('0.1', '.', tmp_HL$LNGTCODE)
      tmp_CA$LNGTCODE <- as.character(tmp_CA$LNGTCODE) 
      tmp_CA$LNGTCODE <- gsub('0.1', '.', tmp_CA$LNGTCODE)
      badTows <- tmp_HH[tmp_HH$HAULVAL == "I","HAULNO"]
      if (length(badTows)>0){
        tmp_HL[tmp_HL$HAULNO %in% badTows,"SPECVAL"] <-0
      }
      colnames(tmp_CA)[colnames(tmp_CA)=="DEPTHSTRATUM"] <- "AREACODE"
      tmp_CA[is.na(tmp_CA)]<- -9
      SPP <- sort(unique(c(unique(tmp_env$GSCAT$SPEC), unique(tmp_env$GSDET$SPEC))))
      SPP <- data.frame(SPEC = SPP)
      GSSPECIES_CODES <- tmp_env$GSSPECIES_NEW[tmp_env$GSSPECIES_NEW$CODE %in% SPP$SPEC,c("CODE","APHIA_ID")]

      colnames(GSSPECIES_CODES)[colnames(GSSPECIES_CODES)=="APHIA_ID"] <- "SPECCODE"
      GSSPECIES_CODES$SPECCODETYPE <- -9
      GSSPECIES_CODES[!is.na(GSSPECIES_CODES$SPECCODE),"SPECCODETYPE"]<- "W"
      GSSPECIES_CODES[is.na(GSSPECIES_CODES$SPECCODE),"SPECCODE"]<- -9
      
      tmp_HL <- merge(tmp_HL, GSSPECIES_CODES, all.x=T, by.x= "SPEC", by.y="CODE")
      tmp_CA <- merge(tmp_CA, GSSPECIES_CODES, all.x=T, by.x= "SPEC", by.y="CODE")
      HHMissing <- setdiff(ord_HH, names(tmp_HH))
      tmp_HH[HHMissing]<- -9
      
      HLMissing <- setdiff(ord_HL, names(tmp_HL))
      tmp_HL[HLMissing]<- -9
      
      CAMissing <- setdiff(ord_CA, names(tmp_CA))
      tmp_CA[CAMissing]<- -9
      
      ord_HH<-ord_HH[ord_HH %in% names(tmp_HH)]
      ord_HL<-ord_HL[ord_HL %in% names(tmp_HL)]
      ord_CA<-ord_CA[ord_CA %in% names(tmp_CA)]
      tmp_HH<-tmp_HH[,ord_HH]
      tmp_HL<-tmp_HL[,ord_HL]
      tmp_CA<-tmp_CA[,ord_CA]
      if (debug){
        cat("Just getting 1 set and 1 species","\n")
        CAsp =stats::aggregate(tmp_CA$SPECCODE,
                               by = list(
                                 STNO = tmp_CA$STNO,
                                 SPECCODE = tmp_CA$SPECCODE
                               ),
                               length
        )
        CAspMAX <- CAsp[which.max(CAsp$x),]
        # tmp_CA <- 
        tmp_CA <- tmp_CA[tmp_CA$STNO == CAspMAX$STNO & tmp_CA$SPECCODE == CAspMAX$SPECCODE, ]
        tmp_HL <- tmp_HL[tmp_HL$STNO == CAspMAX$STNO & tmp_HL$SPECCODE == CAspMAX$SPECCODE, ]
        tmp_HH <- tmp_HH[tmp_HH$STNO == CAspMAX$STNO, ]
      }
      names(tmp_HH) <- c("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt",  "GearEx", "DoorType", "StNo", "HaulNo", 
                         "Year", "Month", "Day", "TimeShot", "DepthStratum", "HaulDur", "DayNight", "ShootLat", "ShootLong", "HaulLat", 
                         "HaulLong", "StatRec", "Depth", "HaulVal", "HydroStNo", "StdSpecRecCode", "BySpecRecCode", "DataType", "Netopening", "Rigging", 
                         "Tickler", "Distance", "Warplngt", "Warpdia", "WarpDen", "DoorSurface", "DoorWgt", "DoorSpread", "WingSpread", "Buoyancy", 
                         "KiteDim", "WgtGroundRope", "TowDir", "GroundSpeed", "SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir", "BotCurSpeed", "WindDir", 
                         "WindSpeed", "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal", "ThermoCline", "ThClineDepth", "CodendMesh", 
                         "SecchiDepth", "Turbidity", "TidePhase", "TideSpeed", "PelSampType", "MinTrawlDepth", "MaxTrawlDepth")
      
      names(tmp_HL) <- c("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "SpecCodeType", "SpecCode", "SpecVal", "Sex", "TotalNo", "CatIdentifier", "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt", "DevStage", "LenMeasType")
      names(tmp_CA) <- c("RecordType", "Quarter", "Country", "Ship", "Gear", "SweepLngt", "GearEx", "DoorType", "StNo", "HaulNo", "Year", "SpecCodeType", "SpecCode", "AreaType", "AreaCode", "LngtCode", "LngtClass", "Sex", "Maturity", "PlusGr", "AgeRings", "CANoAtLngt", "MaturityScale", "FishID", "GenSamp", "StomSamp", "AgeSource", "AgePrepMet", "OtGrading", "ParSamp", "LiverWeight")
      
      # colnames(tmp_HH)[colnames(tmp_HH)=="GEAREXP"] <- "GEAREX"
      # colnames(tmp_HL)[colnames(tmp_HL)=="GEAREXP"] <- "GEAREX"
      
      
      SHIPS <- unique(tmp_HH$Ship)
      for (s in 1:length(SHIPS)){
        nmShip = paste0(nm,"_",SHIPS[s])
        fullnmShip <- gsub(".csv", paste0("_",SHIPS[s],".csv"), fullnm)
        this_tmp_HH <- tmp_HH[tmp_HH$Ship == SHIPS[s],]
        this_tmp_HL <- tmp_HL[tmp_HL$Ship == SHIPS[s],]
        this_tmp_CA <- tmp_CA[tmp_CA$Ship == SHIPS[s],]
        
        if(csv){
          theFile <- file.create(fullnmShip)
          suppressWarnings(utils::write.table(x = this_tmp_HH, file = fullnmShip, row.names = F, col.names = T, quote = FALSE, sep = ","))
          suppressWarnings(utils::write.table(x = this_tmp_HL, file = fullnmShip, row.names = F, col.names = T, quote = FALSE, sep = ",", append = T))
          suppressWarnings(utils::write.table(x = this_tmp_CA, file = fullnmShip, row.names = F, col.names = T, quote = FALSE, sep = ",", append = T))
          if (debug){
            utils::write.csv(x = this_tmp_HH, file = paste0(gsub(pattern = ".csv", replacement = "", x = fullnmShip),"_HH_debug.csv"), row.names = F)
            utils::write.csv(x = this_tmp_HL, file = paste0(gsub(pattern = ".csv", replacement = "", x = fullnmShip),"_HL_debug.csv"), row.names = F) 
            utils::write.csv(x = this_tmp_CA, file = paste0(gsub(pattern = ".csv", replacement = "", x = fullnmShip),"_CA_debug.csv"), row.names = F)
          }
          cat("\n",paste0("File written to ", getwd(),"/", fullnmShip))
        }
        thisyrShp <- list(HH = this_tmp_HH, HL = this_tmp_HL, CA = this_tmp_CA)
        results[[nmShip]]<-thisyrShp
      }
      

      tmp_env<-NULL
    }
  }
  return(results)
}
