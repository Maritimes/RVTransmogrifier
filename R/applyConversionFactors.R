#' @title applyConversionFactors
#' @description Apply vessel conversion factors to standardize catch data across different research vessels. Merges GSDET and GSCAT data, applies length-weight relationships, calculates sample ratios, and applies vessel-specific conversion factors for abundance and biomass to standardize historical data to current vessel (CAR/CAB) equivalents.
#' @param cxn A valid Oracle connection object. This parameter allows you to 
#' pass an existing connection, reducing the need to establish a new connection 
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, GSDET, GSMISSIONS, and GSSTRATUM.
#' @return A data frame containing standardized catch data with vessel conversion factors applied, including fields for raw and converted abundance (TOTNO) and biomass (TOTWGT), along with metadata indicating which conversion factors were used.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function downloads additional reference tables (GSCONVERSIONS, GSSPEC2) if not already available. It handles seasonal differences (SPRING/SUMMER) and multiple vessel transitions (NED/TEM to TEL/VEN to CAR/CAB).
#' @importFrom dplyr filter select rename group_by ungroup mutate case_when left_join anti_join across
#' @export
applyConversionFactors <- function(cxn=NULL, tblList){
  season <- unique(tblList$GSMISSION$SEASON)
  if (length(season)>1)stop("The function can only handle a single season of data")
  if (!season %in% c('SPRING','SUMMER'))stop("This function can only handle SUMMER or SPRING/GEORGES") 
  
  # get a few extra tables we'll need
  message("Need to pass a cxn")
  Mar.utils::get_data_tables('groundfish', cxn=cxn, data.dir = get_pesd_rvt_dir(), 
                             tables=c("GSCONVERSIONS", "GSSPEC2"),force.extract = F)
  
  # pull the As and Bs for summer (or all).  If multiple exist for the same SPEC/SEX, choose the higher R2 value
  # also rename FSEX to maintain clarity during joining
  
  ABs <- GSSPEC2 |>
    filter(SEASON == ifelse(season=="SUMMER", "SUMMER", "WINTER")) |> 
    # filter(grepl(paste(season, "ALL", sep = "|"), SEASON)) |>  #"WINTER|ALL"
    group_by(SPEC, FSEX) |>
    filter(R2 == max(R2)) |>
    ungroup() |>
    rename(FSEX_key = FSEX)
  
  
  GSDET <- tblList$GSDET
  GSCAT_mrg<-tblList$GSCAT
  GSMISSIONS <- tblList$GSMISSIONS
  # ensure that a value for sex exists and that berried females count as females
  GSDET$FSEX<-ifelse(!GSDET$FSEX %in% c(1,2,3),0,GSDET$FSEX)
  GSDET$FSEX<-ifelse(GSDET$FSEX %in% c(3),2,GSDET$FSEX)
  GSDET$SRC <- "GSDET"
  
  ########################################################################################
  ########   Start of removing the GSDET data that will be used from GSCAT ##############
  ########################################################################################
  #This is where I have added most of the code to filter out any species that don't need conversions done by FLEN.
  
  #First two lines just pluck out any info pertaining to SPECs that have conversion factors based on FLENs for the TEL/VEN to CAR/CAB and the NED/TEM to TEL/VEN
  GSCAT_SPEC_LIST_TELVEN<-subset(GSCONVERSIONS,!CF_VALUE==1 & CF_METRIC=="ABUNDANCE" & FROM_VESSEL=="TEL_VEN")
  GSCAT_SPEC_LIST_NEDTEM<-subset(GSCONVERSIONS,!CF_VALUE==1 & CF_METRIC=="ABUNDANCE" & FROM_VESSEL=="NED_TEM")
  
  #The next six lines denote which records we want to remove cause they will use data from GSCAT instead. If a species requires a conversion factor for the TEL/VEN to CAR/CAB, those species have to have those conversion factors applied to all of the historical vessels used so thats why the NED, TEM, ATC and HAM all get the same treatment here.
  GSDET$Remove<-ifelse(grepl("TEL",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC,"N","Y")
  GSDET$Remove<-ifelse(grepl("VEN",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("NED",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("TEM",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("ATC",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("HAM",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC,"N",GSDET$Remove)
  
  #Repeat the exact same step for the NED/TEM to TEL/VEN conversions. A lot of these species will already have a "N" for them from the above code, but this will catch any that don't.
  GSDET$Remove<-ifelse(grepl("NED",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_NEDTEM$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("TEM",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_NEDTEM$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("ATC",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_NEDTEM$SPEC,"N",GSDET$Remove)
  GSDET$Remove<-ifelse(grepl("HAM",GSDET$MISSION) & GSDET$SPEC %in% GSCAT_SPEC_LIST_NEDTEM$SPEC,"N",GSDET$Remove)
  
  #We don't need to repeat this step for the ATC/HAM to NED/TEM conversions cause the only species used in those are already accounted for in the above code.
  #This just removes all those unwanted records and gives a quick list of species included to make sure they make sense
  GSDET<- subset(GSDET,Remove=="N")
  
  
  ########################################################################################
  ########   End of removing the GSDET data that will be used from GSCAT #################
  ########################################################################################  
  
  
  
  
  ########################################################################################
  ########   Start of applying Sample ratios to GSDET CLENs ##############################
  ########################################################################################
  #I moved this section up here just cause it made sense to do it earlier when doing GSDET stuff
  #Adjust for sample_ratio from gscat
  #Sample weight may also be 0 if it was the same as total weight
  
  GSCAT_mrg$SAMPWGT<-ifelse(GSCAT_mrg$SAMPWGT==0,GSCAT_mrg$TOTWGT,GSCAT_mrg$SAMPWGT)
  GSCAT_mrg$SAMPWGT<-ifelse(is.na(GSCAT_mrg$SAMPWGT),GSCAT_mrg$TOTWGT,GSCAT_mrg$SAMPWGT)
  GSCAT_mrg$SAMPTOT_Ratio<-GSCAT_mrg$TOTWGT/GSCAT_mrg$SAMPWGT
  GSCAT_mrg$SAMPTOT_Ratio<-ifelse(is.nan(GSCAT_mrg$SAMPTOT_Ratio),1,GSCAT_mrg$SAMPTOT_Ratio)#You also cant get ratios if total weight is 0. this sets it to one
  
  ###########################################################################################################################################
  ####################### I THINK THIS MERGE LINE GOT DELETED, I ADDED IT BACK IN ###########################################################
  ###########################################################################################################################################
  GSDET<-merge(GSDET,GSCAT_mrg[,c("MISSION", "SETNO", "SPEC","SAMPTOT_Ratio","SIZE_CLASS")],by=c("MISSION", "SETNO", "SPEC","SIZE_CLASS"))
  
  
  GSDET$CLEN<-GSDET$CLEN*GSDET$SAMPTOT_Ratio
  
  GSDET<- GSDET  |>  select(-c(Remove,SAMPTOT_Ratio))
  
  
  
  
  ########################################################################################
  ########   End of applying Sample ratios to GSDET CLENs ################################
  ########################################################################################  
  
  # find records in GSCAT that don't have associated data in GSDET
  
  GSCAT_mrg<- anti_join(GSCAT_mrg, GSDET, by = c("MISSION", "SETNO", "SPEC"))
  
  
  ########################################################################################
  #Ths line can be deleted, TOTNO in gscat already accounts for the sample ratio, this only applies to gsdet data
  # GSCAT_mrg$TOTNO<-GSCAT_mrg$TOTNO*GSCAT_mrg$SAMPTOT_Ratio #Ths line needs to be deleted, TOTNO in gscat already accounts for the sample ratio, this only applies to gsdet data
  ########################################################################################
  
  
  
  message("Corrected TOTNO for subsampled weights")
  
  GSCAT_mrg <- GSCAT_mrg |>
    select(MISSION, SETNO, SPEC, TOTWGT, TOTNO) |>
    rename(FWT = TOTWGT, CLEN = TOTNO) |>
    mutate(FWT = FWT * 1000,  # Convert FWT from kg to g
           SRC = "GSCAT",     # Note source for unanticipated debugging
           FLEN = NA,         # GSCAT does not have values for FLEN
           FSEX= 0           # GSCAT does not have values for FSEX 
           #       AGMAT = NA        # GSCAT does not have values for age material
           #     AGE = NA,          # GSCAT does not have ages
           #     FMAT = 0           # GSCAT does not have values for maturity
    )
  
  message("Some recs had weight = 9999 (e.g. snowcrab, scallops). Placeholder?" )
  
  CATDET_p <- rbind.data.frame(GSCAT_mrg, GSDET[,c("MISSION", "SETNO", "SPEC",  "FSEX", "FWT", "FLEN", "CLEN", "SRC")])  #"","AGMAT", "AGE", "FMAT")])
  # remove these records that aren't actually species
  CATDET_p <- CATDET_p[CATDET_p$SPEC<9500,]
  # these are unidentified species - not helpful
  CATDET_p <- CATDET_p[!(CATDET_p$SPEC %in% c(1091:1095)),]
  # this is literal trash
  CATDET_p <- CATDET_p[!(CATDET_p$SPEC %in% c(9400)),]
  message("Dropping records for unidentified species and non species records (e.g.eggs,  mud, oil)")
  
  # CATDET - merge the AB info with GSDET/GSCAT
  # 1 - GSDET_SEXED - attempt to merge on SPEC and FSEX
  CATDET_SEXED_p <- merge(CATDET_p, ABs[,c("SPEC","FSEX_key","LENWT_A", "LENWT_B")], 
                          by.x = c("SPEC", "FSEX"), by.y = c("SPEC", "FSEX_key"), all.x = TRUE, all.y = FALSE)
  CATDET_SEXED <- CATDET_SEXED_p[!is.na(CATDET_SEXED_p$LENWT_A) & !is.na(CATDET_SEXED_p$LENWT_B) ,]
  # 2 - GSDET_UNSEXED - any records that couldn't be merged on SPEC and FSEX should be merged on SPEC alone
  CATDET_UNSEXED_p <- CATDET_SEXED_p[is.na(CATDET_SEXED_p$LENWT_A) & is.na(CATDET_SEXED_p$LENWT_B) ,]
  CATDET_UNSEXED_p<- CATDET_UNSEXED_p[,names(CATDET_UNSEXED_p) %in% names(GSDET)]
  CATDET_UNSEXED_p <- merge(CATDET_UNSEXED_p, unique(ABs[ABs$FSEX_key==0,c("SPEC","LENWT_A", "LENWT_B")]), 
                            by.x = "SPEC", by.y = "SPEC", all.x = TRUE, all.y = FALSE)
  CATDET_UNSEXED <- CATDET_UNSEXED_p[!is.na(CATDET_UNSEXED_p$LENWT_A) & !is.na(CATDET_UNSEXED_p$LENWT_B) ,]
  # 3 - remaining_U - any records that could not be merged on SPEC are retained
  remaining_U <- CATDET_UNSEXED_p[is.na(CATDET_UNSEXED_p$LENWT_A) & is.na(CATDET_UNSEXED_p$LENWT_B) ,]
  
  CATDET <- rbind(CATDET_SEXED, CATDET_UNSEXED, remaining_U)
  
  # CLEAN
  rm(list=c("CATDET_SEXED", "CATDET_SEXED_p", "CATDET_UNSEXED", "CATDET_UNSEXED_p", "remaining_U", "CATDET_p"))
  # where possible, if FWT is blank, calculate it
  CATDET$FWT<-ifelse(is.na(CATDET$FWT) | CATDET$FWT == 0,
                     CATDET$LENWT_A*(CATDET$FLEN^CATDET$LENWT_B),
                     CATDET$FWT)
  CATDET$FWT <- CATDET$FWT/1000
  message("GSDET and GSCAT data now merged.  All weights converted to kgs")
  # add VESSEL, and use to determine "FROM_VESSEL"
  CATDET <- merge(CATDET, GSMISSIONS[,c("MISSION", "VESEL", "YEAR")],all.x = T, by=c("MISSION"))
  # iUsing vessel, figure out the category of FROM_VESSEL.
  # franken$FROM_VESSEL <- NA 
  CATDET <- CATDET |>
    mutate(FROM_VESSEL = case_when(
      VESEL == "A" ~ "ATC_HAM",
      VESEL == "H" ~ "ATC_HAM",
      VESEL == "J" ~ "NONE",     #<<CARTIER>>
      VESEL == "B" ~ "NONE",
      VESEL == "N" ~ "NED_TEM",
      VESEL == "S" ~ "TEL_VEN",
      VESEL == "T" ~ "NED_TEM",
      VESEL == "V" ~ "TEL_VEN",  #not in data, but putting here anyways
      TRUE ~ NA_character_
    ))
  
  #First lets deal with all the to cartier/cabot conversions we need to account for
  TELVEN_TO_CARCAB_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(TELVEN_TO_CARCAB_ABUND = CF_VALUE)
  
  
  NEDTEM_TO_CARCAB_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(NEDTEM_TO_CARCAB_ABUND = CF_VALUE) |>
    mutate(FROM_VESSEL="NED_TEM")
  
  ATCHAM_TO_CARCAB_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(ATCHAM_TO_CARCAB_ABUND = CF_VALUE) |>
    mutate(FROM_VESSEL="ATC_HAM")
  
  TELVEN_TO_CARCAB_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(TELVEN_TO_CARCAB_BMASS = CF_VALUE)
  
  
  NEDTEM_TO_CARCAB_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(NEDTEM_TO_CARCAB_BMASS = CF_VALUE) |>
    mutate(FROM_VESSEL="NED_TEM")
  
  ATCHAM_TO_CARCAB_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(ATCHAM_TO_CARCAB_BMASS = CF_VALUE) |>
    mutate(FROM_VESSEL="ATC_HAM")
  
  #Now for the NEDTEM to TELVEN
  
  NEDTEM_TO_TELVEN_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(NEDTEM_TO_TELVEN_ABUND = CF_VALUE)
  
  ATCHAM_TO_TELVEN_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(ATCHAM_TO_TELVEN_ABUND = CF_VALUE) |>
    mutate(FROM_VESSEL="ATC_HAM")
  
  NEDTEM_TO_TELVEN_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(NEDTEM_TO_TELVEN_BMASS = CF_VALUE)
  
  ATCHAM_TO_TELVEN_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    rename(ATCHAM_TO_TELVEN_BMASS = CF_VALUE) |>
    mutate(FROM_VESSEL="ATC_HAM")
  
  
  
  
  
  ######################################################################################################################################
  #These are just the conversion factors for the few species for ATC/HAM to NED/TEM
  message("MMM 20251104 - in the code below, the first df is immediately overwritten by the 2nd, and neither is ever used?")
  ATCHAM_TO_NEDTEM_ABUND_CONV<-data.frame(SPEC=c(11,40,41,42,43), ATCHAM_TO_NEDTEM_ABUND_LAM=c(0.83333,1.25,1.25,1.25,1.25),FROM_VESSEL = c("ATC_HAM","ATC_HAM","ATC_HAM","ATC_HAM","ATC_HAM") ) #FIXED THIS SO ATC AND HAM WOULD BE IN LINE WITH OTHER DATA FRAMES
  
  
  #I didn't make any more changes to the script after this
  ######################################################################################################################################
  
  
  
  #This is my alternative way to break up stuff
  
  
  #First lets do all LDM models
  TELVEN_TO_CARCAB_ABUND_CONV_LDM<-subset(TELVEN_TO_CARCAB_ABUND_CONV, !is.na(FLEN))
  NEDTEM_TO_CARCAB_ABUND_CONV_LDM<-subset(NEDTEM_TO_CARCAB_ABUND_CONV, !is.na(FLEN))
  ATCHAM_TO_CARCAB_ABUND_CONV_LDM<-subset(ATCHAM_TO_CARCAB_ABUND_CONV, !is.na(FLEN))
  NEDTEM_TO_TELVEN_ABUND_CONV_LDM<-subset(NEDTEM_TO_TELVEN_ABUND_CONV, !is.na(FLEN))
  ATCHAM_TO_TELVEN_ABUND_CONV_LDM<-subset(ATCHAM_TO_TELVEN_ABUND_CONV, !is.na(FLEN))
  
  
  
  
  LF_Data_All<-merge(CATDET,TELVEN_TO_CARCAB_ABUND_CONV_LDM,by=c("SPEC","FLEN","FROM_VESSEL"),all.x=TRUE)
  
  LF_Data_All<-merge(LF_Data_All,NEDTEM_TO_CARCAB_ABUND_CONV_LDM,by=c("SPEC","FLEN","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,NEDTEM_TO_TELVEN_ABUND_CONV_LDM,by=c("SPEC","FLEN","FROM_VESSEL"),all.x=TRUE)
  
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_CARCAB_ABUND_CONV_LDM,by=c("SPEC","FLEN","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_TELVEN_ABUND_CONV_LDM,by=c("SPEC","FLEN","FROM_VESSEL"),all.x=TRUE)
  
  
  
  #Second all LAM abundance models
  TELVEN_TO_CARCAB_ABUND_CONV_LAM<-subset(TELVEN_TO_CARCAB_ABUND_CONV, is.na(FLEN))
  TELVEN_TO_CARCAB_ABUND_CONV_LAM<- TELVEN_TO_CARCAB_ABUND_CONV_LAM |> 
    select(c(-FLEN)) |>
    rename("TELVEN_TO_CARCAB_ABUND_LAM"="TELVEN_TO_CARCAB_ABUND")
  
  NEDTEM_TO_CARCAB_ABUND_CONV_LAM<-subset(NEDTEM_TO_CARCAB_ABUND_CONV, is.na(FLEN))
  NEDTEM_TO_CARCAB_ABUND_CONV_LAM<- NEDTEM_TO_CARCAB_ABUND_CONV_LAM |> 
    select(c(-FLEN)) |>
    rename("NEDTEM_TO_CARCAB_ABUND_LAM"="NEDTEM_TO_CARCAB_ABUND")
  
  ATCHAM_TO_CARCAB_ABUND_CONV_LAM<-subset(ATCHAM_TO_CARCAB_ABUND_CONV, is.na(FLEN))
  ATCHAM_TO_CARCAB_ABUND_CONV_LAM<- ATCHAM_TO_CARCAB_ABUND_CONV_LAM |> 
    select(c(-FLEN)) |>
    rename("ATCHAM_TO_CARCAB_ABUND_LAM"="ATCHAM_TO_CARCAB_ABUND")
  
  NEDTEM_TO_TELVEN_ABUND_CONV_LAM<-subset(NEDTEM_TO_TELVEN_ABUND_CONV, is.na(FLEN))
  NEDTEM_TO_TELVEN_ABUND_CONV_LAM<- NEDTEM_TO_TELVEN_ABUND_CONV_LAM |> 
    select(c(-FLEN)) |>
    rename("NEDTEM_TO_TELVEN_ABUND_LAM"="NEDTEM_TO_TELVEN_ABUND")
  
  ATCHAM_TO_TELVEN_ABUND_CONV_LAM<-subset(ATCHAM_TO_TELVEN_ABUND_CONV, is.na(FLEN))
  ATCHAM_TO_TELVEN_ABUND_CONV_LAM<- ATCHAM_TO_TELVEN_ABUND_CONV_LAM |> 
    select(c(-FLEN)) |>
    rename("ATCHAM_TO_TELVEN_ABUND_LAM"="ATCHAM_TO_TELVEN_ABUND")
  
  
  LF_Data_All<-merge(LF_Data_All,TELVEN_TO_CARCAB_ABUND_CONV_LAM,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  
  LF_Data_All<-merge(LF_Data_All,NEDTEM_TO_CARCAB_ABUND_CONV_LAM,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,NEDTEM_TO_TELVEN_ABUND_CONV_LAM,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_CARCAB_ABUND_CONV_LAM,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_TELVEN_ABUND_CONV_LAM,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_NEDTEM_ABUND_CONV,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  
  
  #Third all biomass
  
  TELVEN_TO_CARCAB_BMASS_CONV<- TELVEN_TO_CARCAB_BMASS_CONV |> 
    select(c(-FLEN)) 
  
  NEDTEM_TO_CARCAB_BMASS_CONV<- NEDTEM_TO_CARCAB_BMASS_CONV |> 
    select(c(-FLEN)) 
  
  ATCHAM_TO_CARCAB_BMASS_CONV<- ATCHAM_TO_CARCAB_BMASS_CONV |> 
    select(c(-FLEN)) 
  
  
  NEDTEM_TO_TELVEN_BMASS_CONV<- NEDTEM_TO_TELVEN_BMASS_CONV |> 
    select(c(-FLEN)) 
  
  
  ATCHAM_TO_TELVEN_BMASS_CONV<- ATCHAM_TO_TELVEN_BMASS_CONV |> 
    select(c(-FLEN)) 
  
  LF_Data_All<-merge(LF_Data_All,TELVEN_TO_CARCAB_BMASS_CONV,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  
  LF_Data_All<-merge(LF_Data_All,NEDTEM_TO_CARCAB_BMASS_CONV,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,NEDTEM_TO_TELVEN_BMASS_CONV,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_CARCAB_BMASS_CONV,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,ATCHAM_TO_TELVEN_BMASS_CONV,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  
  
  ###This gets tricky here. TOTNO is easy, it gets calculated just by CLEN times the conversion factors, either LDM or LAM conversion factors for abundance, all are in there and gets times across columns easy peazy. 
  
  # TOTWGT is a nightmare though, we have multiple scenerios in how it gets calculated:
  # Scenerio 1: LDM abundance models only = Times the Converted TOTNO by TOTWGT_RAW
  # Scenerio 2: LAM abundance models only = Times Converted TOTNO by TOTWGT_RAW
  # Scenerio 3: LAM Biomass models only = Times TOTWGT_RAW by the Conversion factor columns
  # Scenerio 4: LAM abundance and LAM Biomass models = Times TOTWGT_RAW by the Conversion factor columns
  
  #So the question is prioritizing certain actions first and I think we might need to bug Yihao about this a bit. we have certain species that are especially problematic like SPEC 610. It has abundance and biomass conversions for both the NED_TEM and TEL_VEN but it has an abundance conversion for TEL_VEN and a biomass conversion for NED_TEM. So we use the abundance conversion for all vessels and then would use As and Bs to calculate biomass, while the biomass conversion would just be used on NED_TEM and ATC_HAM vessels, but since we have an abundance conversion already being applied to those, we might not really be using the biomass conversion. Nightmare!
  
  #Right now I will set it up as follows:
  # Step 1: anything without an FLEN value and has a biomass conversion gets TOTWGT calculated as TOTWGT_RAW times the conversion factors
  # Step 2: If there is an abundance conversion factor for TEL_VEN to CAR_CAB and the data did not derive from GSCAT, then TOTWGT is calculated as TOTNO times TOTWGT_RAW
  # Step 3: If there is an abundance conversion factor for NED_TEM to TEL_VEN and the data did not derive from GSCAT, then TOTWGT is calculated as TOTNO times TOTWGT_RAW for NED_TEM and ATC_HAM
  # Step 4: 
  
  SpeciesB<-subset(GSCONVERSIONS,CF_METRIC=="BIOMASS" & !CF_VALUE==1 & FROM_VESSEL=="NED_TEM")
  Species_List_BMASS_NEDTEM<-unique(SpeciesB$SPEC)
  
  SpeciesB<-subset(GSCONVERSIONS,CF_METRIC=="BIOMASS" & !CF_VALUE==1 & FROM_VESSEL=="TEL_VEN")
  Species_List_BMASS_TELVEN<-unique(SpeciesB$SPEC)
  
  
  #Now we have to account for abundance ones
  SpeciesA<-subset(GSCONVERSIONS,CF_METRIC=="ABUNDANCE" & !CF_VALUE==1 & FROM_VESSEL=="NED_TEM")
  Species_List_ABUND_NEDTEM<-unique(SpeciesA$SPEC)
  
  SpeciesA<-subset(GSCONVERSIONS,CF_METRIC=="ABUNDANCE" & !CF_VALUE==1 & FROM_VESSEL=="TEL_VEN")
  Species_List_ABUND_TELVEN<-unique(SpeciesA$SPEC)
  
  
  
  
  
  LF_Data_All <- LF_Data_All |>
    mutate(across(c("FWT", "CLEN"), ~ ifelse(is.na(.), 0, .)),
           across(c(TELVEN_TO_CARCAB_ABUND, 
                    NEDTEM_TO_CARCAB_ABUND,
                    NEDTEM_TO_TELVEN_ABUND,
                    ATCHAM_TO_CARCAB_ABUND,
                    ATCHAM_TO_TELVEN_ABUND,
                    TELVEN_TO_CARCAB_ABUND_LAM,
                    NEDTEM_TO_CARCAB_ABUND_LAM,
                    NEDTEM_TO_TELVEN_ABUND_LAM,
                    ATCHAM_TO_CARCAB_ABUND_LAM,
                    ATCHAM_TO_TELVEN_ABUND_LAM,
                    ATCHAM_TO_NEDTEM_ABUND_LAM,
                    TELVEN_TO_CARCAB_BMASS,
                    NEDTEM_TO_CARCAB_BMASS,
                    NEDTEM_TO_TELVEN_BMASS,
                    ATCHAM_TO_CARCAB_BMASS,
                    ATCHAM_TO_TELVEN_BMASS    ), ~ ifelse(is.na(.), 1, .)),
           TOTNO_RAW = CLEN,
           TOTWGT_RAW = FWT,
           CF_USED = NA,
           TOTNO = TOTNO_RAW/ TELVEN_TO_CARCAB_ABUND/NEDTEM_TO_CARCAB_ABUND/NEDTEM_TO_TELVEN_ABUND/ATCHAM_TO_CARCAB_ABUND/ATCHAM_TO_TELVEN_ABUND/TELVEN_TO_CARCAB_ABUND_LAM/NEDTEM_TO_CARCAB_ABUND_LAM/NEDTEM_TO_TELVEN_ABUND_LAM/ATCHAM_TO_CARCAB_ABUND_LAM/ATCHAM_TO_TELVEN_ABUND_LAM/ATCHAM_TO_NEDTEM_ABUND_LAM
           ,
           TOTWGT = ifelse(is.na(FLEN) & SPEC %in% c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM),TOTWGT_RAW/TELVEN_TO_CARCAB_BMASS/NEDTEM_TO_CARCAB_BMASS/NEDTEM_TO_TELVEN_BMASS/ATCHAM_TO_CARCAB_BMASS/ATCHAM_TO_TELVEN_BMASS,
                           ifelse(SPEC %in% Species_List_ABUND_TELVEN & !SRC %in% c("GSCAT") & FROM_VESSEL %in% c("TEL_VEN","NED_TEM","ATC_HAM"),TOTNO*TOTWGT_RAW,
                                  ifelse(SPEC %in% Species_List_ABUND_NEDTEM & !SRC %in% c("GSCAT") & FROM_VESSEL %in% c("NED_TEM","ATC_HAM"),TOTNO*TOTWGT_RAW,
                                         ifelse(SPEC %in% Species_List_BMASS_TELVEN & !SPEC %in% Species_List_ABUND_TELVEN & FROM_VESSEL %in% c("TEL_VEN"),TOTWGT_RAW/TELVEN_TO_CARCAB_BMASS/NEDTEM_TO_CARCAB_BMASS/NEDTEM_TO_TELVEN_BMASS/ATCHAM_TO_CARCAB_BMASS/ATCHAM_TO_TELVEN_BMASS,
                                                ifelse(SPEC %in% c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM) & !SPEC %in% c(Species_List_ABUND_TELVEN, Species_List_ABUND_NEDTEM) & FROM_VESSEL %in% c("NED_TEM","ATC_HAM"),TOTWGT_RAW/TELVEN_TO_CARCAB_BMASS/NEDTEM_TO_CARCAB_BMASS/NEDTEM_TO_TELVEN_BMASS/ATCHAM_TO_CARCAB_BMASS/ATCHAM_TO_TELVEN_BMASS,TOTWGT_RAW))))),
           
           CONV_USED= ifelse(FROM_VESSEL %in% c("NONE"),"NONE",
                             ifelse(is.na(FLEN) & SPEC %in% c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM),"BIOMASS",
                                    ifelse(SPEC %in% Species_List_ABUND_TELVEN & !SRC %in% c("GSCAT") & FROM_VESSEL %in% c("TEL_VEN","NED_TEM","ATC_HAM"),"ABUND",
                                           ifelse(SPEC %in% Species_List_ABUND_NEDTEM & !SRC %in% c("GSCAT") & FROM_VESSEL %in% c("NED_TEM","ATC_HAM"),"ABUND",
                                                  ifelse(SPEC %in% Species_List_BMASS_TELVEN & !SPEC %in% Species_List_ABUND_TELVEN & FROM_VESSEL %in% c("TEL_VEN"),"BIOMASS",
                                                         ifelse(SPEC %in% c(Species_List_BMASS_TELVEN,Species_List_BMASS_NEDTEM) & !SPEC %in% c(Species_List_ABUND_TELVEN, Species_List_ABUND_NEDTEM) & FROM_VESSEL %in% c("NED_TEM","ATC_HAM"),"BIOMASS","NONE"))))))
           
    )
  
  
  #These are just manual checks, will delete later or you can delete now as I'll have this stuff in another file.
  #LF_Data_All$WGT_Diff<-LF_Data_All$TOTWGT_RAW-LF_Data_All$TOTWGT
  #LF_Data_All$NUM_Diff<-LF_Data_All$TOTNO_RAW-LF_Data_All$TOTNO 
  #LF_Data_All$WGT_CHANGE<-ifelse(LF_Data_All$WGT_Diff==0,"N","Y")
  #LF_Data_All$NUM_CHANGE<-ifelse(LF_Data_All$NUM_Diff==0,"N","Y")
  #LF_Data_All$NEDTEM_BIOM<-ifelse(LF_Data_All$SPEC %in% Species_List_BMASS_NEDTEM,"Y","N")
  #LF_Data_All$TELVEN_BIOM<-ifelse(LF_Data_All$SPEC %in% Species_List_BMASS_TELVEN,"Y","N")
  #LF_Data_All$NEDTEM_ABUND<-ifelse(LF_Data_All$SPEC %in% Species_List_ABUND_NEDTEM,"Y","N")
  #LF_Data_All$TELVEN_ABUND<-ifelse(LF_Data_All$SPEC %in% Species_List_ABUND_TELVEN,"Y","N")
  #TesterA<- GSCONVERSIONS |> select(SPEC,FROM_VESSEL,CF_MODEL_TYPE)
  #TesterA<- TesterA[!duplicated(TesterA),]
  #TesterA<-merge(LF_Data_All,TesterA,by=c("SPEC","FROM_VESSEL"),all.x=TRUE)
  #openxlsx::write.xlsx(TesterA,"Data_Checks_And_Flags.xlsx")
  
  
  message("How conversion factors are applied and how biomass is calculated can affect results. This script applies conversion factors and calculates biomass in the following way:
          1: All length disaggregated/aggregated abundance conversion factors are applied to count at lengths or total number caught at the set level.
          2: If count at length data is NOT available, Biomass conversions are used and biomass is calculated from total weight at the set level.
          3: If count at length data and abundance conversion factors not equal to 1 are available, biomass is calculated by the sum of the product of count at lengths and fish weights (either by measured weight or derived from length to weight relationships when no weights were taken).")
  
  LF_Data_All <- LF_Data_All |> 
    mutate(
      # Intermediate variables for conditions
      neither_condition =   (TELVEN_TO_CARCAB_ABUND * NEDTEM_TO_CARCAB_ABUND * NEDTEM_TO_TELVEN_ABUND * ATCHAM_TO_CARCAB_ABUND * ATCHAM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND_LAM * NEDTEM_TO_CARCAB_ABUND_LAM * NEDTEM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_CARCAB_ABUND_LAM * ATCHAM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_NEDTEM_ABUND_LAM == 1) & (TELVEN_TO_CARCAB_BMASS * NEDTEM_TO_CARCAB_BMASS * NEDTEM_TO_TELVEN_BMASS * ATCHAM_TO_CARCAB_BMASS * ATCHAM_TO_TELVEN_BMASS == 1),
      
      either_condition =    (TELVEN_TO_CARCAB_ABUND * NEDTEM_TO_CARCAB_ABUND * NEDTEM_TO_TELVEN_ABUND * ATCHAM_TO_CARCAB_ABUND * ATCHAM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND_LAM * NEDTEM_TO_CARCAB_ABUND_LAM * NEDTEM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_CARCAB_ABUND_LAM * ATCHAM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_NEDTEM_ABUND_LAM != 1) & (TELVEN_TO_CARCAB_BMASS * NEDTEM_TO_CARCAB_BMASS * NEDTEM_TO_TELVEN_BMASS * ATCHAM_TO_CARCAB_BMASS * ATCHAM_TO_TELVEN_BMASS != 1),
      
      abundance_condition = (TELVEN_TO_CARCAB_ABUND * NEDTEM_TO_CARCAB_ABUND * NEDTEM_TO_TELVEN_ABUND * ATCHAM_TO_CARCAB_ABUND * ATCHAM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND_LAM * NEDTEM_TO_CARCAB_ABUND_LAM * NEDTEM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_CARCAB_ABUND_LAM * ATCHAM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_NEDTEM_ABUND_LAM != 1) & (TELVEN_TO_CARCAB_BMASS * NEDTEM_TO_CARCAB_BMASS * NEDTEM_TO_TELVEN_BMASS * ATCHAM_TO_CARCAB_BMASS * ATCHAM_TO_TELVEN_BMASS == 1),
      
      biomass_condition =   (TELVEN_TO_CARCAB_ABUND * NEDTEM_TO_CARCAB_ABUND * NEDTEM_TO_TELVEN_ABUND * ATCHAM_TO_CARCAB_ABUND * ATCHAM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND_LAM * NEDTEM_TO_CARCAB_ABUND_LAM * NEDTEM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_CARCAB_ABUND_LAM * ATCHAM_TO_TELVEN_ABUND_LAM * ATCHAM_TO_NEDTEM_ABUND_LAM == 1) & (TELVEN_TO_CARCAB_BMASS * NEDTEM_TO_CARCAB_BMASS * NEDTEM_TO_TELVEN_BMASS * ATCHAM_TO_CARCAB_BMASS * ATCHAM_TO_TELVEN_BMASS != 1),
      
      # CF_USED assignment
      CF_USED = case_when(
        neither_condition & CLEN == 0 & FWT != 0 ~ "BIOMASS",
        neither_condition & CLEN != 0 & FWT == 0 ~ "ABUNDANCE",
        neither_condition ~ "NEITHER",
        
        either_condition & FWT != 0 & CLEN == 0 ~ "BIOMASS",
        either_condition & FWT == 0 & CLEN != 0 ~ "ABUNDANCE",
        either_condition ~ "EITHER",
        
        abundance_condition & CLEN != 0 ~ "ABUNDANCE",
        abundance_condition & CLEN == 0 ~ "ABUNDANCE_PROBLEM",
        
        biomass_condition & FWT != 0 ~ "BIOMASS",
        biomass_condition & FWT == 0 ~ "BIOMASS_PROBLEM",
        
        TRUE ~ CF_USED
      )
    ) |> select(SPEC, FLEN, FROM_VESSEL, MISSION, FSEX, SETNO, SRC, VESEL, YEAR, TOTNO_RAW, TOTWGT_RAW, TOTNO, TOTWGT, CF_USED) # AGMAT, AGE FMAT
  
  #NEDTEM_TO_TELVEN_ABUND, TELVEN_TO_CARCAB_ABUND, NEDTEM_TO_TELVEN_BMASS, TELVEN_TO_CARCAB_BMASS, LENWT_A, LENWT_B, 
  
  message("Note that for records where 'ABUNDANCE_PROBLEM'or 'BIOMASS_PROBLEM', that the preferred CF will result in 0, while the other has a non-zero value." )
  
  #while we have all the GS tables loaded, let's add fields we'll want
  LF_Data_All <- merge(LF_Data_All, tblList$GSINF[,c("MISSION", "SETNO","STRAT", "AREA", "DIST", "GEAR","SLONG", "SLAT", "ELONG", "ELAT")],all = T, by=c("MISSION", "SETNO")) 
  
  GSSTRATUM <- tblList$GSSTRATUM |>
    select(STRAT, AREA) |> #CHANGED TO ARE_KM2
    mutate(AREA_KM2 = AREA * 3.42990) |>
    select(-AREA)
  
  LF_Data_All <- merge(LF_Data_All, GSSTRATUM,all.x = T, by="STRAT") 
  
  LF_Data_All<-LF_Data_All[, c( "MISSION", "SETNO", "STRAT", "AREA_KM2", "VESEL", "FROM_VESSEL", "DIST", "GEAR","SLONG", "SLAT", "ELONG", "ELAT", 
                                "SRC", "SPEC", "FLEN" , "FSEX", "CF_USED", "TOTNO_RAW", "TOTWGT_RAW", "TOTNO", "TOTWGT" )]  #"AGMAT", "AGE", "FMAT", "YEAR",
  return(LF_Data_All)
}
