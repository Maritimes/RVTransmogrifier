#' @title applyConversionFactors
#' @description Apply vessel conversion factors to standardize catch data across different research vessels. Merges GSDET and GSCAT data, applies length-weight relationships, calculates sample ratios, and applies vessel-specific conversion factors for abundance and biomass to standardize historical data to current vessel (CAR/CAB) equivalents.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, GSDET, GSMISSIONS, and GSSTRATUM.
#' @return A data frame containing standardized catch data with vessel conversion factors applied, including fields for raw and converted abundance (TOTNO) and biomass (TOTWGT), along with metadata indicating which conversion factors were used.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function downloads additional reference tables (GSCONVERSIONS, GSSPEC2) if not already available. It handles seasonal differences (SPRING/SUMMER) and multiple vessel transitions (NED/TEM to TEL/VEN to CAR/CAB).
#' @export
applyConversionFactors <- function(tblList){

  season <- unique(tblList$GSMISSION$SEASON)
  if (length(season)>1)stop("The function can only handle a single season of data")
  if (!season %in% c('SPRING','SUMMER'))stop("This function can only handle SUMMER or SPRING/GEORGES") 
  
  # get a few extra tables we'll need
  Mar.utils::get_data_tables('groundfish', cxn=getCxn(), data.dir = get_pesd_rvt_dir(), 
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
  
  GSDET<-merge(GSDET,GSCAT_mrg[,c("MISSION", "SETNO", "SPEC","SAMPTOT_Ratio","SIZE_CLASS")],by=c("MISSION", "SETNO", "SPEC","SIZE_CLASS"))
  GSDET$CLEN<-GSDET$CLEN*GSDET$SAMPTOT_Ratio
  
  GSDET<- GSDET %>% select(-c(Remove,SAMPTOT_Ratio))
  
  
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
      VESEL == "A" ~ "NED_TEM",
      VESEL == "H" ~ "NED_TEM",
      VESEL == "J" ~ "NONE",     #<<CARTIER>>
      VESEL == "N" ~ "NED_TEM",
      VESEL == "S" ~ "TEL_VEN",
      VESEL == "T" ~ "NED_TEM",
      VESEL == "V" ~ "TEL_VEN",  #not in data, but putting here anyways
      TRUE ~ NA_character_
    ))
  
  NEDTEM_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE) |>
    rename(NEDTEM_TO_TELVEN_ABUND = CF_VALUE)
  
  NEDTEM_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE) |>
    rename(NEDTEM_TO_TELVEN_BMASS = CF_VALUE)
  
  TELVEN_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE) |>
    rename(TELVEN_TO_CARCAB_ABUND = CF_VALUE)
  
  TELVEN_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") &  FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE) |>
    rename(TELVEN_TO_CARCAB_BMASS = CF_VALUE)
  
  ######################################################################################################################################
  #These are just the conversion factors for the few species for ATC/HAM to NED/TEM
  message("MMM 20251104 - in the code below, the first df is immediately overwritten by the 2nd, and neither is ever used?")
  ATC_ABUND_CONV<-data.frame(SPEC=c(11,40,41,42,43),      ATC_To_NEDTEM=c(0.83333,1.25,1.25,1.25,1.25),VESEL=c("A","A","A","A","A"))
  
  ATC_ABUND_CONV<-data.frame(SPEC=c(11,40,41,42,43),CF_VALUE_HAM_To_NED=c(0.83333,1.25,1.25,1.25,1.25),VESEL=c("A","A","A","A","A"))
  
  #I didn't make any more changes to the script after this
  ######################################################################################################################################
  
  
  
  LF_Data_All<-merge(CATDET,NEDTEM_ABUND_CONV,by=c("SPEC","FLEN"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,TELVEN_ABUND_CONV,by=c("SPEC","FLEN"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,NEDTEM_BMASS_CONV,by=c("SPEC","FLEN"),all.x=TRUE)
  LF_Data_All<-merge(LF_Data_All,TELVEN_BMASS_CONV,by=c("SPEC","FLEN"),all.x=TRUE)
  LF_Data_All <- LF_Data_All %>%
    mutate(across(c("FWT", "CLEN"), ~ ifelse(is.na(.), 0, .)),
           across(c("NEDTEM_TO_TELVEN_ABUND", "TELVEN_TO_CARCAB_ABUND", "NEDTEM_TO_TELVEN_BMASS", "TELVEN_TO_CARCAB_BMASS"), ~ ifelse(is.na(.), 1, .)),
           TOTNO_RAW = CLEN,
           TOTWGT_RAW = FWT,
           CF_USED = NA,
           TOTNO = case_when(
             FROM_VESSEL == "TEL_VEN" ~ round(TOTNO_RAW / NEDTEM_TO_TELVEN_ABUND / TELVEN_TO_CARCAB_ABUND, 4),
             FROM_VESSEL == "NED_TEM" ~ round(TOTNO_RAW / TELVEN_TO_CARCAB_ABUND, 4),
             FROM_VESSEL == "NONE" ~ round(TOTNO_RAW, 4),
             TRUE ~ NA_real_  # Handle any other cases if necessary
           ),
           TOTWGT = case_when(
             FROM_VESSEL == "TEL_VEN" ~ round(TOTWGT_RAW / NEDTEM_TO_TELVEN_BMASS / TELVEN_TO_CARCAB_BMASS, 4),
             FROM_VESSEL == "NED_TEM" ~ round(TOTWGT_RAW / TELVEN_TO_CARCAB_BMASS, 4),
             FROM_VESSEL == "NONE" ~ round(TOTWGT_RAW, 4),
             TRUE ~ NA_real_  # Handle any other cases if necessary
           )
    )
  
  
  LF_Data_All <- LF_Data_All %>%
    mutate(
      # Intermediate variables for conditions
      neither_condition =   (NEDTEM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND == 1) & (NEDTEM_TO_TELVEN_BMASS * TELVEN_TO_CARCAB_BMASS == 1),
      either_condition =    (NEDTEM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND != 1) & (NEDTEM_TO_TELVEN_BMASS * TELVEN_TO_CARCAB_BMASS != 1),
      abundance_condition = (NEDTEM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND != 1) & (NEDTEM_TO_TELVEN_BMASS * TELVEN_TO_CARCAB_BMASS == 1),
      biomass_condition =   (NEDTEM_TO_TELVEN_ABUND * TELVEN_TO_CARCAB_ABUND == 1) & (NEDTEM_TO_TELVEN_BMASS * TELVEN_TO_CARCAB_BMASS != 1),
      
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
  LF_Data_All <- merge(LF_Data_All, tblList$GSINF[,c("MISSION", "SETNO","STRAT", "AREA", "DIST", "GEAR","SLONG_DD", "SLAT_DD", "ELONG_DD", "ELAT_DD")],all = T, by=c("MISSION", "SETNO")) 
  
  GSSTRATUM <- tblList$GSSTRATUM |>
    select(STRAT, AREA) |> #CHANGED TO ARE_KM2
    mutate(AREA_KM2 = AREA * 3.42990) |>
    select(-AREA)
  
  LF_Data_All <- merge(LF_Data_All, GSSTRATUM,all.x = T, by="STRAT") 
  
  LF_Data_All<-LF_Data_All[, c( "MISSION", "SETNO", "STRAT", "AREA_KM2", "VESEL", "FROM_VESSEL", "DIST", "GEAR","SLONG_DD", "SLAT_DD", "ELONG_DD", "ELAT_DD", 
                                "SRC", "SPEC", "FLEN" , "FSEX", "CF_USED", "TOTNO_RAW", "TOTWGT_RAW", "TOTNO", "TOTWGT" )]  #"AGMAT", "AGE", "FMAT", "YEAR",
  return(LF_Data_All)
}
