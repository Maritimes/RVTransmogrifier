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
  Mar.utils::get_data_tables('groundfish', cxn=cxn, data.dir = get_pesd_rvt_dir(), 
                             tables=c("GSCONVERSIONS", "GSSPEC2"),force.extract = F)
  
  # pull the As and Bs for the selected season (summer or winter).  
  # If multiple exist (for the same SPEC/SEX) choose the higher R2 value
  ABs <- GSSPEC2 |>
    filter(SEASON == ifelse(season=="SUMMER", "SUMMER", "WINTER")) |> 
    group_by(SPEC, FSEX) |>
    filter(R2 == max(R2)) |>
    ungroup() |>
    rename(FSEX_key = FSEX)
  
  # retain data for SPECs that have conversion factors based on FLENs for the TEL/VEN to CAR/CAB and the NED/TEM to TEL/VEN
  GSCAT_SPEC_LIST_TELVEN<-subset(GSCONVERSIONS,!CF_VALUE==1 & CF_METRIC=="ABUNDANCE" & FROM_VESSEL=="TEL_VEN")
  GSCAT_SPEC_LIST_NEDTEM<-subset(GSCONVERSIONS,!CF_VALUE==1 & CF_METRIC=="ABUNDANCE" & FROM_VESSEL=="NED_TEM")
  
  #########################################################################
  ########   Applying Sample ratios to CLENs ##############################
  #########################################################################
  # Adjust for sample_ratio from gscat
  # Sample weight may also be 0 if it was the same as total weight
  # if total weight is 0. this sets it to one
  
  GSCAT_mrg <- tblList$GSCAT |>
    dplyr::mutate(
      SAMPWGT = dplyr::if_else(SAMPWGT == 0 | is.na(SAMPWGT), TOTWGT, SAMPWGT),
      SAMPTOT_Ratio = dplyr::if_else(is.nan(TOTWGT / SAMPWGT), 1, TOTWGT / SAMPWGT)
    )
  
  #########################################################################
  ########   Handle GSDET ##############################
  #########################################################################
  # ensure that berried females count as females
  # ensure that a value for sex exists 
  # Remove records that will use data from GSCAT
  # filter out any species that don't need conversions done by FLEN (i.e. GSCAT_SPEC_LIST_*)
  # If a species requires a conversion factor for the TEL/VEN to CAR/CAB, those species have to have those conversion 
  # factors applied to all of the historical vessels used so that's why the NED, TEM, ATC and HAM all get the same treatment here.
  GSDET_ratioed <- tblList$GSDET |>
    dplyr::mutate(
      FSEX = dplyr::case_when(
        FSEX == 3 ~ 2,
        FSEX %in% c(1, 2) ~ FSEX,
        .default = 0
      ),
      SRC = "GSDET"
    ) |>
    dplyr::filter(
      (grepl("TEL|VEN|NED|TEM|ATC|HAM", MISSION) & SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC) |
        (grepl("NED|TEM|ATC|HAM", MISSION) & SPEC %in% GSCAT_SPEC_LIST_NEDTEM$SPEC)
    ) |>
    dplyr::left_join(
      GSCAT_mrg |> dplyr::select(MISSION, SETNO, SPEC, SIZE_CLASS, SAMPTOT_Ratio),
      by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")
    ) |>
    dplyr::mutate(CLEN = CLEN * SAMPTOT_Ratio) |>
    dplyr::select(-SAMPTOT_Ratio)
  
  # find records in GSCAT that don't have associated data in GSDET
  GSCAT_mrg <- GSCAT_mrg |>
    dplyr::anti_join(GSDET_ratioed, by = c("MISSION", "SETNO", "SPEC")) |>
    dplyr::select(MISSION, SETNO, SPEC, TOTWGT, TOTNO) |>
    dplyr::rename(FWT = TOTWGT, CLEN = TOTNO) |>
    dplyr::mutate(
      FWT = FWT * 1000,# Convert FWT from kg to g
      SRC = "GSCAT",   # Note source for unanticipated debugging
      FLEN = NA,       # GSCAT does not have values for FLEN
      FSEX = 0         #, GSCAT does not have values for FSEX 
      #     AGMAT = NA        # GSCAT does not have values for age material
      #     AGE = NA,          # GSCAT does not have ages
      #     FMAT = 0           # GSCAT does not have values for maturity
    )
  
  message("Some recs had weight = 9999 (e.g. snowcrab, scallops). Placeholder?" )
  
  # CATDET - merge the AB info with GSDET/GSCAT
  # - first, attempt to merge on sex
  # - any records that couldn't be merged on SPEC and FSEX should be merged on SPEC alone
  # - any records that could not be merged on SPEC are retained
  # where possible, if FWT is blank, calculate it, convert to kgs
  # add FROM_VESSEL
  CATDET_base <- dplyr::bind_rows(
    GSCAT_mrg,
    GSDET_ratioed |> dplyr::select(MISSION, SETNO, SPEC, FSEX, FWT, FLEN, CLEN, SRC)
  ) |>
    dplyr::filter(
      SPEC < 9500,              # remove these records that aren't actually species
      !SPEC %in% c(1091:1095),  # these are unidentified species - not helpful
      SPEC != 9400              # this is literal trash
    ) |>
    dplyr::left_join(
      ABs |> dplyr::select(SPEC, FSEX_key, LENWT_A, LENWT_B),
      by = c("SPEC" = "SPEC", "FSEX" = "FSEX_key")
    )
  
  CATDET <- dplyr::bind_rows(
    CATDET_base |> dplyr::filter(!is.na(LENWT_A), !is.na(LENWT_B)),
    CATDET_base |> 
      dplyr::filter(is.na(LENWT_A), is.na(LENWT_B)) |>
      dplyr::select(dplyr::any_of(names(GSDET_ratioed))) |>
      dplyr::left_join(
        ABs |> dplyr::filter(FSEX_key == 0) |> dplyr::select(SPEC, LENWT_A, LENWT_B) |> dplyr::distinct(),
        by = "SPEC"
      )
  ) |>
    dplyr::mutate(
      FWT = dplyr::if_else(is.na(FWT) | FWT == 0, LENWT_A * (FLEN ^ LENWT_B), FWT),
      FWT = FWT / 1000
    ) |>
    dplyr::left_join(
      tblList$GSMISSIONS |> dplyr::select(MISSION, VESEL, YEAR),
      by = "MISSION"
    ) |>
    dplyr::mutate(
      FROM_VESSEL = dplyr::case_when(
        VESEL == "A" ~ "ATC_HAM",
        VESEL == "H" ~ "ATC_HAM",
        VESEL == "J" ~ "NONE",    # <<CARTIER>>
        VESEL == "B" ~ "NONE",
        VESEL == "N" ~ "NED_TEM",
        VESEL == "S" ~ "TEL_VEN",
        VESEL == "T" ~ "NED_TEM",
        VESEL == "V" ~ "TEL_VEN", # not in data, but putting here anyways
        .default = NA_character_
      )
    )
  rm(list=c("CATDET_base"))
  
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
  
  # These are just the conversion factors for the few species for ATC/HAM to NED/TEM
  ATCHAM_TO_NEDTEM_ABUND_CONV<-data.frame(SPEC=c(11,40,41,42,43), 
                                          ATCHAM_TO_NEDTEM_ABUND_LAM=c(0.83333,1.25,1.25,1.25,1.25),
                                          FROM_VESSEL = c("ATC_HAM","ATC_HAM","ATC_HAM","ATC_HAM","ATC_HAM") ) 
  #FIXED THIS SO ATC AND HAM WOULD BE IN LINE WITH OTHER DATA FRAMES
  
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
  
  
  # This gets tricky here. TOTNO is easy, it gets calculated just by CLEN times the conversion factors, either LDM or 
  # LAM conversion factors for abundance, all are in there and gets times across columns easy peazy. 
  
  # TOTWGT is a nightmare though, we have multiple scenerios in how it gets calculated:
  # Scenerio 1: LDM abundance models only = Times the Converted TOTNO by TOTWGT_RAW
  # Scenerio 2: LAM abundance models only = Times Converted TOTNO by TOTWGT_RAW
  # Scenerio 3: LAM Biomass models only = Times TOTWGT_RAW by the Conversion factor columns
  # Scenerio 4: LAM abundance and LAM Biomass models = Times TOTWGT_RAW by the Conversion factor columns
  
  # So the question is prioritizing certain actions first and I think we might need to bug Yihao about this a bit. we 
  # have certain species that are especially problematic like SPEC 610. It has abundance and biomass conversions for 
  # both the NED_TEM and TEL_VEN but it has an abundance conversion for TEL_VEN and a biomass conversion for NED_TEM. 
  # So we use the abundance conversion for all vessels and then would use As and Bs to calculate biomass, while the 
  # biomass conversion would just be used on NED_TEM and ATC_HAM vessels, but since we have an abundance conversion 
  # already being applied to those, we might not really be using the biomass conversion. Nightmare!
  
  #Right now I will set it up as follows:
  # Step 1: anything without an FLEN value and has a biomass conversion gets TOTWGT calculated as TOTWGT_RAW times the 
  #         conversion factors
  # Step 2: If there is an abundance conversion factor for TEL_VEN to CAR_CAB and the data did not derive from GSCAT, 
  #         then TOTWGT is calculated as TOTNO times TOTWGT_RAW
  # Step 3: If there is an abundance conversion factor for NED_TEM to TEL_VEN and the data did not derive from GSCAT, 
  #         then TOTWGT is calculated as TOTNO times TOTWGT_RAW for NED_TEM and ATC_HAM
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
           TOTNO = TOTNO_RAW/ TELVEN_TO_CARCAB_ABUND/NEDTEM_TO_CARCAB_ABUND/NEDTEM_TO_TELVEN_ABUND/ATCHAM_TO_CARCAB_ABUND/ATCHAM_TO_TELVEN_ABUND/TELVEN_TO_CARCAB_ABUND_LAM/NEDTEM_TO_CARCAB_ABUND_LAM/NEDTEM_TO_TELVEN_ABUND_LAM/ATCHAM_TO_CARCAB_ABUND_LAM/ATCHAM_TO_TELVEN_ABUND_LAM/ATCHAM_TO_NEDTEM_ABUND_LAM,
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
  
  
  message("How conversion factors are applied and how biomass is calculated can affect results. 
This script applies conversion factors and calculates biomass in the following way:
          1: All length disaggregated/aggregated abundance conversion factors are 
          \tapplied to count at lengths or total number caught at the set level.
          2: If count at length data is NOT available, Biomass conversions 
          \tare used and biomass is calculated from total weight at the set level.
          3: If count at length data and abundance conversion factors not 
          \tequal to 1 are available, biomass is calculated by the sum of the product 
          \tof count at lengths and fish weights (either by measured weight or derived 
          \tfrom length to weight relationships when no weights were taken).")
  
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
  
  message("Note that for records where 'ABUNDANCE_PROBLEM'or 'BIOMASS_PROBLEM', 
the preferred CF will result in 0, while the other has a non-zero value." )

    #while we have all the GS tables loaded, let's add fields we'll want
  LF_Data_All <- merge(LF_Data_All, tblList$GSINF[,c("MISSION", "SETNO","STRAT", "AREA", "DIST", "GEAR","SLONG_DD", "SLAT_DD", "ELONG_DD", "ELAT_DD", "AREA_KM2")],all = T, by=c("MISSION", "SETNO")) 

  LF_Data_All<-LF_Data_All[, c( "MISSION", "SETNO", "STRAT", "AREA_KM2", "VESEL", "FROM_VESSEL", "DIST", "GEAR","SLONG_DD", "SLAT_DD", "ELONG_DD", "ELAT_DD", 
                                "SRC", "SPEC", "FLEN" , "FSEX", "CF_USED", "TOTNO_RAW", "TOTWGT_RAW", "TOTNO", "TOTWGT" )]  #"AGMAT", "AGE", "FMAT", "YEAR",
  return(LF_Data_All)
}
