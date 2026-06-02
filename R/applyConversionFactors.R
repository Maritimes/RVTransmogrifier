#' @title applyConversionFactors
#' @description Apply vessel conversion factors to standardize catch data across different research vessels. Merges GSDET and GSCAT data, applies length-weight relationships, calculates sample ratios, and applies vessel-specific conversion factors for abundance and biomass to standardize historical data to current vessel (CAR/CAB) equivalents.
#' @param cxn A valid Oracle connection object. This parameter allows you to
#' pass an existing connection, reducing the need to establish a new connection
#' within the function. If provided, it takes precedence over the connection-
#' related parameters.
#' @param tblList the default is \code{NULL}. A list of RV dataframes including GSINF, GSCAT, GSDET, GSMISSIONS, and GSSTRATUM.
#' @return A data frame containing standardized catch data with vessel conversion factors applied, including fields for raw and converted abundance (TOTNO) and biomass (TOTWGT), along with metadata indicating which conversion factors were used.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note This function downloads additional reference tables (GSCONVERSIONS, GSSPEC2) if not already available. It 
#' handles seasonal differences (SPRING/SUMMER) and multiple vessel transitions (NED/TEM to TEL/VEN to CAR/CAB).
#' 
#' The differences in the record counts between GSCAT/GSDET and GSCAT_CONV/GSDET_CONV arise from several key 
#' transformations performed by the applyConversionFactors() function. For GSDET, records are filtered based on the 
#' availability of required fields, such as fish lengths (FLEN) and counts-at-length (CLEN), which are essential for 
#' applying species-specific or gear-specific conversions. Records may also be dropped if they represent species that 
#' do not require conversions or fall into excluded categories (e.g., unidentified species or invalid data points). 
#' Furthermore, GSDET_CONV integrates and applies length-weight relationships using "A" and "B" coefficients or other 
#' scaling factors, which may modify the final set of records. For GSCAT, the conversions involve recalculating weight 
#' and numbers data while grouping and aggregating records by fields such as MISSION, SETNO, and SPEC. This grouping 
#' process can consolidate multiple input rows into a single output row, reducing the overall record count. 
#' Additionally, the function filters out records attributed to invalid or unnecessary species, adds derived fields, 
#' and integrates data from other tables, all of which can influence the final number of output rows. These 
#' transformations collectively ensure the resulting datasets are both standardized and adjusted for comparability 
#' across different vessels and gears.
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr anti_join
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr distinct
#' @importFrom dplyr case_when
#' @importFrom Mar.utils get_data_tables
#' @export
applyConversionFactors <- function(cxn = NULL, tblList) {
  season <- unique(tblList$GSMISSIONS$SEASON)
  if (length(season) > 1) {
    stop("The function can only handle a single season of data")
  }
  if (!season %in% c('SPRING', 'SUMMER')) {
    stop("This function can only handle SUMMER or SPRING/GEORGES")
  }
  
  # get a few extra tables we'll need
  get_data_tables('groundfish', cxn = cxn, data.dir = get_pesd_rvt_dir(), tables = c("GSCONVERSIONS", "GSSPEC2","GSCONVERSIONS_RAW"), force.extract = F)
  
  # retain data for SPECs that have conversion factors based on FLENs for the TEL/VEN to CAR/CAB and the NED/TEM to TEL/VEN
  GSCAT_SPEC_LIST_TELVEN <- subset(GSCONVERSIONS, !CF_VALUE == 1 & CF_METRIC == "ABUNDANCE" & FROM_VESSEL == "TEL_VEN")
  GSCAT_SPEC_LIST_NEDTEM <- subset(GSCONVERSIONS, !CF_VALUE == 1 & CF_METRIC == "ABUNDANCE" & FROM_VESSEL == "NED_TEM")
  
  # pull the As and Bs for the selected season (summer or winter).
  # If multiple exist (for the same SPEC/SEX) choose the higher R2 value
  ABs <- GSSPEC2 |>
    filter(SEASON == ifelse(season == "SUMMER", "SUMMER", "WINTER")) |>
    group_by(SPEC, FSEX) |>
    filter(R2 == max(R2)) |>
    ungroup() |>
    rename(FSEX_key = FSEX)
  
  tblList$GSCAT <- tblList$GSCAT |> arrange(MISSION, SETNO, SPEC, SIZE_CLASS)
  tblList$GSDET <- tblList$GSDET |> arrange(MISSION, SETNO, SPEC, SIZE_CLASS, FLEN, FWT)
  
  ogGSCAT <- tblList$GSCAT |> 
              select(MISSION, SETNO, SPEC, SIZE_CLASS, TOTNO, TOTWGT, SAMPWGT) |>
              rename(TOTNO_OG = TOTNO,
                     TOTWGT_OG = TOTWGT,
                     SAMPWGT_OG = SAMPWGT)
              
  ogGSDET <- tblList$GSDET |> 
              select(MISSION, SETNO, SPEC, SIZE_CLASS, FLEN, SPECIMEN_ID, FWT, CLEN) |> 
              rename(FWT_OG = FWT,
                     CLEN_OG = CLEN)
  # GSCAT -------------------------------------------------------------------
  # retain copies of original GSCAT and apply sample ratio where approp 
  raw_GSCAT <- tblList$GSCAT |> 
    select(MISSION, SETNO, SPEC, SIZE_CLASS, SAMPWGT, TOTWGT, TOTNO) |> 
    left_join(
      tblList$GSMISSIONS |> select(MISSION, VESEL, YEAR),
      by = "MISSION"
    ) |> 
    mutate(
      FROM_VESSEL = case_when(
        VESEL == "A" ~ "ATC_HAM",
        VESEL == "H" ~ "ATC_HAM",
        VESEL == "J" ~ "CAR_CAB", # <<CARTIER>>
        VESEL == "B" ~ "CAR_CAB",
        VESEL == "N" ~ "NED_TEM",
        VESEL == "S" ~ "TEL_VEN",
        VESEL == "T" ~ "NED_TEM",
        VESEL == "V" ~ "TEL_VEN", # not in data, but putting here anyways
        .default = NA_character_
      ),
      SAMPWGT = 1000*SAMPWGT,
      TOTWGT = 1000*TOTWGT,
      SAMPWGT = if_else(SAMPWGT == 0 | is.na(SAMPWGT), TOTWGT, SAMPWGT),
      SAMPTOT_Ratio = if_else(is.nan(TOTWGT / SAMPWGT), 1, TOTWGT / SAMPWGT),
      SAMPTOT_Ratio = if_else(is.na(TOTWGT / SAMPWGT),  1, TOTWGT / SAMPWGT)
    ) 
  # separate recs into convertible and unconvertible -------------------------
  GSCAT_unconv  <- raw_GSCAT|> 
    filter(SPEC >= 9400 | SPEC %in% c(1091:1092))|> 
    mutate(SRC = "GSCAT: unconverted") |> 
    select(-SAMPWGT,-SAMPTOT_Ratio , -VESEL, -YEAR)
  
  GSCAT_forconv_pre <- raw_GSCAT |> anti_join(GSCAT_unconv, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS","TOTWGT", "TOTNO"))   |> 
    mutate(SRC = "GSCAT: converted") 
  # GSDET -------------------------------------------------------------------
  # limit to particular fields
  raw_GSDET <- tblList$GSDET |> 
    select(MISSION, SETNO, SPEC, SIZE_CLASS,FLEN, FSEX, SPECIMEN_ID, FWT, CLEN, AGE, FMAT, FSHNO) |> 
    left_join(
      tblList$GSMISSIONS |> select(MISSION, VESEL, YEAR),
      by = "MISSION"
    ) |> 
    mutate(SRC="GSDET",
           FROM_VESSEL = case_when(
             VESEL == "A" ~ "ATC_HAM",
             VESEL == "H" ~ "ATC_HAM",
             VESEL == "J" ~ "CAR_CAB", # <<CARTIER>>
             VESEL == "B" ~ "CAR_CAB",
             VESEL == "N" ~ "NED_TEM",
             VESEL == "S" ~ "TEL_VEN",
             VESEL == "T" ~ "NED_TEM",
             VESEL == "V" ~ "TEL_VEN", # not in data, but putting here anyways
             .default = NA_character_
           ))
  
  # separate recs into convertible and unconvertible
  # put spp that should use GSCAT processing into GSDET_unconv (i.e. c(2212,2214,2415, ...))  
  # put recs that don't need conversions done by FLEN (i.e. GSCAT_SPEC_LIST_*)  
  # If a species requires a conversion factor for the TEL/VEN to CAR/CAB, those species have to have those conversion
  # factors applied to all of the historical vessels used so t[hat's why the NED, TEM, ATC and HAM all get the same treatment here.
  GSDET_forconv_pre <- raw_GSDET |> filter((grepl("TEL|VEN|NED|TEM|ATC|HAM", MISSION) & SPEC %in% GSCAT_SPEC_LIST_TELVEN$SPEC) |
                                             (grepl("NED|TEM|ATC|HAM", MISSION) & SPEC %in% GSCAT_SPEC_LIST_NEDTEM$SPEC))  |>  
    filter( !( SPEC %in% c(2212,2214,2415,2555,4521,4536,8500,8601,6411,6500,6511) | 
                 SPEC >= 9400 | 
                 SPEC %in% c(1091,1092) ) )
  
  GSDET_unconv <- raw_GSDET |> 
    anti_join(GSDET_forconv_pre, by = c("MISSION", "SETNO", "SPEC", "SRC",  "SPECIMEN_ID", "SIZE_CLASS","FSEX", "FWT", "CLEN")) |> 
    rename(TOTWGT = FWT) |> #Deleted CLEN rename here
    mutate(SRC = "GSDET: unconverted",
           CF_USED = "None",
           TOTNO= CLEN) |> #Added TOTNO here so we keep both CLEN and TOTNO
    select(-YEAR, -VESEL)
  
  # ensure that berried females count as females and that a value for sex exists
  GSDET_forconv_pre <- GSDET_forconv_pre |>
    mutate(
      FSEX = case_when(
        FSEX == 3 ~ 2,
        FSEX %in% c(1, 2) ~ FSEX,
        .default = 0
      )
    ) |>
    left_join(
      GSCAT_forconv_pre |>
        select(MISSION, SETNO, SPEC, SIZE_CLASS, SAMPTOT_Ratio),
      by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")
    ) |>
    mutate(CLEN = CLEN * SAMPTOT_Ratio,
           SRC = "GSDET: converted") |>
    select(-SAMPTOT_Ratio) 
  
  
  #I added this to make sure the unconverted gsdet data we bind back in later is treated the same as the converted ones
  # ensure that berried females count as females and that a value for sex exists
  GSDET_unconv <- GSDET_unconv |>
    mutate(
      FSEX = case_when(
        FSEX == 3 ~ 2,
        FSEX %in% c(1, 2) ~ FSEX,
        .default = 0
      )
    ) |>
    left_join(
      GSCAT_forconv_pre |>
        select(MISSION, SETNO, SPEC, SIZE_CLASS, SAMPTOT_Ratio),
      by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")
    ) |>
    mutate(FWT=TOTWGT, #Added this to account for the actual weight of the fish
      TOTNO = TOTNO * SAMPTOT_Ratio,
           SRC = "GSDET: converted") |>
    select(-SAMPTOT_Ratio) 
  
  
  
  
  # find records in GSCAT that don't have associated data in GSDET
  GSCAT_forconv<- GSCAT_forconv_pre |>
    anti_join(GSDET_forconv_pre, by = c("MISSION", "SETNO", "SPEC","SIZE_CLASS")) |>
    rename(FWT = TOTWGT, CLEN = TOTNO) |>
    select(MISSION, SETNO, SPEC, FWT, CLEN, FROM_VESSEL, SIZE_CLASS) |>
    mutate(
      FWT = FWT, 
      SRC = "GSCAT: converted", # Note source for unanticipated debugging
      FLEN = NA, # GSCAT does not have values the fields below - all set to NA
      AGE = NA,
      FMAT = NA,
      FSEX = NA,
      FSHNO = NA,
      SPECIMEN_ID = NA
    )
  # find records in GSCAT that DO have associated data in GSDET
  GSCAT_useGSDET <- GSCAT_forconv_pre  |>  
    semi_join(GSDET_forconv_pre, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS")) |>
    mutate(SRC = "GSCAT: converted using GSDET records not GSCAT",
           TOTWGT = NA,
           TOTNO = NA) |>
    select(MISSION, SETNO, SPEC, SIZE_CLASS, TOTWGT, TOTNO, FROM_VESSEL, SRC)
  
  # message("Some recs had weight = 9999 (e.g. snowcrab, scallops). Placeholder?")
  
  # CATDET - merge the AB info with GSDET/GSCAT
  # - first, attempt to merge on sex
  # - any records that couldn't be merged on SPEC and FSEX should be merged on SPEC alone
  # - any records that could not be merged on SPEC are retained
  # where possible, if FWT is blank, calculate it, convert to kgs
  # add FROM_VESSEL
  # "AGER", "CHKMRK", "EDGE", "FSHNO", "NANN", "REMARKS", "SIZE_CLASS", "SPECIMEN_ID"
  CATDET_base <- bind_rows(
    GSCAT_forconv,
    GSDET_forconv_pre 
  ) |>
    left_join(
      ABs |> select(SPEC, FSEX_key, LENWT_A, LENWT_B),
      by = c("SPEC" = "SPEC", "FSEX" = "FSEX_key")
    )
  
  CATDET <- bind_rows(
    CATDET_base |> filter(!is.na(LENWT_A), !is.na(LENWT_B)),
    CATDET_base |> filter(is.na(LENWT_A), is.na(LENWT_B)) |>
      select(any_of(names(GSDET_forconv_pre))) |>
      left_join(
        ABs |>
          filter(FSEX_key == 0) |>
          select(SPEC, LENWT_A, LENWT_B) |>
          distinct(),
        by = "SPEC"
      )
  ) |>
    mutate(
      SRC= ifelse(is.na(FWT),paste(SRC,";Weight_Derived",sep=""),SRC),#Added this to highlight FWT was derived from As and Bs
      FWT = if_else(
        is.na(FWT) | FWT == 0,
        LENWT_A * (FLEN^LENWT_B),
        FWT
      ),
      FWT = FWT
    ) |>
    left_join(
      tblList$GSMISSIONS |> select(MISSION, VESEL, YEAR),
      by = "MISSION"
    ) 
  rm(list = c("CATDET_base"))
  
  # from/to logic
  # to teleost:
  # from cam/hammond -> needler/templeman -> venture/teleost -> cabot/cartier
  # anywhere where the inversions is done, swap the from and to vessels?
  
  if(F){
    #Mike, I think this is the best spot to start introducing the backup biomass 
    #conversions. Once they get uploaded to the new RAW Table we can really test it 
    #out. but for now I just made some dummy data on the troublesome species.
    
    #For some species with length disaggregated conversion factors there is missing 
    #detailed data on lengths required to use conversion factors. Therefore, it is 
    #necessary to use supplemental conversion factors for biomass from overall 
    #catch weights to maintain an in-tact time series. 
    
    Backup_Biomass_Conversions<- GSCONVERSIONS_RAW |>
      filter(CF_METRIC=="BIOMASS") |>
      select(-c(CI_UPPER,CI_LOWER,ASSEMBLAGE_ID))
    
    
    #This is all dummy data and can be deleted once we get the tables updated but I 
    #left it here in case you wanted to test some #########################
    Tester<-head(Backup_Biomass_Conversions,4)
    new_vals<-c(2550, 2513, 2511, 11)
    
    Tester$SPEC[1:4] <- new_vals
    TesterA<-Tester  
    TesterA$CF_METRIC<-"ABUNDANCE"
    Backup_Biomass_Conversions<-rbind(Backup_Biomass_Conversions,Tester,TesterA)
    ##################################################################################################
    
    Backup_Biomass_Conversions<- Backup_Biomass_Conversions[Backup_Biomass_Conversions$SPEC %in% GSCONVERSIONS$SPEC[GSCONVERSIONS$CF_MODEL_TYPE=="LDM"], ]
    GSCONVERSIONS<-rbind(GSCONVERSIONS,Backup_Biomass_Conversions)
  }
  #round value or we get duplicate conv factors for the same sp and joins start adding rows
  GSCONVERSIONS[!is.na(GSCONVERSIONS$CF_VALUE),"CF_VALUE"]<- round(GSCONVERSIONS[!is.na(GSCONVERSIONS$CF_VALUE),"CF_VALUE"],8)
  GSCONVERSIONS <- GSCONVERSIONS |> unique()
  
  # First lets deal with all the to cartier/cabot conversions we need to account for
  TELVEN_TO_CARCAB_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(TELVEN_TO_CARCAB_ABUND = CF_VALUE)
  
  NEDTEM_TO_CARCAB_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(NEDTEM_TO_CARCAB_ABUND = CF_VALUE) |>
    mutate(FROM_VESSEL = "NED_TEM")
  
  ATCHAM_TO_CARCAB_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(ATCHAM_TO_CARCAB_ABUND = CF_VALUE) |>
    mutate(FROM_VESSEL = "ATC_HAM")
  
  TELVEN_TO_CARCAB_BMASS_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(TELVEN_TO_CARCAB_BMASS = CF_VALUE)
  
  NEDTEM_TO_CARCAB_BMASS_CONV <- GSCONVERSIONS |>    
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(NEDTEM_TO_CARCAB_BMASS = CF_VALUE) |>
    mutate(FROM_VESSEL = "NED_TEM")
  
  ATCHAM_TO_CARCAB_BMASS_CONV <- GSCONVERSIONS |>    
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "TEL_VEN" & TO_VESSEL == "CAR_CAB") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(ATCHAM_TO_CARCAB_BMASS = CF_VALUE) |>
    mutate(FROM_VESSEL = "ATC_HAM")
  
  #Now for the NEDTEM to TELVEN
  
  NEDTEM_TO_TELVEN_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(NEDTEM_TO_TELVEN_ABUND = CF_VALUE)
  
  ATCHAM_TO_TELVEN_ABUND_CONV <- GSCONVERSIONS |>
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("ABUNDANCE") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN") |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(ATCHAM_TO_TELVEN_ABUND = CF_VALUE) |>
    mutate(FROM_VESSEL = "ATC_HAM")
  
  NEDTEM_TO_TELVEN_BMASS_CONV <- GSCONVERSIONS |>
    #Sandlance in summer is an oddball, we don't use the biomass conversion for it
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN" & !SPEC==610) |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(NEDTEM_TO_TELVEN_BMASS = CF_VALUE)
  
  ATCHAM_TO_TELVEN_BMASS_CONV <- GSCONVERSIONS |>
    #Sandlance in summer is an oddball, we don't use the biomass conversion for it
    filter(grepl(paste(season, "ALL", sep = "|"), SEASON) & CF_METRIC %in% c("BIOMASS") & FROM_VESSEL == "NED_TEM" & TO_VESSEL == "TEL_VEN" & !SPEC==610) |>
    select(SPEC, FLEN, CF_VALUE, FROM_VESSEL) |>
    unique() |> 
    rename(ATCHAM_TO_TELVEN_BMASS = CF_VALUE) |>
    mutate(FROM_VESSEL = "ATC_HAM")
  
  # These are just the conversion factors for the few species for ATC/HAM to NED/TEM
  ATCHAM_TO_NEDTEM_ABUND_CONV <- data.frame(
    SPEC = c(11, 40, 41, 42, 43),
    ATCHAM_TO_NEDTEM_ABUND_LAM = c(0.83333, 1.25, 1.25, 1.25, 1.25),
    FROM_VESSEL = c("ATC_HAM", "ATC_HAM", "ATC_HAM", "ATC_HAM", "ATC_HAM")
  )
  #FIXED THIS SO ATC AND HAM WOULD BE IN LINE WITH OTHER DATA FRAMES
  
  #First lets do all LDM models
  TELVEN_TO_CARCAB_ABUND_CONV_LDM <- subset(TELVEN_TO_CARCAB_ABUND_CONV, !is.na(FLEN))
  NEDTEM_TO_CARCAB_ABUND_CONV_LDM <- subset(NEDTEM_TO_CARCAB_ABUND_CONV, !is.na(FLEN))
  ATCHAM_TO_CARCAB_ABUND_CONV_LDM <- subset(ATCHAM_TO_CARCAB_ABUND_CONV, !is.na(FLEN))
  NEDTEM_TO_TELVEN_ABUND_CONV_LDM <- subset(NEDTEM_TO_TELVEN_ABUND_CONV, !is.na(FLEN))
  ATCHAM_TO_TELVEN_ABUND_CONV_LDM <- subset(ATCHAM_TO_TELVEN_ABUND_CONV, !is.na(FLEN))
  
  LF_Data_All <- merge(CATDET, TELVEN_TO_CARCAB_ABUND_CONV_LDM, by = c("SPEC", "FLEN", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, NEDTEM_TO_CARCAB_ABUND_CONV_LDM, by = c("SPEC", "FLEN", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, NEDTEM_TO_TELVEN_ABUND_CONV_LDM, by = c("SPEC", "FLEN", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_CARCAB_ABUND_CONV_LDM, by = c("SPEC", "FLEN", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_TELVEN_ABUND_CONV_LDM, by = c("SPEC", "FLEN", "FROM_VESSEL"), all.x = TRUE)
  
  #Second all LAM abundance models
  TELVEN_TO_CARCAB_ABUND_CONV_LAM <- subset(TELVEN_TO_CARCAB_ABUND_CONV, is.na(FLEN))
  TELVEN_TO_CARCAB_ABUND_CONV_LAM <- TELVEN_TO_CARCAB_ABUND_CONV_LAM |>
    select(c(-FLEN)) |>
    unique() |> 
    rename("TELVEN_TO_CARCAB_ABUND_LAM" = "TELVEN_TO_CARCAB_ABUND")
  
  NEDTEM_TO_CARCAB_ABUND_CONV_LAM <- subset(NEDTEM_TO_CARCAB_ABUND_CONV, is.na(FLEN))
  NEDTEM_TO_CARCAB_ABUND_CONV_LAM <- NEDTEM_TO_CARCAB_ABUND_CONV_LAM |>
    select(c(-FLEN)) |>
    unique() |> 
    rename("NEDTEM_TO_CARCAB_ABUND_LAM" = "NEDTEM_TO_CARCAB_ABUND")
  
  ATCHAM_TO_CARCAB_ABUND_CONV_LAM <- subset(ATCHAM_TO_CARCAB_ABUND_CONV, is.na(FLEN))
  ATCHAM_TO_CARCAB_ABUND_CONV_LAM <- ATCHAM_TO_CARCAB_ABUND_CONV_LAM |>
    select(c(-FLEN)) |>
    unique() |> 
    rename("ATCHAM_TO_CARCAB_ABUND_LAM" = "ATCHAM_TO_CARCAB_ABUND")
  
  NEDTEM_TO_TELVEN_ABUND_CONV_LAM <- subset(NEDTEM_TO_TELVEN_ABUND_CONV, is.na(FLEN))
  NEDTEM_TO_TELVEN_ABUND_CONV_LAM <- NEDTEM_TO_TELVEN_ABUND_CONV_LAM |>
    select(c(-FLEN)) |>
    unique() |> 
    rename("NEDTEM_TO_TELVEN_ABUND_LAM" = "NEDTEM_TO_TELVEN_ABUND")
  
  ATCHAM_TO_TELVEN_ABUND_CONV_LAM <- subset(ATCHAM_TO_TELVEN_ABUND_CONV, is.na(FLEN))
  ATCHAM_TO_TELVEN_ABUND_CONV_LAM <- ATCHAM_TO_TELVEN_ABUND_CONV_LAM |> 
    select(c(-FLEN)) |>
    unique() |> 
    rename("ATCHAM_TO_TELVEN_ABUND_LAM" = "ATCHAM_TO_TELVEN_ABUND")
  
  LF_Data_All <- merge(LF_Data_All, TELVEN_TO_CARCAB_ABUND_CONV_LAM, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE  )
  LF_Data_All <- merge(LF_Data_All, NEDTEM_TO_CARCAB_ABUND_CONV_LAM, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE  )
  LF_Data_All <- merge(LF_Data_All, NEDTEM_TO_TELVEN_ABUND_CONV_LAM, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE  )
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_CARCAB_ABUND_CONV_LAM, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE  )
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_TELVEN_ABUND_CONV_LAM, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE  )
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_NEDTEM_ABUND_CONV, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE  )
  
  #Third all biomass
  TELVEN_TO_CARCAB_BMASS_CONV <- TELVEN_TO_CARCAB_BMASS_CONV |> select(c(-FLEN)) |> unique()
  NEDTEM_TO_CARCAB_BMASS_CONV <- NEDTEM_TO_CARCAB_BMASS_CONV |> select(c(-FLEN)) |> unique()
  ATCHAM_TO_CARCAB_BMASS_CONV <- ATCHAM_TO_CARCAB_BMASS_CONV |> select(c(-FLEN)) |> unique()
  NEDTEM_TO_TELVEN_BMASS_CONV <- NEDTEM_TO_TELVEN_BMASS_CONV |> select(c(-FLEN)) |> unique()
  ATCHAM_TO_TELVEN_BMASS_CONV <- ATCHAM_TO_TELVEN_BMASS_CONV |> select(c(-FLEN)) |> unique()
  
  LF_Data_All <- merge(LF_Data_All, TELVEN_TO_CARCAB_BMASS_CONV, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, NEDTEM_TO_CARCAB_BMASS_CONV, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, NEDTEM_TO_TELVEN_BMASS_CONV, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_CARCAB_BMASS_CONV, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE)
  LF_Data_All <- merge(LF_Data_All, ATCHAM_TO_TELVEN_BMASS_CONV, by = c("SPEC", "FROM_VESSEL"), all.x = TRUE)
  
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
  
  SpeciesB <- subset(GSCONVERSIONS, CF_METRIC == "BIOMASS" & !CF_VALUE == 1 & FROM_VESSEL == "NED_TEM")
  Species_List_BMASS_NEDTEM <- unique(SpeciesB$SPEC)
  
  SpeciesB <- subset(GSCONVERSIONS, CF_METRIC == "BIOMASS" & !CF_VALUE == 1 & FROM_VESSEL == "TEL_VEN")
  Species_List_BMASS_TELVEN <- unique(SpeciesB$SPEC)
  
  #Now we have to account for abundance ones
  SpeciesA <- subset(GSCONVERSIONS, CF_METRIC == "ABUNDANCE" & !CF_VALUE == 1 & FROM_VESSEL == "NED_TEM")
  Species_List_ABUND_NEDTEM <- unique(SpeciesA$SPEC)
  
  SpeciesA <- subset(GSCONVERSIONS, CF_METRIC == "ABUNDANCE" & !CF_VALUE == 1 & FROM_VESSEL == "TEL_VEN")
  Species_List_ABUND_TELVEN <- unique(SpeciesA$SPEC)
  
  #I added in these to deal with the new special circumstances when the backup biomass and backup abundance are reuired
  SpeciesA <- subset(GSCONVERSIONS, CF_METRIC == "ABUNDANCE" & !CF_VALUE == 1 & FROM_VESSEL == "NED_TEM" & CF_MODEL_TYPE=="LDM" & !is.na(FLEN))  
  Species_List_ABUND_NEDTEM_LDM <- unique(SpeciesA$SPEC)
  
  SpeciesA <- subset(GSCONVERSIONS, CF_METRIC == "ABUNDANCE" & !CF_VALUE == 1 & FROM_VESSEL == "TEL_VEN" & CF_MODEL_TYPE=="LDM" & !is.na(FLEN))
  Species_List_ABUND_TELVEN_LDM <- unique(SpeciesA$SPEC)
  
  LF_Data_All <- LF_Data_All |>
    mutate(
      across(c("FWT", "CLEN"), ~ ifelse(is.na(.), 0, .)),
      across(
        c(
          TELVEN_TO_CARCAB_ABUND,
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
          ATCHAM_TO_TELVEN_BMASS
        ),
        ~ ifelse(is.na(.), 1, .)
      ),
      across( #New ifelse statement across all these columns. Right now the new backup biomass and abundance conversion factors get applied to all species with them even if the FLEN data is there. This is just an extra step to say if there is FLEN data, change the LAM abundace conversion factors to 1 so when the CLEN column gets converted it isn't doing more conversions then needed
        c(
          TELVEN_TO_CARCAB_ABUND_LAM,
          NEDTEM_TO_CARCAB_ABUND_LAM,
          ATCHAM_TO_CARCAB_ABUND_LAM,
        ),
        ~ ifelse(!is.na(FLEN) & SPEC %in% Species_List_ABUND_TELVEN_LDM , 1, .)
      ),
      across(
        c(
          NEDTEM_TO_TELVEN_ABUND_LAM,
          ATCHAM_TO_TELVEN_ABUND_LAM
        ),
        ~ ifelse(!is.na(FLEN) & SPEC %in% Species_List_ABUND_NEDTEM_LDM , 1, .)
      ),
      TOTNO_RAW = CLEN,
      TOTWGT_RAW = FWT,
      CF_USED = NA,
      TOTNO = ifelse(
        is.na(FLEN),
        TOTNO_RAW /
          TELVEN_TO_CARCAB_ABUND_LAM /
          NEDTEM_TO_CARCAB_ABUND_LAM /
          NEDTEM_TO_TELVEN_ABUND_LAM /
          ATCHAM_TO_CARCAB_ABUND_LAM /
          ATCHAM_TO_TELVEN_ABUND_LAM /
          ATCHAM_TO_NEDTEM_ABUND_LAM,
        TOTNO_RAW / #Added TOTNO_RAW here
          TELVEN_TO_CARCAB_ABUND /
          NEDTEM_TO_CARCAB_ABUND /
          NEDTEM_TO_TELVEN_ABUND /
          ATCHAM_TO_CARCAB_ABUND /
          ATCHAM_TO_TELVEN_ABUND /
          TELVEN_TO_CARCAB_ABUND_LAM /
          NEDTEM_TO_CARCAB_ABUND_LAM /
          NEDTEM_TO_TELVEN_ABUND_LAM /
          ATCHAM_TO_CARCAB_ABUND_LAM /
          ATCHAM_TO_TELVEN_ABUND_LAM /
          ATCHAM_TO_NEDTEM_ABUND_LAM),
      TOTWGT = ifelse(
        is.na(FLEN) &
          SPEC %in% c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM),
        TOTWGT_RAW /
          TELVEN_TO_CARCAB_BMASS /
          NEDTEM_TO_CARCAB_BMASS /
          NEDTEM_TO_TELVEN_BMASS /
          ATCHAM_TO_CARCAB_BMASS /
          ATCHAM_TO_TELVEN_BMASS,
        ifelse(SPEC %in% Species_List_ABUND_TELVEN & substr(SRC, 1, 5) != "GSCAT" & FROM_VESSEL %in% c("TEL_VEN", "NED_TEM", "ATC_HAM"), TOTNO * TOTWGT_RAW,
               ifelse(SPEC %in% Species_List_ABUND_NEDTEM & substr(SRC, 1, 5) != "GSCAT" & FROM_VESSEL %in% c("NED_TEM", "ATC_HAM"), TOTNO * TOTWGT_RAW,
                      ifelse(SPEC %in% Species_List_BMASS_TELVEN & !SPEC %in% Species_List_ABUND_TELVEN & FROM_VESSEL %in% c("TEL_VEN"),
                             TOTWGT_RAW /
                               TELVEN_TO_CARCAB_BMASS /
                               NEDTEM_TO_CARCAB_BMASS /
                               NEDTEM_TO_TELVEN_BMASS /
                               ATCHAM_TO_CARCAB_BMASS /
                               ATCHAM_TO_TELVEN_BMASS,
                             ifelse(SPEC %in% c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM) & !SPEC %in% c(Species_List_ABUND_TELVEN, Species_List_ABUND_NEDTEM) & FROM_VESSEL %in% c("NED_TEM", "ATC_HAM"),
                                    TOTWGT_RAW /
                                      TELVEN_TO_CARCAB_BMASS /
                                      NEDTEM_TO_CARCAB_BMASS /
                                      NEDTEM_TO_TELVEN_BMASS /
                                      ATCHAM_TO_CARCAB_BMASS /
                                      ATCHAM_TO_TELVEN_BMASS,
                                    TOTWGT_RAW
                             )
                      )
               )
        )
      ),
      CONV_USED = ifelse(
        FROM_VESSEL %in% c("NONE"),
        "NONE",
        ifelse(
          is.na(FLEN) &
            SPEC %in% c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM),
          "BIOMASS",
          ifelse(
            SPEC %in%
              Species_List_ABUND_TELVEN &
              substr(SRC, 1, 5) != "GSCAT"&
              FROM_VESSEL %in% c("TEL_VEN", "NED_TEM", "ATC_HAM"),
            "ABUND",
            ifelse(
              SPEC %in%
                Species_List_ABUND_NEDTEM &
                substr(SRC, 1, 5) != "GSCAT" &
                FROM_VESSEL %in% c("NED_TEM", "ATC_HAM"),
              "ABUND",
              ifelse(
                SPEC %in%
                  Species_List_BMASS_TELVEN &
                  !SPEC %in% Species_List_ABUND_TELVEN &
                  FROM_VESSEL %in% c("TEL_VEN"),
                "BIOMASS",
                ifelse(
                  SPEC %in%
                    c(Species_List_BMASS_TELVEN, Species_List_BMASS_NEDTEM) &
                    !SPEC %in%
                    c(Species_List_ABUND_TELVEN, Species_List_ABUND_NEDTEM) &
                    FROM_VESSEL %in% c("NED_TEM", "ATC_HAM"),
                  "BIOMASS",
                  "NONE"
                )
              )
            )
          )
        )
      )
    )
  
    message(
      "How conversion factors are applied and how biomass is calculated can affect results.
  This script applies conversion factors and calculates biomass in the following way:
            1: All length disaggregated/aggregated abundance conversion factors are
            \tapplied to count at lengths or total number caught at the set level.
            2: If count at length data is NOT available, Biomass conversions
            \tare used and biomass is calculated from total weight at the set level.
            3: If count at length data and abundance conversion factors not
            \tequal to 1 are available, biomass is calculated by the sum of the product
            \tof count at lengths and fish weights (either by measured weight or derived
            \tfrom length to weight relationships when no weights were taken).
            4: If a species has both a biomass and abundance conversion factor that are not
            \tequal to 1 and that species has length frequency data, only the abundance
            \tconversion factor is applied to data and biomass is calculated as stated above.
            \tAn exception to this rule is when that species has missing length frequency
            \tdata for a given set, in which the biomass conversion is used instead.
            5: For species without length frequency data, both biomass and abundance
            \tconversion factors are applied when available."
    )
  
  LF_Data_All <- LF_Data_All |>
    mutate(
      # Intermediate variables for conditions
      neither_condition = (TELVEN_TO_CARCAB_ABUND *
                             NEDTEM_TO_CARCAB_ABUND *
                             NEDTEM_TO_TELVEN_ABUND *
                             ATCHAM_TO_CARCAB_ABUND *
                             ATCHAM_TO_TELVEN_ABUND *
                             TELVEN_TO_CARCAB_ABUND_LAM *
                             NEDTEM_TO_CARCAB_ABUND_LAM *
                             NEDTEM_TO_TELVEN_ABUND_LAM *
                             ATCHAM_TO_CARCAB_ABUND_LAM *
                             ATCHAM_TO_TELVEN_ABUND_LAM *
                             ATCHAM_TO_NEDTEM_ABUND_LAM ==
                             1) &
        (TELVEN_TO_CARCAB_BMASS *
           NEDTEM_TO_CARCAB_BMASS *
           NEDTEM_TO_TELVEN_BMASS *
           ATCHAM_TO_CARCAB_BMASS *
           ATCHAM_TO_TELVEN_BMASS ==
           1),
      either_condition = (TELVEN_TO_CARCAB_ABUND *
                            NEDTEM_TO_CARCAB_ABUND *
                            NEDTEM_TO_TELVEN_ABUND *
                            ATCHAM_TO_CARCAB_ABUND *
                            ATCHAM_TO_TELVEN_ABUND *
                            TELVEN_TO_CARCAB_ABUND_LAM *
                            NEDTEM_TO_CARCAB_ABUND_LAM *
                            NEDTEM_TO_TELVEN_ABUND_LAM *
                            ATCHAM_TO_CARCAB_ABUND_LAM *
                            ATCHAM_TO_TELVEN_ABUND_LAM *
                            ATCHAM_TO_NEDTEM_ABUND_LAM !=
                            1) &
        (TELVEN_TO_CARCAB_BMASS *
           NEDTEM_TO_CARCAB_BMASS *
           NEDTEM_TO_TELVEN_BMASS *
           ATCHAM_TO_CARCAB_BMASS *
           ATCHAM_TO_TELVEN_BMASS !=
           1) &
        (substr(SRC, 1, 5) == "GSCAT"),###### Had to change these since the backup biomass and abundance are now being used
      abundance_condition = (TELVEN_TO_CARCAB_ABUND *
                               NEDTEM_TO_CARCAB_ABUND *
                               NEDTEM_TO_TELVEN_ABUND *
                               ATCHAM_TO_CARCAB_ABUND *
                               ATCHAM_TO_TELVEN_ABUND *
                               TELVEN_TO_CARCAB_ABUND_LAM *
                               NEDTEM_TO_CARCAB_ABUND_LAM *
                               NEDTEM_TO_TELVEN_ABUND_LAM *
                               ATCHAM_TO_CARCAB_ABUND_LAM *
                               ATCHAM_TO_TELVEN_ABUND_LAM *
                               ATCHAM_TO_NEDTEM_ABUND_LAM !=
                               1) &
        (substr(SRC, 1, 5) == "GDET"),
      biomass_condition = (TELVEN_TO_CARCAB_ABUND *
                             NEDTEM_TO_CARCAB_ABUND *
                             NEDTEM_TO_TELVEN_ABUND *
                             ATCHAM_TO_CARCAB_ABUND *
                             ATCHAM_TO_TELVEN_ABUND *
                             TELVEN_TO_CARCAB_ABUND_LAM *
                             NEDTEM_TO_CARCAB_ABUND_LAM *
                             NEDTEM_TO_TELVEN_ABUND_LAM *
                             ATCHAM_TO_CARCAB_ABUND_LAM *
                             ATCHAM_TO_TELVEN_ABUND_LAM *
                             ATCHAM_TO_NEDTEM_ABUND_LAM ==
                             1) &
        (TELVEN_TO_CARCAB_BMASS *
           NEDTEM_TO_CARCAB_BMASS *
           NEDTEM_TO_TELVEN_BMASS *
           ATCHAM_TO_CARCAB_BMASS *
           ATCHAM_TO_TELVEN_BMASS !=
           1),
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
    ) |>
    select(
      SPEC,
      FLEN,
      FROM_VESSEL,
      MISSION,
      FSEX,
      SETNO,
      SRC,
      AGE,
      FMAT,
      CLEN,#Added this to carry it through
      FWT,#Added this to carry it through
      TOTNO,
      TOTWGT,
      CF_USED,
      FSHNO,
      SIZE_CLASS,
      SPECIMEN_ID
    ) 
  
  
  message(
    "Note that for records where 'ABUNDANCE_PROBLEM'or 'BIOMASS_PROBLEM', 
the preferred CF will result in 0, while the other has a non-zero value."
  )
  
  taxAll <- data.frame()
  if ("TAXA_" %in% names(tblList$GSDET)) {
    taxAll <- tblList$GSDET |> select(SPEC, TAXA_, TAXARANK_) |> unique()
  }
  if ("TAXA_" %in% names(tblList$GSCAT)) {
    taxAll <- bind_rows(
      taxAll,
      tblList$GSCAT |> select(SPEC, TAXA_, TAXARANK_)
    ) |>
      unique()
  }
  
  if (nrow(taxAll) > 0) {
    LF_Data_All <- LF_Data_All |> left_join(taxAll, by = "SPEC")
    GSDET_unconv <- GSDET_unconv |> left_join(taxAll, by = "SPEC")
  }
  
  detcols <- c("MISSION", "SETNO", "SPEC", "FLEN", "FSEX", "AGE", "FMAT",  "SRC", "CF_USED", "TOTNO", "TOTWGT", "FSHNO", "SIZE_CLASS", "SPECIMEN_ID")
  catcols <- c("MISSION", "SETNO", "SPEC", "TOTNO", "TOTWGT")
  
  catgrpcols <- c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "SRC") #, "SAMPWGT"
  if ("TAXA_" %in% names(LF_Data_All)) {
    detcols <- append(detcols, c("TAXA_", "TAXARANK_"), after = 3)
    catcols <- append(catcols, c("TAXA_", "TAXARANK_"), after = 3)
    catgrpcols <- append(catgrpcols, c("TAXA_", "TAXARANK_"), after = 3)
  }
  
  

  
  newDet <- rbind.data.frame(LF_Data_All[substr(LF_Data_All$SRC, 1,5)=="GSDET",],
                             GSDET_unconv)
  
  newCat <- LF_Data_All |>
    group_by(MISSION, SETNO, SPEC, FROM_VESSEL, SRC, SIZE_CLASS) |>
    summarise(TOTWGT=sum(TOTWGT), TOTNO=sum(TOTNO))
  
  newCat <- rbind.data.frame(newCat,
                             GSCAT_unconv)
  
  newCat <- newCat |>
    mutate(
      TOTWGT = TOTWGT / 1000
    )
  
  #newDet <- newDet |> #I think we can just delete this since we are carrying over both CLEN and TOTNO and FWT and TOTWGT now
    #rename(CLEN = TOTNO)
  #I don't think we need this anymore since we just carry over all the raw values now
  #newDet <- merge(newDet, ogGSDET, by=c("MISSION", "SETNO", "SPEC", "SIZE_CLASS", "FLEN", "SPECIMEN_ID"))
  
  #This one seems fine since it can get merged up easily and we don't need the carryover
  newCat <- merge(newCat, ogGSCAT, by = c("MISSION", "SETNO", "SPEC", "SIZE_CLASS"))
  tblList$GSDET_CONV <-  newDet
  tblList$GSCAT_CONV <- newCat
  return(tblList)
}
