devtools::load_all()
#Mar.utils::debugMode(enable = T,  package_path = "C:/git/Maritimes/RVTransmogrifier/" )

cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "BRUNSDONE", "<password>", "PTRAN")

#### LOADING ALL DATA (UNFILTERED) ####
all_data <- loadRVData(cxn = getCxn())

#### FILTERING 1: PRE-BUILT ####
# a variety of pre-built filters exist in loadRVData(), and the following can be used alone or together:
# survey, years, months, missions, strata, types, areas, code, aphiaid, taxa
# 'keep_nullsets' is used to specify if you want sets where a specified taxa was not caught.  
filtered_data1 <- loadRVData(cxn = getCxn(), years = c(2024:2025),  survey="GEORGES",code = 10, strata = "5Z2" )
filtered_data2 <- loadRVData(cxn = getCxn(), MISSION="NED2016016", years=2016, taxa = "ECHINODERMATA", type = c(1,3,5,8,9), keep_nullsets=F)

#### FILTERING 2A: LIMITLESS ####
# data can be filtered using any mechanism that doesn't change the structure of the data - just overwrite the default 
# object(s) with filtered version(s)
custom_filtered_data <- loadRVData(cxn = getCxn(), aphiaid = 126175) #aphiaid is for redfish
custom_filtered_data$GSSEX <- subset(custom_filtered_data$GSSEX, CODE ==2 ) # subset() - base R
custom_filtered_data$GSDET <- custom_filtered_data$GSDET[custom_filtered_data$GSDET$FLEN>=40,] # square brackets - base R
custom_filtered_data$GSMISSIONS <- custom_filtered_data$GSMISSIONS[with(custom_filtered_data$GSMISSIONS, YEAR %in% c(2016,2017)), ] # with() - base R
custom_filtered_data$GSXTYPE <- custom_filtered_data$GSXTYPE[which(custom_filtered_data$GSXTYPE$XTYPE %in% 1), ] # which() - base R
custom_filtered_data$GSSTRATUM <- dplyr::filter(custom_filtered_data$GSSTRATUM, STRAT %in% c(440:465)) #dplyr::filter()

# At this stage, the discrete tables have been filtered, but the records may not be self consistent.
# e.g. - GSDET records includes records for all maturities (not just 3:6), it contains data for male and unsexed specimens, 
#        it contains records for all years, and for all reported species (not just redfish) 
#      - GSINF records include all sets - not just those in the chosen years or strata

# To ensure the specified filters cascade across all possible tables, you have to run propagateChanges()
custom_filtered_data <- propagateChanges(custom_filtered_data)

#### PLOTTING ####
custom_filtered_data_TOTWGT <- plotRV(custom_filtered_data, plotSets = "TOTWGT",plotBathy=NULL)
custom_filtered_data_TOTNO <- plotRV(custom_filtered_data, plotSets = "TOTNO",plotBathy=NULL)

#### generate datasets for OpenData ####
survey_GEORGES  <- extractFGP(cxn = getCxn(), survey = "GEORGES", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251023")
survey_4VSW  <- extractFGP(cxn = getCxn(), survey = "4VSW", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251023")
survey_FALL  <- extractFGP(cxn = getCxn(), survey = "FALL", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251023")
survey_SUMMER <- extractFGP(cxn = getCxn(), years= 2025, survey = "SUMMER", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251031")

#### generate datasets for OBIS ####
obis_survey_GEORGES_2025  <- extractOBIS(cxn = getCxn(), survey = "GEORGES", years= 2025, path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/OBIS/20251031")

#### generate datasets for DATRAS ####
datras_survey_SUMMER_2016  <- extractDATRAS(cxn = getCxn(), survey = "SUMMER", years= 2016, path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/DATRAS/20251113")

#### do whatever STRANAL (& Stratisfy) does ####
SHake_2016 <- loadRVData(cxn = getCxn(), code = 14, years= "2016", survey="SUMMER", strata=c(440:495), types=1)

# stratify using tblList
SHake_2016_post_strat  <- stratify_simple(tblList = SHake_2016)

# stratify using a dataframe
SHake_2016_flat <- easyFlatten(SHake_2016)
SHake_2016_flat_post_strat  <- stratify_simple(df = SHake_2016_flat)

#stratify with lengths
testData_len <- loadRVData(cxn = getCxn(), code = 43, years= c(2010:2021), survey="GEORGES", strata=c("5Z1", "5Z2", "5Z3", "5Z4"), types=1)
testData_len_det<-stratify_detailed(testData_len)
plotRV(tblList = testData_len, catchStrataData = testData_len_det$stratified_byStrat, plotCatchStrata = "BIOMASS")

testData_len_det_wide <- widen_length_data(testData_len_det$length_set, value_col = "CLEN_SQKM_TOTAL",  bin_size = 3,level = "set")


# plot strat level biomass/abundance
plotRV(tblList = SHake_2016, catchStrataData = SHake_2016_post_strat$stratified_byStrat, plotCatchStrata = "ABUNDANCE", plotSets = NULL)
plotRV(tblList = SHake_2016, catchStrataData = SHake_2016_post_strat$stratified_byStrat, plotCatchStrata = "BIOMASS")



#### *NEW* WORKING WITH TAXONOMIC GROUPS  ####
# beyond species codes, we can now filter the data by any taxonomic group.  We can use these groups to simply facilitate
# extractions of multiple species, or we can aggregate the data together and consider the group as a whole



# Here we extract SUMMER 2016 echinoderms
echinoderms_summer_2016 <- loadRVData(cxn = getCxn(), survey = "SUMMER", years = 2016, taxa="ECHINODERMATA")

#Now we have all of the echinoderms, and we can handle them by species, if we choose...
plotRV(echinoderms_summer_2016)

#we can see that 14 different species of echinoderms were caught in SETNO 122
echinoderms_summer_2016$GSCAT |> filter(SETNO== 122)
# TAXA_ TAXARANK_    MISSION SETNO SPEC CALWT SAMPWGT TOTWGT TOTNO
# ECHINODERMATA    PHYLUM NED2016016   122 6411     0       0  0.035    19
# ECHINODERMATA    PHYLUM NED2016016   122 6115     0       0  0.046     6
# ECHINODERMATA    PHYLUM NED2016016   122 6114     0       0  0.252     4
# ECHINODERMATA    PHYLUM NED2016016   122 6611     0       0  1.120    10
# ECHINODERMATA    PHYLUM NED2016016   122 6123     0       0  0.002     2
# ECHINODERMATA    PHYLUM NED2016016   122 6300     0       0  1.200     7
# ECHINODERMATA    PHYLUM NED2016016   122 6118     0       0  0.027    18
# ECHINODERMATA    PHYLUM NED2016016   122 6111     0       0  0.015    38
# ECHINODERMATA    PHYLUM NED2016016   122 6118     0       0  0.027    18
# ECHINODERMATA    PHYLUM NED2016016   122 6600     0       0  0.023     8
# ECHINODERMATA    PHYLUM NED2016016   122 6121     0       0  0.001     3
# ECHINODERMATA    PHYLUM NED2016016   122 6117     0       0  1.309     3
# ECHINODERMATA    PHYLUM NED2016016   122 6200     0       0  0.132   260
# ECHINODERMATA    PHYLUM NED2016016   122 6719     0       0  0.002     1

# Now we can aggregate all of them together, and treat them as a single group
echinoderms_summer_2016_agg <- aggregateByTaxa(tblList=echinoderms_summer_2016)

#looking at set 122 now, we only have 1 record, which sums all of the numbers and weights:
echinoderms_summer_2016_agg$GSCAT |> filter(SETNO== 122)
# TAXA_ TAXARANK_    MISSION SETNO CALWT SAMPWGT TOTWGT TOTNO
# ECHINODERMATA    PHYLUM NED2016016   122     0       0  4.191   397

# Now the plot shows the aggregate values of all of the echinoderm species 
plotRV(echinoderms_summer_2016_agg)

#We can also stratify the values of this group, in the same way we would do it for a single species
echinoderms_summer_2016_agg_stratified <- stratify_simple(echinoderms_summer_2016_agg)
# echinoderms_summer_2016_agg_stratified |> head()
# TAXA_ STRAT    AREA_KM COUNT TOTWGT_sum TOTNO_sum TOTWGT_mean TOTNO_mean TOTWGT_SE   TOTNO_SE
# ECHINODERMATA   440  3169.2276     4     12.068      1946     3.01700  486.50000   1.60855  275.92466
# ECHINODERMATA   441  3429.9000     7     10.543       528     1.50614   75.42857   0.62123   54.17734
# ECHINODERMATA   442  4928.7663     7    140.819     45275    20.11700 6467.85714   7.97676 4793.86118
# ECHINODERMATA   443  4520.6082     4     48.412      2267    12.10300  566.75000   5.03772  171.99776
# ECHINODERMATA   444 13462.3575    17     51.902      1346     3.05306   79.17647   0.85155   16.13323
# ECHINODERMATA   445  3508.7877     4      2.649        42     0.66225   10.50000   0.35448    3.61709
# TOTWGT_sqkm_strat_mean TOTNO_sqkm_strat_mean   BIOMASS_SE    ABUNDANCE_SE       BIOMASS       ABUNDANCE
#              76.15612           12287.29265 128769.15306  22067481.99274  241356.07741  38941226.99566
#              38.40156            1867.37000  51115.68292   4549877.70843  131713.51064   6404892.36300
#             495.58780          160408.04987 959475.12892 592195338.11748 2442636.44733 790613790.44798
#             292.82972           13781.80892 545001.93496  19316131.97663 1323768.43344  62302158.41459
#              75.63098            1980.57447 280081.91855   5342706.77256 1018171.29084  26663201.57051
#              16.81935             265.68455  31500.94558    328711.12813   59015.52840    932230.68112









#### work on Conversion Factors
data_GEORGES_1999 <- loadRVData(cxn = getCxn(), code = 14, survey="GEORGES", years=1999)

tt <- applyConversionFactors(data_GEORGES_1999)








#### LOOPING THOUGH A PROCESS ####
years=c(2020:2025)
cod <- loadRVData(cxn = getCxn(), code = 10, years=years, survey="SUMMER", types=c(1))
for (y in years){
  thisData <- cod
  thisData$GSMISSIONS <- thisData$GSMISSIONS[thisData$GSMISSIONS$YEAR %in% y,]
  thisData <- propagateChanges(thisData)
  thisData_plot <- plotRV(thisData, plotSets = "TOTNO", plotBathy=NULL)
}

testData <- loadRVData(cxn = getCxn(), code = 10, years= "2016", survey="SUMMER", strata=c(440:495), types=1)
redfish <- loadRVData(cxn = getCxn(), code = 23, years= "2021", survey="SUMMER", strata=c(440:495), types=1)
plotRV(tblList = redfish)
redfish_strat_simp <- stratify_simple(tblList = redfish)

echin <- loadRVData(cxn = getCxn(), survey = "SUMMER", years = 2016, taxa="ECHINODERMATA")
plotRV(echin)
echin_AGG <- aggregateByTaxa(tblList=echin)
plotRV(echin_AGG)
echin_AGG_strat<-stratify_simple(echin_AGG)
plotRV(tblList = echin_AGG, catchStrataData = echin_AGG_strat$stratified_byStrat, plotCatchStrata = "BIOMASS", plotSets = NULL)






data_GEORGES_1999 <- loadRVData(cxn =cxnit(), code = c(2332), survey="SUMMER", years=c(1970:2025))