cleantheGarbage()

devtools::load_all()
Mar.utils::debugMode(enable = T,  package_path = "C:/git/Maritimes/RVTransmogrifier/" )

cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), "BRUNSDONE", "<password>", "PTRAN")

#### LOADING ####
## unfiltered
data <- loadRVData(cxn = getCxn())

#### FILTERING: PRE-BUILT ####
# a variety of pre-built filters exist in loadRVData(), and the following can be used alone or together:
# survey, years, months, missions, strata, types, areas, code, aphiaid, taxa
data_2025 <- loadRVData(cxn = getCxn(), years = 2025)
data_GEORGES_2016 <- loadRVData(cxn = getCxn(), survey="GEORGES", years=2016)
data_ORDER_NUDIBRANCHIA <- loadRVData(cxn = getCxn(), taxa = "NUDIBRANCHIA")
data_northern_cod <- loadRVData(cxn = getCxn(), code = 10, strata=c(440:465), types=c(1,5))

#### FILTERING: LIMITLESS ###
# data can be filtered by any other field in the data set:
data_northern_cod <- loadRVData(cxn = getCxn(), code = 10, years=c(2020:2025), strata=c(440:465), types=c(1))
# 1) filtering the data (via subset)
data_northern_cod$GSSEX <- subset(data_northern_cod$GSSEX, CODE ==2 )
data_northern_cod$GSMATURITY <- subset(data_northern_cod$GSMATURITY, CODE %in% c(3,4,5,6) )
# 2) filtering the data (via square brackets)
data_northern_cod$GSDET <- data_northern_cod$GSDET_LF[data_northern_cod$GSDET$FLEN>=40,]
# propagating the changes
data_northern_large_mature_female_cod <- propagateChanges(data_northern_cod)

#### PLOTTING ###
data_northern_large_mature_female_cod_TOTWGT <- plotRV(data_northern_large_mature_female_cod, plotSets = "TOTWGT",plotBathy=NULL)
data_northern_large_mature_female_cod_TOTNO <- plotRV(data_northern_large_mature_female_cod, plotSets = "TOTNO",plotBathy=NULL)

#### LOOPING THOUGH A PROCESS ####
years=c(2020:2025)
cod <- loadRVData(cxn = getCxn(), code = 10, years=years, survey="SUMMER", types=c(1))
for (y in years){
  thisData <- cod
  thisData$GSMISSIONS <- thisData$GSMISSIONS[thisData$GSMISSIONS$YEAR %in% y,]
  thisData <- propagateChanges(thisData)
  thisData_plot <- plotRV(thisData, plotSets = "TOTNO", plotBathy=NULL)
}

#### WORKING WITH TAXONOMIC GROUPS ####
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
echinoderms_summer_2016_agg_stratified <- stratify(echinoderms_summer_2016_agg)
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

plotRV(tblList = echinoderms_summer_2016_agg, catchStrataData = echinoderms_summer_2016_agg_stratified, plotCatchStrata = "ABUNDANCE", plotSets = NULL)
plotRV(tblList = echinoderms_summer_2016_agg, catchStrataData = echinoderms_summer_2016_agg_stratified, plotCatchStrata = "BIOMASS", plotSets = NULL)

calcTotalSE(theDataByStrat = polychaetes_2016_transmogrified_agg_stratified, valueField = "BIOMASS", areaField = "AREA_KM")
polychaetes_2016_transmogrified_agg_stratified$BIOMASS

#### generate datasets for OpenData ####
survey_GEORGES  <- extractFGP(cxn = getCxn(), survey = "GEORGES", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251023")
survey_4VSW  <- extractFGP(cxn = getCxn(), survey = "4VSW", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251023")
survey_FALL  <- extractFGP(cxn = getCxn(), survey = "FALL", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251023")
survey_SUMMER <- extractFGP(cxn = getCxn(), years= 2025, survey = "SUMMER", path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/20251031")

#### generate datasets for OBIS ####
devtools::load_all()
Mar.utils::debugMode(enable = T,  package_path = "C:/git/Maritimes/RVTransmogrifier/" )
obis_survey_GEORGES_2025  <- extractOBIS(cxn = getCxn(), survey = "GEORGES", years= 2025, path="c:/Users/McMahonM/OneDrive - DFO-MPO/DataMgmt/FGP/OBIS/20251031")

#### generate datasets for DATRAS ####
devtools::load_all()
Mar.utils::debugMode(enable = T,  package_path = "C:/git/Maritimes/RVTransmogrifier/" )
tt<-extractDATRAS(years=2025, survey = "SUMMER", cxn = getCxn(),data.dir="C:/DFO-MPO/")

#### do whatever STRATISFY does ####
devtools::load_all()
Mar.utils::debugMode(enable = T,  package_path = "C:/git/Maritimes/RVTransmogrifier/" )
SHake_2016 <- loadRVData(cxn = getCxn(), code = 14, years= "2016", survey="SUMMER", strata=c(440:495), types=1)

# stratify using tblList
SHake_2016_post_strat  <- stratify(tblList = SHake_2016)

# stratify using df
SHake_2016_flat <- easyFlatten(SHake_2016)
SHake_2016_flat <- SHake_2016_flat[SHake_2016_flat$TYPE ==1,]  #shouldn't be run on anything other than type 1
SHake_2016_flat_post_strat  <- stratify(df = SHake_2016_flat)

#### work on Conversion Factors
devtools::load_all()
data_GEORGES_1999 <- loadRVData(cxn = getCxn(), code = 14, survey="GEORGES", years=1999)

tt <- applyConversionFactors(data_GEORGES_1999)



