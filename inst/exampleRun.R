l
data <- loadRVData()
nrow(data$GSCAT)
data$GSMISSIONS <- data$GSMISSIONS[data$GSMISSIONS$SEASON=="SUMMER",]
data$GSMISSIONS <- data$GSMISSIONS[data$GSMISSIONS$YEAR %in% c(2014,2015),]
data$GSSPECIES_NEW <- data$GSSPECIES_NEW[data$GSSPECIES_NEW$CODE == 10,]
data <- propagateChanges(data, keep_nullsets = F)

data<-loadRVData()
data$GSSPECIES_NEW<- data$GSSPECIES[which(data$GSSPECIES_NEW$CLASS == "POLYCHAETA"),]
data <- propagateChanges(data, keep_nullsets = F)

data <- loadRVData()
cod_2016_transmogrified <- getSurvey("SUMMER_ALL", years = 2016, code=10)
codPlot_transmogrified <- plotRV(cod_2016_transmogrified, plotSets = "TOTWGT",plotBathy=NULL)
codPlot_transmogrified

tt <- getSurvey("SUMMER_ALL", years = 2016, code=10)
