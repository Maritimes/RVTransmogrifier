#' @title getSurvey
#' @description A survey is a defined by a combination of a range of months, a selection of strata, 
#' and a tow 'type'. This function ensures that all those values are combined correctly to get the 
#' valid data for a survey.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are:
#' \itemize{
#' \item \code{4X} - Type 1; Spring (i.e. months 1:4); 2008+; specific strata 
#' \item \code{GEORGES} - Type 1; Spring (i.e. months 1:4); strata 5Z*
#' \item \code{SPRING} - Type 1; Spring (i.e. months 1:4); pre-2008; specific strata 
#' \item \code{4VSW}  - Type 1; Spring (i.e. months 1:4); 4VSW strata;  
#' \item \code{SUMMER} - Type 1; Summer (i.e. months 5:8); strata 440:495 - the "standard" strata
#' \item \code{SUMMER_ALL} - Type 1; Summer (i.e. months 5:8); strata 434:559+5Z*
#' \item \code{FALL} - Type 1; Fall (i.e. months 9:12)
#' }
#' @param years the default is \code{NULL}. This parameter allows you to generate datasets for one or
#' more specific years.  A value of NULL will result in products being generated for all years for
#' which data exists, and a vector of years will result in dataset that include the specified years.
#' @param type1TowsOnly the default is \code{TRUE}.  "Valid" survey tows (i.e. "type 1") are those 
#' tows that fished correctly, and can be used confidently while looking data from a stratified  
#' random survey design.  Setting \code{type1TowsOnly} to \code{FALSE} will allow other types of sets 
#' to be extracted, but these shouldn't be included while assessing fish stocks.
#' @param ... other arguments passed to methods (i.e. 'keep_nullsets', debug' and 'quiet')
#' @returns a list of dataframes which have been filtered to only include data related to the 
#' specified survey and years
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
getSurvey<-function(survey = NULL, years=NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  if (is.null(survey)) stop("Please specify a value for 'survey'")
  tblList <- loadRVData(...)
  survey <- toupper(survey)
  if (args$type1TowsOnly) tblList$GSINF <- tblList$GSINF[tblList$GSINF$TYPE==1,]
  if (!is.null(years)){
    tblList$GSMISSIONS <- tblList$GSMISSIONS[which(tblList$GSMISSIONS$YEAR %in% c(years)),]
  }
  if (survey %in% c("4VSW", "4X", "GEORGES", "SPRING")){
    tblList$GSINF <- tblList$GSINF[which(lubridate::month(tblList$GSINF$SDATE) %in% c(1,2,3,4)),]
    if(survey == "4VSW"){
      tblList$GSINF <- tblList$GSINF[tblList$GSINF$STRAT %in% c('396','397', '398', '399', '400', 
                                                                '401', '402', '403', '404', '405', '406', '407', 
                                                                '408', '409', '410', '411'),]
    }else if (survey=="4X"){
      tblList$GSMISSIONS <- tblList$GSMISSIONS[tblList$GSMISSIONS$YEAR >= 2008,]
      tblList$GSINF <- tblList$GSINF[tblList$GSINF$STRAT %in% c('434', '436', '437', '438', '439', 
                                                                '440', '441', '442', '443', '444', '445', '446', '447', 
                                                                '448', '449', '450', '451', '452', '453', '454', '455', 
                                                                '456', '457', '458', '459', '460', '461', '462', '463', 
                                                                '464', '465', '466', '470', '471', '472', '473', '474', 
                                                                '475', '476', '477', '478', '480', '481', '482', '483', 
                                                                '484', '485', '490', '491', '492', '493', '494', '495', 
                                                                '496', '497', '498', '501', '502', '503', '504', '505', 
                                                                '557', '558', '559'),]
    }else if (survey=="GEORGES"){
      tblList$GSINF <- tblList$GSINF[tblList$GSINF$STRAT %in% c("5Z1", "5Z2", "5Z3", "5Z4", "5Z5", 
                                                                "5Z6", "5Z7", "5Z8", "5Z9"),]
    }else if (survey=="SPRING"){
      tblList$GSMISSIONS <- tblList$GSMISSIONS[tblList$GSMISSIONS$YEAR < 2008,]
      tblList$GSINF <- tblList$GSINF[!grepl(pattern = "5Z", x = tblList$GSINF$STRAT) &
                                       !(tblList$GSINF$STRAT %in% c('396','397', '398', '399', '400', '401', 
                                                                    '402', '403', '404', '405', '406', '407', 
                                                                    '408', '409', '410', '411', '551', '552', 
                                                                    '553', '554', '555', '556', '557', '558', 
                                                                    '559')),]
    }
  }else if (survey=="SUMMER"){
    tblList$GSINF <- tblList$GSINF[which(lubridate::month(tblList$GSINF$SDATE) %in% c(5,6,7,8)),]
    tblList$GSINF <- tblList$GSINF[tblList$GSINF$STRAT %in% c(
      "440", "441", "442", "443", "444", "445", "446", "447", "448", "449", "450", 
      "451", "452", "453", "454", "455", "456", "457", "458", "459", "460", "461", 
      "462", "463", "464", "465", "466", "470", "471", "472", "473", "474", "475", 
      "476", "477", "478", "480", "481", "482", "483", "484", "485", "490", "491", 
      "492", "493", "494", "495"),]
  }else if (survey=="SUMMER_ALL"){
    tblList$GSINF <- tblList$GSINF[which(lubridate::month(tblList$GSINF$SDATE) %in% c(5,6,7,8)),]
    tblList$GSINF <- tblList$GSINF[tblList$GSINF$STRAT %in% c("434", "436", "437", "438", "439", #1971 only, Gulf region
                                                              "440", "441", "442", "443", "444", "445", "446", "447", "448", "449", "450", 
                                                              "451", "452", "453", "454", "455", "456", "457", "458", "459", "460", "461", 
                                                              "462", "463", "464", "465", "466", "470", "471", "472", "473", "474", "475", 
                                                              "476", "477", "478", "480", "481", "482", "483", "484", "485", "490", "491", 
                                                              "492", "493", "494", "495", "496", "497", "498", "501", "502", "503", "504", 
                                                              "505", "557", "558", "559", "5Z1", "5Z2", "5Z3", "5Z4", "5Z5", "5Z6", "5Z7", 
                                                              "5Z8", "5Z9"),]
  }else if (survey== "FALL"){
    tblList$GSINF <- tblList$GSINF[which(lubridate::month(tblList$GSINF$SDATE) %in% c(9,10,11,12)),]
    
  }
  tblList<-propagateChanges(tblList = tblList, taxaAgg=F,...)
  
  if (nrow(tblList$GSMISSIONS)<1 | nrow(tblList$GSINF)<1) stop("The selected survey does not appear to have data for any of the selected years")
  tblList <- filterSpecies(tblList = tblList, ...)
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}