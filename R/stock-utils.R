#If strata includes 5z3, 5z4, 5z8, 5Z5, 5z6, 5z7, likely should include areas 523 and 524
stock_map <- list(
  GAMO_4Vn							              = list(code=  10, months = c(5,6,7,8),	strata = c(440:442)),
  GAMO_4VsW							              = list(code=  10, months = c(5,6,7,8),	strata = c(443:466)),
  GAMO_4X5Y							              = list(code=  10, months = c(5,6,7,8),	strata = c(470:495)),
  GAMO_5Zjm 						              = list(code=  10, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  MEAE_4X5Y							              = list(code=  11, months = c(5,6,7,8),	strata = c(470:495)),
  MEAE_4TVW							              = list(code=  11, months = c(5,6,7,8),	strata = c(440:466)),
  MEAE_5Zjm							              = list(code=  11, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  URTE_4VW							              = list(code=  12, months = c(5,6,7,8),	strata = c(440:466)),
  URTE_4X5Zc						              = list(code=  12, months = c(5,6,7,8),	strata = c(470:495)),
  MEBI_4VWX							              = list(code=  14, months = c(5,6,7,8),	strata = c(440:483)),
  MEBI_4Xwest						              = list(code=  14, months = c(5,6,7,8),	strata = c(484:495)),
  POVI_4VWXmn						              = list(code=  16, months = c(5,6,7,8),	strata = c(440:473, 475, 477, 478)),
  POVI_4X5							              = list(code=  16, months = c(5,6,7,8),	strata = c(474, 476, 480:495,"5Z1", "5Z2", "5Z9")),
  SEFA_4VW							              = list(code=  23, months = c(5,6,7,8),	strata = c(440:456, 464)),
  SEFA_Un3							              = list(code=  23, months = c(5,6,7,8),	strata = c(456, 458:495)),
  HIHI_3NOPs4VWX_5			              = list(code=  30, months = c(5,6,7,8),	strata = c(440:466,470:495)),
  HIPL_4X								              = list(code=  40, months = c(5,6,7,8),	strata = c(470:495)),
  HIPL_4VW							              = list(code=  40, months = c(5,6,7,8),	strata = c(440:466)),
  GLCY_4X								              = list(code=  41, months = c(5,6,7,8),	strata = c(470:495)),
  GLCY_4VW							              = list(code=  41, months = c(5,6,7,8),	strata = c(440:466)),
  PSAM_4X								              = list(code=  43, months = c(5,6,7,8),	strata = c(470:495)),
  PSAM_4VW							              = list(code=  43, months = c(5,6,7,8),	strata = c(440:466)),
  LIFE_4X								              = list(code=  42, months = c(5,6,7,8),	strata = c(470:495)),
  LIFE_4VW							              = list(code=  42, months = c(5,6,7,8),	strata = c(440:466)),
  LIFE_5Z								              = list(code=  42, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4"), areas = c(523,524)),
  DILA_4X								              = list(code= 200, months = c(5,6,7,8),	strata = c(470:495)),
  DILA_4VW							              = list(code= 200, months = c(5,6,7,8),	strata = c(440:466)),
  DILA_5Z								              = list(code= 200, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  LEER_4X								              = list(code= 203, months = c(5,6,7,8),	strata = c(470:495)),
  LEER_4VW							              = list(code= 203, months = c(5,6,7,8),	strata = c(440:466)),
  LEER_5Z								              = list(code= 203, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  MASE_4X								              = list(code= 202, months = c(5,6,7,8),	strata = c(470:495)),
  MASE_4VW							              = list(code= 202, months = c(5,6,7,8),	strata = c(440:466)),
  MASE_5Z								              = list(code= 202, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  AMRA_4X								              = list(code= 201, months = c(5,6,7,8),	strata = c(470:495)),
  AMRA_4VW							              = list(code= 201, months = c(5,6,7,8),	strata = c(440:466)),
  AMRA_5Z								              = list(code= 201, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  LEOC_4X								              = list(code= 204, months = c(5,6,7,8),	strata = c(470:495)),
  LEOC_4VW							              = list(code= 204, months = c(5,6,7,8),	strata = c(440:466)),
  LEOC_5Z								              = list(code= 204, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  ANLU_4X								              = list(code=  50, months = c(5,6,7,8),	strata = c(470:495)),
  ANLU_4VW							              = list(code=  50, months = c(5,6,7,8),	strata = c(440:466)),
  LOAM_4X								              = list(code= 400, months = c(5,6,7,8),	strata = c(470:495)),
  LOAM_4VW							              = list(code= 400, months = c(5,6,7,8),	strata = c(440:466)),
  MYOC_4X								              = list(code= 300, months = c(5,6,7,8),	strata = c(470:495)),
  MYOC_4VW							              = list(code= 300, months = c(5,6,7,8),	strata = c(440:466)),
  Longhorn_Sculpin_5Z     			      = list(code= 300, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  SQA									                = list(code= 220, months = c(5,6,7,8),	strata = c(440:466, 470:495)),
  URCH_4X								              = list(code=  13, months = c(5,6,7,8),	strata = c(470:495)),
  URCH_4VW							              = list(code=  13, months = c(5,6,7,8),	strata = c(440:466)),
  HEAM_4X								              = list(code= 320, months = c(5,6,7,8),	strata = c(470:495)),
  HEAM_4VW							              = list(code= 320, months = c(5,6,7,8),	strata = c(440:466)),
  ZOAM_4X								              = list(code= 640, months = c(5,6,7,8),	strata = c(470:495)),
  ZOAM_4VW							              = list(code= 640, months = c(5,6,7,8),	strata = c(440:466)),
  Ocean_Pout_NAFO_5Z			            = list(code= 640, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  HEDA_4VWX5c							            = list(code= 123, months = c(5,6,7,8),	strata = c(440:466, 470:495)),
  ZEOC_4VWX5c							            = list(code= 704, months = c(5,6,7,8),	strata = c(440:466, 470:495)),
  ILIL_34								              = list(code=4511, months = c(5,6,7,8),	strata = c(440:466, 470:495)),
  American_Lobster 					          = list(code=2550, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524)),
  Juvenile_Little_and_Winter_Skate  	= list(code=1191, months = c(1,2,3,4),	strata = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z9"), areas = c(523,524))
)

stock_lookup <- function(stock) {
  if (!stock %in% names(stock_map)) stop("Invalid stock")
  stock_map[[stock]]
}

stock_list <- function() {
  names(stock_map)
}

stock_search <- function(pattern) {
  grep(pattern, names(stock_map), value = TRUE, ignore.case = TRUE)
}

stock_validate <- function(stock) {
  if (stock %in% names(stock_map)) {
    return(stock_map[[stock]])
  }
  closest_matches <- agrep(stock, names(stock_map), value = TRUE, max.distance = 0.2)
  if (length(closest_matches) > 0) {
    stop(paste("Invalid stock. Did you mean:", paste(closest_matches, collapse = ", ")))
  } else {
    stop("Invalid stock. No similar stock names found.")
  }
}

# ILIL_34 <- lookup_stock("ILIL_34")
# test <- loadRVData(cxn = NULL, code = ILIL_34$code, years= c(2014), months=ILIL_34$months, strata=ILIL_34$strata, types=1)
# plotRV(tblList = test, plotSets = "TOTNO")
# 
# Ocean_Pout_NAFO_5Z <- lookup_stock("Ocean_Pout_NAFO_5Z")
# test <- loadRVData(cxn = NULL, code = Ocean_Pout_NAFO_5Z$code, years= c(2014), months=Ocean_Pout_NAFO_5Z$months, strata=Ocean_Pout_NAFO_5Z$strata, areas=Ocean_Pout_NAFO_5Z$areas, types=1)
# plotRV(tblList = test, plotSets = "TOTNO")
