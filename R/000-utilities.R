where_now <- function (callstack = sys.calls()) 
{
  clean_where <- function(x) {
    val <- sapply(x, function(xt) {
      z <- strsplit(paste(xt, collapse = "\t"), "\t")[[1]]
      switch(z[1], lapply = z[3], sapply = z[3], do.call = z[2], 
             `function` = "FUN", source = "###", eval.with.vis = "###", 
             z[1])
    })
    val[grepl("\\<function\\>", val)] <- "FUN"
    val <- val[!grepl("(###|FUN)", val)]
    val <- utils::head(val, -1)
    paste(val, collapse = "|")
  }
  cs <- callstack
  cs <- clean_where(cs)
  return(cs)
}

st_err <- function (x = NULL, na.rm=T) {
  x<-x[!is.na(x)]
  stats::sd(x)/sqrt(length(x))
}
combine_lists <- function(primary = NULL, ancilliary = NULL){ 
  new <- ancilliary[setdiff(names(ancilliary),names(primary))]
  # discarded <- ancilliary[intersect(names(ancilliary),names(primary))]
  kept <- c(primary, new)
  return(kept)
}


sexifyNames <- function(df, desc=NULL){
  colnames(df) <- gsub(pattern = "^0_", paste0("UNKN_",desc,"_"), colnames(df))
  colnames(df) <- gsub(pattern = "^1_", paste0("MALE_",desc,"_"), colnames(df))
  colnames(df) <- gsub(pattern = "^2_", paste0("FEMALE_",desc,"_"), colnames(df))
  colnames(df) <- gsub(pattern = "^9_", paste0(desc,"_"), colnames(df))
  return(df)
}


unSexifyNames <- function(df, desc=NULL){
  colnames(df) <- gsub(pattern = paste0("^UNKN_",desc,"_"), paste0("0_"), colnames(df))
  colnames(df) <- gsub(pattern = paste0("^MALE_",desc,"_"), paste0("1_"), colnames(df))
  colnames(df) <- gsub(pattern = paste0("^FEMALE_",desc,"_"), paste0("2_"), colnames(df))
  colnames(df) <- gsub(pattern = paste0("^",desc,"_"), paste0("9_"), colnames(df))
  return(df)
}

set_defaults <- function(debug = FALSE, 
                         quiet = TRUE,
                         survey=NULL,
                         years= NULL,
                         type1TowsOnly = TRUE,
                         keep_nullsets= TRUE, 
                         towDist = 1.75, 
                         bySex=F, 
                         useBins = F,
                         code = NULL, 
                         aphiaid = NULL, 
                         taxa= NULL, 
                         taxaAgg = FALSE, ...){
  defaults <- as.list(environment())
  defaults[["tblList"]] <- NULL
  sentArgs <- list(...)
  #ensure hardcoded args take priority over user args
  submittedArgs <- combine_lists(primary = sentArgs$argsFn, ancilliary = sentArgs$argsUser)
  #ensure submitted args take priority over default args
  argg <- combine_lists(primary =  submittedArgs, ancilliary = defaults)
  if(!is.null(argg$taxa))argg$taxa<- toupper(argg$taxa)
  return(argg)
}

expandDF <- function(templateDF = NULL, keyFields = NULL, expandField= NULL, expandVals = NULL){
  #This function takes a df and repeats some key fields for each value found in expandVals
  if (is.null(expandField))stop("expandField must have a value")
  newDF<- unique(templateDF[,keyFields])
  newDF <- merge(unique(newDF), expandVals)
  colnames(newDF)[colnames(newDF)=="y"] <- expandField
  return(newDF)
}



