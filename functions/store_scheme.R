analyzeTacsatData <- function(eflalo, tacsat) {
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  # NA's cannot be a gear type, so we assign them as 'NO_GEAR'
  tacsatp$YEAR <- format(tacsatp$SI_DATIM, format='%Y')
  tacsatp$LE_GEAR[which(is.na(tacsatp$LE_GEAR) == TRUE)] <- "NO_GEAR"
  
  # Analyze the data
  storeScheme <- activityTacsatAnalyse(tacsatp, units = "year", identify = "means", analyse.by = "LE_GEAR")
  
  return(storeScheme)
}


