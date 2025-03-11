performInterpolation <- function(data, combined_results) {
  data$FM_Value <- combined_results$FM_Value[match(data$LE_GEAR, combined_results$LE_GEAR)]
  gear_types <- unique(data$LE_GEAR)
  results <- data.frame()
  
  for (gear in gear_types) {
    subsetData <- data[data$LE_GEAR == gear, ]
    fm_interval <- subsetData[1, 15] # Using the fm value based on gear type
    interpolationcHs <- interpolateTacsat(subsetData, interval = 120, margin = 10, res = 100, method = "cHs", params = list(fm = fm_interval, distscale = 20, sigline = 0.2, st = c(2, 6)), headingAdjustment = 0)
    
    subsetData$INTV <- 0
    subsetData$LE_SURF <- 0
    subsetData$LE_SUBSURF <- 0
    
    outsideInt <- interpolation2Tacsat(interpolationcHs, subsetData, npoints = 39)
    results <- rbind(results, outsideInt)
  }
  return(results)
}
