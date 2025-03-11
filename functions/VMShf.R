# Function to calculate optimal FM value for each gear type
calculateOptimalFM <- function(VMShf_data) {
  gear_types <- unique(VMShf_data$LE_GEAR)
  combined_results <- data.frame()
  
  for (gear in gear_types) {
    subsetVMShf <- VMShf_data[VMShf_data$LE_GEAR == gear, ]
    subsetVMShf <- intervalTacsat(subsetVMShf, level = "vessel", fill.na = TRUE)
    subsetVMShf$INTVCUM <- cumsum(subsetVMShf$INTV)
    VMSlf <- subsetVMShf[which(subsetVMShf$INTVCUM %% 120 == 0), ]
    subsetVMShf <- subsetVMShf[which(subsetVMShf$INTVCUM %% 120 == 0)[1]:nrow(subsetVMShf), ]
    
    tunecHsInterpolation <- function(lowResTacsat, hiResTacsat, interval, margin, res, headingAdjustment = 0, st = c(2, 6)) {
      optimInterpolation <- function(x) {
        ints <- interpolateTacsat(hiResTacsat,
                                  interval = interval,
                                  margin = margin,
                                  res = res,
                                  method = "cHs",
                                  params = list(fm = x, distscale = NA, signline = NA, st = st),
                                  headingAdjustment = headingAdjustment)
        
        diffs <- diffInter(ints, hiResTacsat)[,"mean"]
        pointsToRemove <- which(diffs <= quantile(diffs, probs = c(0.01), na.rm = TRUE) | diffs >= quantile(diffs, probs = c(0.99), na.rm = TRUE))
        meddiffs <- median(diffs[-pointsToRemove], na.rm = TRUE)
        
        return(meddiffs)
      }
      
      res <- optimize(optimInterpolation, interval = c(0, 1), tol = 0.0005)$minimum
      return(res)
    }
    
    result <- tunecHsInterpolation(lowResTacsat = VMSlf, hiResTacsat = subsetVMShf, interval = 120, margin = 3, res = 100, headingAdjustment = 1, st = c(2, 6))
    
    result_df <- data.frame(LE_GEAR = gear, FM_Value = result)
    combined_results <- rbind(combined_results, result_df)
  }
  
  return(combined_results)
}

# Usage of the function with your dataset VMShf
# Assuming VMShf is your initial dataset
# Call the function passing VMShf to get the results
