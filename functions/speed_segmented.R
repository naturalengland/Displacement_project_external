processTacsatData <- function(eflalo, tacsat, year) {
  # Merge datasets
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  
  # Add unique identifier column
  tacsatp$idx <- 1:nrow(tacsatp)
  
  # Segment data
  tacsatSegment <- segmentedTacsatSpeed(tacsatp[which(format(tacsatp$SI_DATIM, format = "%Y") == year), ], 
                                        units = "year", analyse.by = "VE_REF", speed = "instantaneous")
  
  # Merge segmented data back
  tacsatp <- merge(tacsatp, tacsatSegment[, c("idx", "SI_STATE")], by = "idx", all.x = TRUE)
  
  # Create contingency table
  spds <- table(tacsatp$SI_SP, tacsatp$SI_STATE)
  
  # Set up layout for multiple plots on the same page
  par(mfrow = c(1, 2))
  
  # Calculate speed
  tacsatp <- calculateSpeed(tacsatp, level = "trip", fill.na = TRUE)
  
  # Create histograms for fishing and no fishing states with limited x-axis range
  hist(subset(tacsatp, SI_STATE == "f")$SI_SPCA, breaks = 100, main = "fishing", xlab = "Knots", xlim = c(0, 20))
  hist(subset(tacsatp, SI_STATE == "nf")$SI_SPCA, breaks = 100, main = "no fishing", xlab = "Knots", xlim = c(0, 20))
  
  # Return processed dataset (if needed)
  return(tacsatp)
}



