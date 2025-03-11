nor_dis_tacsat <- function(tacsat) {
  # Perform activityTacsat
  res <- activityTacsat(tacsat, units = "year", analyse.by = "LE_GEAR", storeScheme)
  
  # Subset tacsat to remove rows where SI_SP is NA
  tacsatSubset <- subset(tacsat, !is.na(SI_SP))
  
  # Assign the non-missing values of res to SI_STATE
  tacsatSubset$SI_STATE <- na.omit(res)
  
  # Convert character states into numeric
  tacsatSubset$SI_STATE[tacsatSubset$SI_STATE == "h"] <- 0
  tacsatSubset$SI_STATE[tacsatSubset$SI_STATE == "f"] <- 1
  tacsatSubset$SI_STATE[tacsatSubset$SI_STATE == "s"] <- 0
  
  # Plot the results
  spds <- table(tacsatSubset$SI_SP, tacsatSubset$SI_STATE)
  plot(y = spds[,1], x = an(dimnames(spds)[[1]]), type = "h", 
       ylim = range(spds), xlab = "knots", ylab = "Frequency")
  lines(y = spds[,2], x = (an(dimnames(spds)[[1]]) + 0.1), col = 2, type = "h")
  # Indicates fishing about 2-5 knots which is reasonable
  
  # Return modified tacsatSubset
  return(tacsatSubset)
}

