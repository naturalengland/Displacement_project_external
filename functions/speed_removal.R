removeSpeedsAboveThreshold <- function(data, threshold) {
  speeds_before <- length(data$SI_SP)
  
  # Identify indices of speeds above the threshold
  idx <- which(data$SI_SP > threshold)
  
  # Remove speeds above the threshold
  data_cleaned <- data[-idx, ]
  
  # Plot histogram of all speeds
  hist(data$SI_SP, breaks = 100, main = "Histogram of Speeds", xlab = "Speed", ylab = "Frequency")
  
  # Inform about the number of speeds removed
  speeds_removed <- speeds_before - length(data_cleaned$SI_SP)
  cat(paste("Number of speeds removed above", threshold, "knots:", speeds_removed, "\n"))
  
  return(data_cleaned)
}

