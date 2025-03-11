
data("tacsat")
points_on_globe <- function(data) {
  idx_lat_long <- which(abs(data$SI_LATI) > 90 | abs(data$SI_LONG) > 180)
  idx_heading <- which(data$SI_HE < 0 | data$SI_HE > 360)
  
  idx_combined <- unique(c(idx_lat_long, idx_heading))
  
  # Count and Remove Outliers
  num_outliers <- length(idx_combined)
  if (num_outliers > 0) {
    data_cleaned <- data[-idx_combined, ]
    message(paste("Removed", num_outliers, "outliers."))
    return(data_cleaned)
  } else {
    message("All points on globe")
    return(data)
  }
}


