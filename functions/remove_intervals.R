removeIntervals <- function(data, level = "vessel", threshold_low = NULL, threshold_high = NULL) {
  # Sort the data by vessel and time
  data <- sortTacsat(data)
  data <- intervalTacsat(data, level = level, fill.na = TRUE)
  
  removed_below_threshold <- 0
  removed_above_threshold <- 0
  
  if (!is.null(threshold_low)) {
    removed_below_threshold <- sum(data$INTV < threshold_low)
    data <- data[which(data$INTV >= threshold_low), ]
    cat(paste("Number of intervals below", threshold_low, "minutes removed:", removed_below_threshold, "\n"))
  }
  
  if (!is.null(threshold_high)) {
    removed_above_threshold <- sum(data$INTV > threshold_high)
    data <- data[which(data$INTV <= threshold_high), ]
    cat(paste("Number of intervals above", threshold_high, "minutes removed:", removed_above_threshold, "\n"))
  }
  
  hist(data$INTV, breaks = 1000000, xlim = c(0, 150), main = "Histogram of Intervals", xlab = "Interval", ylab = "Frequency")
  
  return(data)
}



