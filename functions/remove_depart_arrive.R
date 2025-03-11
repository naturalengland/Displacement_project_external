filterDateTime <- function(data) {
  initial_row_count <- nrow(data)  # Get initial row count
  
  # Create date-time stamp for departure date
  data$FT_DDATIM <- as.POSIXct(paste(data$FT_DDAT, data$FT_DTIME, sep=" "), tz="GMT", format="%d/%m/%Y %H:%M")
  
  # Create date-time stamp for landing date
  data$FT_LDATIM <- as.POSIXct(paste(data$FT_LDAT, data$FT_LTIME, sep=" "), tz="GMT", format="%d/%m/%Y %H:%M")
  
  # Filter rows where departure date-time is not greater than landing date-time
  filtered_data <- data[data$FT_DDATIM <= data$FT_LDATIM, ]
  
  rows_removed <- initial_row_count - nrow(filtered_data)  # Calculate removed rows
  
  cat("Number of arrival dates before departure dates:", rows_removed, "\n")
  
  # Remove FT_DDATIM and FT_LDATIM columns
  filtered_data$FT_DDATIM <- NULL
  filtered_data$FT_LDATIM <- NULL
  
  return(filtered_data)
}


