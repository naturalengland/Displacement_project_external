removeDuplicates <- function(data) {
  data$SI_DATIM <- as.POSIXct(paste(data$SI_DATE, data$SI_TIME, sep = " "), 
                              tz = "GMT", format = "%d/%m/%Y %H:%M")
  
  uniqueCombination <- paste(data$VE_REF, data$SI_LATI, data$SI_LONG, data$SI_DATIM)
  initialRowCount <- nrow(data)
  
  data <- data[!duplicated(uniqueCombination), ]
  
  duplicatesRemoved <- initialRowCount - nrow(data)
  
  message(paste("Number of duplicates removed:", duplicatesRemoved))
  
  return(data)
}


