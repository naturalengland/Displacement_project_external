remove_Duplicate_eflalo <- function(data, column_name) {
  initial_records <- nrow(data)
  
  cleaned_data <- data[!duplicated(data[[column_name]]), ]
  
  removed_records <- initial_records - nrow(cleaned_data)
  
  cat("Number of duplicates removed:", removed_records, "\n")
  
  return(cleaned_data)
}
