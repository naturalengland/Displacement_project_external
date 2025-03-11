Location_column <- function(data, ping_columns) {
  for (col in ping_columns) {
    data[[col]] <- !is.na(get(col))
  }
  
  data$Location <- apply(data[ping_columns], 1, function(x) {
    first_ping <- which(x)[1] # Find the index of the first TRUE value
    if (length(first_ping) == 0) {
      return("Outside") # If no TRUE value found, return "Outside"
    } else {
      return(sub("_Pings", "", ping_columns[first_ping])) # Extract the name without "_Pings"
    }
  })
  
  # Change NA values in Location column to "Outside"
  data$Location[is.na(data$Location)] <- "Outside"
  
  # Remove columns containing TRUE/FALSE values
  data <- data[, !grepl("_Pings", colnames(data))]
  
  # Plot bar chart
  location_counts <- table(data$Location)
  location_counts_df <- as.data.frame(location_counts)
  location_counts_df$Var1 <- factor(location_counts_df$Var1, levels = location_counts_df$Var1[order(location_counts_df$Freq)])
  
  p <- ggplot(location_counts_df, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity") +
    labs(x = "Location", y = "Ping frequency", title = "Location of pings") +
    theme_classic() +  # Apply classic theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  print(p)  # Print the plot
  
  return(data)  # Return the modified data frame
}

