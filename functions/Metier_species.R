eflalo_met <- function(eflalo_data, metier_data) {
  
  eflalo2 <- eflalo_data
  
  # Find columns containing 'LE_EURO_' in the heading
  le_columns <- grep("LE_EURO_", names(eflalo2), value = TRUE)
  second_max_col <- function(row) {
    sorted_values <- sort(row, decreasing = TRUE)
    second_max_value <- sorted_values[2]
    if (is.na(second_max_value)) {
      return(NA)
    } else {
      return(names(row)[which(row == second_max_value)])
    }
  }
  eflalo2 <- eflalo2 %>%
    mutate(
      top_species = substr(le_columns[apply(eflalo2[le_columns], 1, which.max)], nchar(le_columns[1]) - 2, nchar(le_columns[1])),
      second_top_species = sapply(apply(eflalo2[le_columns], 1, second_max_col), function(x) ifelse(is.na(x), NA, substr(x, nchar(le_columns[1]) - 2, nchar(le_columns[1]))))
    ) %>%
    filter(rowSums(.[le_columns]) != 0)  # Remove rows where total of le_value columns is 0
  
  # Function to find the largest and second largest values in a row for specified columns
  find_largest_and_second_largest <- function(row, cols) {
    sorted_row <- sort(row[cols], decreasing = TRUE)
    return(c(largest_val = sorted_row[1], second_largest_val = sorted_row[2]))
  }
  
  # Apply the function to each row
  result <- apply(eflalo2[, le_columns], 1, find_largest_and_second_largest, cols = le_columns)
  
  # Convert the result to a dataframe
  result_df <- t(as.data.frame(result))
  colnames(result_df) <- c("largest_val", "second_largest_val")
  
  # Combine the result with the original dataframe
  eflalo2 <- cbind(eflalo2, result_df)
  
  eflalo2 <- eflalo2[eflalo2$largest_val != 0, ]  # Remove rows where largest value is 0
  
  eflalo2$metier1 <- metier_data$Type[match(eflalo2$top_species, metier_data$Species_code)]
  eflalo2$metier2 <- metier_data$Type[match(eflalo2$second_top_species, metier_data$Species_code)]
  
  # Define function to determine metier
  determine_metier <- function(row) {
    ifelse(is.na(row$metier2), row$metier1,
           ifelse(row$metier1 == row$metier2, row$metier1,
                  ifelse(row$largest_val >= 1.1 * row$second_largest_val, row$metier1, 'MIX')))
  }
  
  # Apply the function to create the 'metier' column
  eflalo2$metier <- apply(eflalo2, 1, determine_metier)
  
  assign_size <- function(LE_GEAR, VE_LEN) {
    size <- ifelse(LE_GEAR == "DRB" & VE_LEN < 10, "SMA",
            ifelse(LE_GEAR == "DRB" & VE_LEN >= 10 & VE_LEN <= 18, "MED",
            ifelse(LE_GEAR == "DRB" & VE_LEN > 18, "LAR",
            ifelse(LE_GEAR == "OTB" & VE_LEN < 12, "SMA",
            ifelse(LE_GEAR == "OTB" & VE_LEN >= 12 & VE_LEN <= 18, "MED",
            ifelse(LE_GEAR == "OTB" & VE_LEN > 18, "LAR",  
            ifelse(LE_GEAR == "OTT" & VE_LEN < 12, "SMA",
            ifelse(LE_GEAR == "OTT" & VE_LEN >= 12 & VE_LEN <= 18, "MED",
            ifelse(LE_GEAR == "OTT" & VE_LEN > 18, "LAR",
            ifelse(LE_GEAR == "PTB" & VE_LEN < 12, "SMA",
            ifelse(LE_GEAR == "PTB" & VE_LEN >= 12 & VE_LEN <= 15, "MED",
            ifelse(LE_GEAR == "PTB" & VE_LEN > 15, "LAR",
            ifelse(LE_GEAR == "TBB" & VE_LEN < 12, "SMA",
            ifelse(LE_GEAR == "TBB" & VE_LEN >= 12 & VE_LEN <= 18, "MED",
            ifelse(LE_GEAR == "TBB" & VE_LEN > 18, "LAR",
            "NON")))))))))))))))
    return(size)
  }
  
  
  
  eflalo2$size_threshold <- assign_size(eflalo2$LE_GEAR, eflalo2$VE_LEN)
  
  # Function to determine direction based on the condition you provided
  determine_direction <- function(value) {
    first_two_digits <- as.integer(substr(value, 1, 2))
    ifelse(first_two_digits >= 31 | value %in% c("30E5", "29E4", "30E4"), 'Other', 'South')
  }
  
  # Apply the function to create the new column
  eflalo2 <- eflalo2 %>%
    mutate(Direction = determine_direction(LE_RECT))
  
  eflalo2$LE_MET <- paste(eflalo2$LE_GEAR, eflalo2$metier, eflalo2$size_threshold,eflalo2$Direction, sep = "_")
  
  # Plot metier frequency by gear type
  gg <- ggplot(eflalo2, aes(fill = metier, x = LE_GEAR)) +
    geom_bar(position = "fill") +
    labs(title = "Metier Frequency by Gear Type", x = "Gear Type", y = "Proportion") +
    theme_minimal() +
    theme(legend.title = element_text(face = "bold"))
  
  # Print the plot
  print(gg)
  
  # Return processed data
  eflalo2 <- subset(eflalo2, select = -c(metier1, metier2, largest_val, second_largest_val, top_species, second_top_species, size_threshold, metier, Direction))
  return(eflalo2)
}
