
plot_speed_histogram <- function(data, h = 2, f = 2, s = 6) {
  # Remove rows with NA values in the SI_SP column
  data <- na.omit(data)
  
  # Assign letters for each SI_STATE
  data$SI_STATE <- ifelse(data$SI_SP < h, "h",
                          ifelse(data$SI_SP >= f & data$SI_SP < s, "f", "s"))
  
  # Plotting the frequency of each state
  p <- ggplot(data, aes(x=SI_SP, fill=SI_STATE)) +
    geom_histogram(color="black", binwidth = 0.5) +
    scale_fill_manual(values = c("h" = "red", "f" = "blue", "s" = "green")) +
    labs(title = "Frequency of Speed States", x = "Speed (SI_SP)", y = "Frequency") +
    theme_minimal()
  
  # Print the plot
  print(p)
  
  # Return a list containing the modified data and the plot object
  return(list(data = data, plot = p))
}
#removes speeds of 0, assigns speeds for floating, fishing and steaming and plots a histogram



