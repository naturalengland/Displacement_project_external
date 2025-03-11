summarize_size <- function(data, length_threshold, location_order = NULL) {
  
  # Split vessels into different lengths
  data <- data %>%
    mutate(Vessel_Size = ifelse(VE_LEN > length_threshold, paste("Above", length_threshold, "m"), paste("Below", length_threshold, "m")))
  
  if (!is.null(location_order)) {
    data$Location <- factor(data$Location, levels = location_order)
  }
  
  summary_table <- data %>%
    group_by(Location, Vessel_Size) %>%
    summarise(
      Ping_Frequency = n(),
      Total_Value = sum(LE_TOT_VAL),
      Total_Time = sum(TIME)/60,
      Total_Weight = sum(LE_TOT_KG),
      Total_kWh = sum(kwh)
    ) %>%
    mutate(
      Total_Value = round(Total_Value, 2),
      Total_Weight = round(Total_Weight, 2),
      Total_Time = round(Total_Time, 2),
      Total_kWh = round(Total_kWh, 2)
    )
  
  print(summary_table)
  
  # Create stacked bar charts
  freq_plot <- ggplot(summary_table, aes(x = Location, y = Ping_Frequency, fill = Vessel_Size)) +
    geom_bar(stat = "identity") +
    labs(title = "Ping Frequency", y = "Ping Frequency")
  
  value_plot <- ggplot(summary_table, aes(x = Location, y = Total_Value, fill = Vessel_Size)) +
    geom_bar(stat = "identity") +
    labs(title = "Total Value", y = "Total Value (£)")
  
  weight_plot <- ggplot(summary_table, aes(x = Location, y = Total_Weight, fill = Vessel_Size)) +
    geom_bar(stat = "identity") +
    labs(title = "Total Weight", y = "Total Weight (KG)")
  
  time_plot <- ggplot(summary_table, aes(x = Location, y = Total_Time, fill = Vessel_Size)) +
    geom_bar(stat = "identity") +
    labs(title = "Total Time", y = "Total Time(hrs)")
  
  kwh_plot <- ggplot(summary_table, aes(x = Location, y = Total_kWh, fill = Vessel_Size)) +
    geom_bar(stat = "identity") +
    labs(title = "Total effort", y = "Total effort (kWh)")
  
  print(summary_table)
  print(freq_plot)
  print(value_plot)
  print(time_plot)
  print(weight_plot)
  print(kwh_plot)
}
