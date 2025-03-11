summarize_month <- function(data, location = NULL) {
  data$SI_MONTH <- month(data$SI_DATIM)
  data$SI_MONTH <- factor(data$SI_MONTH,
                          levels = 1:12,
                          labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  if (!is.null(location)) {
    data <- filter(data, Location == location)
  }
  
  summary_table <- data %>%
    group_by(Location, SI_MONTH) %>%
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
  freq_plot <- ggplot(summary_table, aes(x = SI_MONTH, y = Ping_Frequency, fill = Location)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(title = "Ping Frequency", y = "Ping Frequency", x="Month")+
    theme_classic()
  
  value_plot <- ggplot(summary_table, aes(x = SI_MONTH, y = Total_Value, fill = Location)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(title = "Total Value", y = "Total Value (£)", x="Month")+
    theme_classic()
  
  weight_plot <- ggplot(summary_table, aes(x = SI_MONTH, y = Total_Weight, fill = Location)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(title = "Total Weight", y = "Total Weight (KG)", x="Month")+
    theme_classic()
  
  time_plot <- ggplot(summary_table, aes(x = SI_MONTH, y = Total_Time, fill = Location)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(title = "Total Time", y = "Total Time(hrs)", x="Month")+
    theme_classic()
  
  kwh_plot <- ggplot(summary_table, aes(x = SI_MONTH, y = Total_kWh, fill = Location)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(title = "Total effort", y = "Total effort (kWh)", x="Month")+
    theme_classic()
  
  print(summary_table)
  print(freq_plot)
  print(value_plot)
  print(time_plot)
  print(weight_plot)
  print(kwh_plot)
}
