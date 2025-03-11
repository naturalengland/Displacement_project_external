
plot_individual_VE <- function(data, location) {
  data$SI_DATE <- as.Date(data$SI_DATE, format = "%d/%m/%Y")
  data$SI_MONTH <- format(data$SI_DATE, "%m")# Make month column
  tacsatEflalo_five <- subset(data, Location == location)#filter for HypHPMA
  template <- expand.grid(VE_REF = unique(tacsatEflalo_five$VE_REF), 
                          SI_MONTH = unique(tacsatEflalo_five$SI_MONTH))
  
  summary <- tacsatEflalo_five %>%
    group_by(VE_REF, SI_MONTH) %>%
    summarise(
      total_value = sum(LE_TOT_VAL),
      total_kg = sum(LE_TOT_KG),
      total_time = sum(TIME),
      total_kwh = sum(kwh)) %>%
    # Right join with the template to fill missing combinations
    right_join(template, by = c("VE_REF", "SI_MONTH")) %>%
    # Replace NA values with 0
    mutate(across(starts_with("total_"), replace_na, replace = 0))
  summary <- summary %>%
    group_by(SI_MONTH) %>%
    summarise(
      mean_value = mean(total_value),
      se_value = sd(total_value) / sqrt(n()),
      mean_weight = mean(total_kg),
      se_weight = sd(total_kg) / sqrt(n()),
      mean_time = mean(total_time),
      se_time = sd(total_time) / sqrt(n()),
      mean_kwh = mean(total_kwh),
      se_kwh = sd(total_kwh) / sqrt(n()),
    )
  summary$SI_MONTH <- month.abb[as.numeric(summary$SI_MONTH)]
  summary$SI_MONTH <- factor(summary$SI_MONTH, levels = month.abb) 
  
  Value<-ggplot(summary, aes(x = SI_MONTH, y = mean_value)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
    labs(
      x = "Month",
      y = "Average Catch Value (£)") +
    theme_classic()
  
  Weight<-ggplot(summary, aes(x = SI_MONTH, y = mean_weight)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2) +
    labs(
      x = "Month",
      y = "Average Catch Weight (KG)") +
    theme_classic()
  
  Time<-ggplot(summary, aes(x = SI_MONTH, y = mean_time)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean_time - se_time, ymax = mean_time + se_time), width = 0.2) +
    labs(
      x = "Month",
      y = "Average time spent fishing (hrs)") +
    theme_classic()
  
  Kwh<-ggplot(summary, aes(x = SI_MONTH, y = mean_kwh)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean_kwh - se_kwh, ymax = mean_kwh + se_kwh), width = 0.2) +
    labs(
      x = "Month",
      y = "Average Effort whilst fishing (kWh)") +
    theme_classic()
  
  
  
  print(Value)
  print(Weight)
  print(Time)
  print(Kwh)
  print(summary)
}
