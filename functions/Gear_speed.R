gear_speeds <- function(eflalo, tacsat) {
  cat("Number of rows where Speed is NA:", length(which(is.na(tacsat$SI_SP))), "\n")
  tacsat <- tacsat[!is.na(tacsat$SI_SP), ]
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  cat("Number of rows where Fishing trip reference is 0:", length(which(tacsatp$FT_REF == 0)), "\n")
  tacsatp$VE_KW <- eflalo$VE_KW[match(tacsatp$FT_REF, eflalo$FT_REF)]
  
  result <- tacsatp %>%
    group_by(LE_GEAR) %>%
    tally()
  
  # Create a list to store plots
  plots_list <- list()
  
  # Generate histograms for SI_SP for each gear type
  for (gear_type in unique(tacsatp$LE_GEAR)) {
    gear_data <- tacsatp[tacsatp$LE_GEAR == gear_type, ]
    plot <- gear_data %>%
      ggplot(aes(x = SI_SP)) +
      geom_histogram(binwidth = 0.5, fill = "lightgrey", color = "black") +  # Adjust binwidth to 15 bars per plot
      labs(title = paste("Speed Distribution for", gear_type), xlab= "Speed (knots)")+
      theme_classic()
    
    plots_list[[gear_type]] <- plot
  }
  
  return(list(tally = result, plots = plots_list))
}
