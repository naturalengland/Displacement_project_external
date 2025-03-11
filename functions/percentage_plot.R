plotISLA_generalized <- function(data, VarList, NameList, location_order, breaks = c(-0.1, 0, 10, 20, 30, 40, 50, 75, 100)) {
  results <- list()
  
  for (i in seq_along(VarList)) {
    Var <- VarList[[i]]
    Name <- NameList[[i]]
    
    # Calculate the proportion of the variable
    data <- within(data, {
      valPerc <- get(Var) / ave(get(Var), VE_REF, SI_YEAR, FUN = sum) * 100
    })
    
    # Aggregate based on Location, LE_GEAR, and VE_REF
    IndAnalysis <- aggregate(data["valPerc"], 
                             by = list(Location = factor(data$Location, levels = location_order), 
                                       LE_GEAR = data$LE_GEAR, 
                                       VE_REF = data$VE_REF), 
                             FUN = sum, na.rm = TRUE)
    
    # Define labels for bins
    labs <- c("0", paste(">", breaks[2:(length(breaks) - 2)], "-", breaks[3:(length(breaks) - 1)], sep = ""), 
              paste(">", breaks[length(breaks) - 1], sep = ""))
    
    # Create the plot
    plot <- ggplot(IndAnalysis, aes(x = cut(valPerc, breaks = breaks, labels = labs), fill = Location)) +
      geom_bar(colour = 'black', position = position_dodge(width = 0.9)) +
      theme_bw() + 
      xlab(paste('% of ', Name, ' depending on location')) + 
      ylab('Number of vessels') + 
      theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, vjust = .5))
    
    # Print the plot
    print(plot)
    
    # Save the aggregated data
    results[[Name]] <- IndAnalysis
  }
  
  return(results)
}
