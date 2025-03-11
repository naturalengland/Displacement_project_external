# Define a function to generate Outside and Inside datasets, plot the Outside data, and return the datasets along with the number of VMS points
generateInOutDatasetsPlot <- function(tacsatp, intv_lower, intv_upper, ve_len_threshold) {
  tacsatp <- sortTacsat(tacsatp)
  
  Outside <- tacsatp %>%
    filter(INTV<intv_upper & FT_REF!="0"& (INTV > intv_lower | (lead(INTV > intv_lower & INTV<intv_upper & FT_REF!="0")& FT_REF==lead(FT_REF))))#need to have same FT_REF
  Outside <-Outside %>% filter(VE_LEN >= ve_len_threshold)
  
  Inside <- anti_join(tacsatp, Outside, by = c("VE_REF", "SI_LATI", "SI_LONG", "SI_TIME", "INTV"))
  
  # Plotting
  x_lim <- c(min(Outside$SI_LONG) - 0.1, max(Outside$SI_LONG) + 0.1)
  y_lim <- c(min(Outside$SI_LATI) - 0.1, max(Outside$SI_LATI) + 0.1)
  
  plot(st_geometry(europa), col = "darkgreen", xlim = x_lim, ylim = y_lim, main = "VMS points for interpolation")
  axis(1)
  axis(2)
  points(x = Outside$SI_LONG, y = Outside$SI_LATI, col = "red", pch = 19, cex = 0.5)
  
  numOutsideValues <- nrow(Outside)
  
  cat("Number of VMS points for interpolation: ", numOutsideValues, "\n")
  
  # Return the datasets and the number of VMS points
  return(list(Outside = Outside, Inside = Inside, Num_VMS_Points = numOutsideValues))
}
