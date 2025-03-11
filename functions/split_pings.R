split_data <- function(tacsat, eflalo) {
  # Attach each point to an ICES rectangle for plotting
  tacsat$LE_RECT <- ICESrectangle(tacsat)
  
  # Merge the data
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  
  # Split the catch weight and value per fishing ping
  tacsatEflalo <- splitAmongPings(tacsat = tacsatp, eflalo = eflalo,
                                  variable = "all", level = c("day", "ICESrectangle", "trip"), conserve = TRUE)
  
  # Calculate total value and total weight
  tacsatEflalo$LE_TOT_VAL <- rowSums(tacsatEflalo[grep("LE_EURO_", names(tacsatEflalo))], na.rm = TRUE)
  tacsatEflalo$LE_TOT_KG <- rowSums(tacsatEflalo[grep("LE_KG_", names(tacsatEflalo))], na.rm = TRUE)
  
  # Assigning total catch weight and value to the dataset
  tacsatEflalo$TIME <- 3 # as there should be 3 minutes between pings each point given 3 minutes to calculate time spent fishing.
  
  return(tacsatEflalo)
}

