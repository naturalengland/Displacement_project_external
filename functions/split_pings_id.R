split_data_id <- function(tacsat, eflalo) {
  # Attach each point to an ICES rectangle for plotting
  tacsat$LE_RECT <- ICESrectangle(tacsat)
  
  # Merge the data
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  
  tacsatEflalo      <- splitAmongPings(tacsat=tacsat,eflalo=eflalo,
                        variable="all",level=c("day","ICESrectangle","trip"),conserve=F)# if looking at individual fishers, remove the "conserved" landings
  # Calculate total value and total weight
  tacsatEflalo$LE_TOT_VAL <- rowSums(tacsatEflalo[grep("LE_EURO_",names(tacsatEflalo))],na.rm=T)# recalculate total value per ping
  tacsatEflalo$LE_TOT_KG <- rowSums(tacsatEflalo[grep("LE_KG_", names(tacsatEflalo))], na.rm = TRUE)
  
  # Assigning total catch weight and value to the dataset
  tacsatEflalo$TIME <- 3/60 # as there should be 3 minutes between pings each point given 3 minutes to calculate time spent fishing, convert to hrs.
  tacsatEflalo$SI_YEAR <- as.factor(year(tacsatEflalo$SI_DATIM))#adding column for year
  return(tacsatEflalo)
}


