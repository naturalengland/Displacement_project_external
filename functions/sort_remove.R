sort_remove <- function(tacsat, eflalo) {
  tacsat <- sortTacsat(tacsat)
  tacsatp <- mergeEflalo2Tacsat(eflalo, tacsat)
  not_linked_count <- length(which(tacsatp$FT_REF == 0))
  tacsatp$VE_LEN <- eflalo$VE_LEN[match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_GEAR <- eflalo$LE_GEAR[match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_MET <- eflalo$LE_MET[match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatp$FT_REF, eflalo$FT_REF)]
  tacsatp <- tacsatp %>% filter(FT_REF != "0")
  
  cat("Number of records not able to link:", not_linked_count, "\n")
  return(tacsatp)
}
