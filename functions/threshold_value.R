threshold<- function(data, lanThres = 1.5) {
  specs <- substr(colnames(data[grep("KG", colnames(data))]), 7, 9)
  
  specBounds <- lapply(as.list(specs), function(x) {
    idx <- grep(x, colnames(data))[grep("KG", colnames(data)[grep(x, colnames(data))])]
    wgh <- sort(unique(data[which(data[, idx] > 0), idx]))
    difw <- diff(log10(wgh))
    ifelse(any(difw > lanThres), wgh[rev(which(difw <= lanThres) + 1)], 
           ifelse(length(wgh) == 0, 0, max(wgh, na.rm = TRUE)))
  })
  
  specBounds <- cbind(specs, unlist(specBounds))
  specBounds[which(is.na(specBounds[, 2]) == TRUE), 2] <- "0"
  
  idx <- unlist(lapply(as.list(specs), function(x) {
    idx <- grep(x, colnames(data))[grep("KG", colnames(data)[grep(x, colnames(data))])]
    return(idx)
  }))
  
  n_turned_to_na <- 0
  n_removed_na <- 0
  
  for (iSpec in idx) {
    exceeding_vals <- which(data[, iSpec] > as.numeric(specBounds[(iSpec - idx[1] + 1), 2]))
    n_turned_to_na <- n_turned_to_na + length(exceeding_vals)
    data[exceeding_vals, iSpec] <- NA
  }
  
  for (i in kgeur(colnames(data))) {
    na_indices <- which(is.na(data[, i]) == TRUE)
    n_removed_na <- n_removed_na + length(na_indices)
    data[na_indices, i] <- 0
  }
  
  cat(paste("Number of weights above threshold:", n_turned_to_na, "\n"))
  cat(paste("Number of NAs removed:", n_removed_na, "\n"))
  
  return(list(
    data_with_nas_fixed = data,
    n_values_turned_to_na = n_turned_to_na,
    n_nas_removed = n_removed_na
  ))
}
