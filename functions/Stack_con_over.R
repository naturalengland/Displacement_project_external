Stack_con_over <- function(tacsatEflalo, Hab_Type,Confidence) {
  Hab_Type$Confidence <-as.numeric(Hab_Type$Confidence)
  Hab_Type$Confidence[is.na(Hab_Type$Confidence)] <- 0
  tacsatEflalo <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
  st_crs(tacsatEflalo) <- 4326
  idx     <- st_over(tacsatEflalo,Hab_Type)
  tacsatEflalo$Confidence <- Hab_Type$Confidence[idx]
  tacsatEflalo$Confidence[is.na(tacsatEflalo$Confidence)] <- 0
  return(list( tacsatEflalo=tacsatEflalo))
}

