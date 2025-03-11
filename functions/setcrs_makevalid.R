# Define the convert_and_set_crs function with st_make_valid
convert_and_set_crs <- function(site, crs = 4326) {
  site_sfc <- st_as_sfc(list(site))
  st_crs(site_sfc) <- crs
  site_sfc_valid <- st_make_valid(site_sfc)
  return(site_sfc_valid)
}


transform_and_make_valid <- function(data, crs = 4326) {
  data <- st_transform(data, crs)
  data_valid <- st_make_valid(data)
  return(data_valid)
}
