calculate_swept_area <- function(site1, site2, site3, boundary, location1, location2, location3, boundary_location, result_data) {
  swept <- aggregate(TR_AREA ~ Location, data = result_data$coords, FUN = sum)
  site1 <- st_transform( site1, crs = 3035)
  site2 <- st_transform( site2, crs = 3035)
  site3 <- st_transform( site3, crs = 3035)
  boundary <- st_transform(boundary, crs = 3035)
  
  swept$site_area <- ifelse(swept$Location == location1, st_area(site1),
                            ifelse(swept$Location == location2, st_area(site2),
                                   ifelse(swept$Location == location3, st_area(site3),
                                          ifelse(swept$Location == boundary_location, sum(st_area(boundary)), NA))))
  
  swept <- swept %>%
    mutate(site_area = site_area / 1000000) %>%
    mutate(swept_area_ratio = TR_AREA / site_area)
  
  return(swept)
}

