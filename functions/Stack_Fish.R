Stack_Fish <- function(spawning, five_site, ten_site, twenty_site, boundary, order_locations, Title, Heading) {
  
  # Finding polygon intersections
  Fish_five <- st_intersection(Fish_hotspot, five_site)
  Fish_ten <- st_intersection(Fish_hotspot, ten_hole)
  Fish_twenty <- st_intersection(Fish_hotspot, twenty_hole)
  Fish_boundary <- st_intersection(Fish_hotspot, boundary_hole)
  
  Fish_five <- st_transform( Fish_five, crs = 3035)
  Fish_ten <- st_transform(Fish_ten, crs = 3035)
  Fish_twenty <- st_transform(Fish_twenty, crs = 3035)#set crs to standard
  Fish_boundary<- st_transform(Fish_boundary, crs = 3035)
  Fish_hotspot<- st_transform(Fish_hotspot, crs = 3035)
  
  # Calculating areas
  Fish_five$Area <- st_area(Fish_five)
  Fish_ten$Area <- st_area(Fish_ten)
  Fish_twenty$Area <- st_area(Fish_twenty)
  Fish_boundary$Area <- st_area(Fish_boundary)
  Fish_hotspot$Area <- st_area(Fish_hotspot)
  # Replace NAs with 0s
  Fish_five$Area[is.na(Fish_five$Area)] <- 0
  Fish_ten$Area[is.na(Fish_ten$Area)] <- 0
  Fish_twenty$Area[is.na(Fish_twenty$Area)] <- 0
  Fish_boundary$Area[is.na(Fish_boundary$Area)] <- 0
  Fish_hotspot$Area[is.na(Fish_hotspot$Area)] <- 0
  # Aggregate areas
  five_area <- Fish_five[, c(spawning, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ spawning, data = ., FUN = sum)
  ten_area <- Fish_ten[, c(spawning, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ spawning, data = ., FUN = sum)
  twenty_area <- Fish_twenty[, c(spawning, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ spawning, data = ., FUN = sum)
  boundary_area <- Fish_boundary[, c(spawning, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ spawning, data = ., FUN = sum)
  Outside_area <- Fish_hotspot[, c(spawning, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ spawning, data = ., FUN = sum)
  
  # Calculate differences in areas

  ten_area_new <-replace(ten_area, is.na(ten_area), 0)

  twenty_area_new <- replace(twenty_area, is.na(twenty_area), 0)

  boundary_area_new <- replace(boundary_area, is.na(boundary_area), 0)

  Outside_area_new <- replace(Outside_area , is.na(Outside_area), 0)

  # Merge dataframes
  ten_area_new <- ten_area_new[, c(spawning, "Area")]
  twenty_area_new <- twenty_area_new[, c(spawning, "Area")]
  boundary_area_new <- boundary_area_new[, c(spawning, "Area")]
  Outside_area_new <- Outside_area_new[, c(spawning, "Area")]
  
  five_area$Location <- "Five"
  ten_area_new$Location <- "Ten"
  twenty_area_new$Location <- "Twenty"
  boundary_area_new$Location <- "Boundary"
  Outside_area_new$Location <- "Outside"
  
  Hab_final <- rbind(five_area, ten_area_new, twenty_area_new, boundary_area_new, Outside_area_new)
  
  # Filter out negative areas
  Hab_final$Area[Hab_final$Area < 0] <- 0
  
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    Hab_final$Location <- factor(Hab_final$Location, levels = order_locations)
  }
  
  assign_scale <- function(spawning) {
    spawning <- ifelse(spawning >= 0 & spawning <= 3, "0-3",
                       ifelse(spawning >= 4 & spawning <= 6, "4-6",
                              ifelse(spawning >= 7 & spawning <= 9, "7-9",
                                     ifelse(spawning >= 10 & spawning <= 12, "10-12",
                                            ifelse(spawning >= 13 , ">12","0-3")))))
    return(spawning)
  }
  Hab_final$Spawning <- assign_scale(Hab_final$spawning)
  Hab_final$Spawning <- factor(Hab_final$Spawning, levels = c("0-3", "4-6", "7-9","10-12",">12"))
  Hab_final <- subset(Hab_final, Location != "Outside")#don't want to plot outside
  # Plotting
  plot <- ggplot(Hab_final, aes(fill = Spawning, y = Area, x = Location)) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = Heading, values = c("0-3" = "#440154",
                                                 "4-6" = "#3b528b",
                                                 "7-9" = "#21918c",
                                                 "10-12" = "#5ec962",         
                                                 ">12" = "#FDE725"))+
    scale_color_manual(name = Heading, values = c("0-3" = "#440154",
                                                 "4-6" = "#3b528b",
                                                 "7-9" = "#21918c",
                                                 "10-12" = "#5ec962",         
                                                 ">12" = "#FDE725"))+
    ggtitle(Title)+
    xlab("Location") +
    ylab("Percentage area (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    theme_classic()
  
  return(list(plot = plot, Hab_final=Hab_final))
}