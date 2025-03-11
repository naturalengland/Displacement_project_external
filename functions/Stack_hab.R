
Stack_hab <- function(five_site, ten_hole, twenty_hole, boundary_hole, order_locations) {

  # Finding polygon intersections
  Hab_five <- st_intersection(Hab_Type, five_site)
  Hab_ten <- st_intersection(Hab_Type, ten_hole)
  Hab_twenty <- st_intersection(Hab_Type, twenty_hole)
  Hab_boundary <- st_intersection(Hab_Type, boundary_hole)
  
  
  Hab_five <- st_transform( Hab_five, crs = 3035)
  Hab_ten <- st_transform(Hab_ten, crs = 3035)
  Hab_twenty <- st_transform(Hab_twenty, crs = 3035)#set crs to standard
  Hab_boundary<- st_transform(Hab_boundary, crs = 3035)
  Hab_Type<- st_transform(Hab_Type, crs = 3035)
  
  # Calculating areas
  Hab_five$Area <- st_area(Hab_five)
  Hab_ten$Area <- st_area(Hab_ten)
  Hab_twenty$Area <- st_area(Hab_twenty)
  Hab_boundary$Area <- st_area(Hab_boundary)
  Hab_Type$Area <- st_area(Hab_Type)
  
  # Aggregate areas
  five_area <- Hab_five[, c("HAB_TYPE", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ HAB_TYPE, data = ., FUN = sum)
  ten_area <- Hab_ten[, c("HAB_TYPE", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ HAB_TYPE, data = ., FUN = sum)
  twenty_area <- Hab_twenty[, c("HAB_TYPE", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ HAB_TYPE, data = ., FUN = sum)
  boundary_area <- Hab_boundary[, c("HAB_TYPE", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ HAB_TYPE, data = ., FUN = sum)
  Outside_area <- Hab_Type[, c("HAB_TYPE", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ HAB_TYPE, data = ., FUN = sum)
  
  # Calculate differences in areas
  ten_area_new <-replace(ten_area, is.na(ten_area), 0)

  twenty_area_new <- replace(twenty_area, is.na(twenty_area), 0)

  boundary_area_new <- replace(boundary_area, is.na(boundary_area), 0)


  Outside_area_new <- replace(Outside_area, is.na(Outside_area), 0)

  # Merge dataframes
  ten_area_new <- ten_area_new[, c("HAB_TYPE", "Area")]
  twenty_area_new <- twenty_area_new[, c("HAB_TYPE", "Area")]
  boundary_area_new <- boundary_area_new[, c("HAB_TYPE", "Area")]
  Outside_area_new <- Outside_area_new[, c("HAB_TYPE", "Area")]
  
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
 
   Hab_final$HAB_TYPE <- ifelse(Hab_final$HAB_TYPE %in% c("A5.35", "A5.44"), Hab_final$HAB_TYPE, "Other")
   Hab_final<- subset(Hab_final, Location != "Outside")

  # Plotting
  plot <- ggplot(Hab_final, aes(fill = HAB_TYPE, y = Area, x = Location)) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = "Habitat type", values = c("A5.35" = "#440154",
                                                        "A5.44" = "#1F9E89","Other" = "#FDE725"), labels = c("A5.35", "A5.44", "Other")) +
    ggtitle("Habitat type: EUNIS Classification")+
    xlab("Location") +
    ylab("Percentage area (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    theme_classic()
  
  return(list(plot = plot, Hab_final=Hab_final))
}


