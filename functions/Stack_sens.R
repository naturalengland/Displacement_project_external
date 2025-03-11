
Stack_sens <- function(Hab_Type,Pressure,Title, five_site, ten_site, twenty_site, boundary, order_locations,Heading) {
  Hab_Type$Pressure <- ifelse(Hab_Type$Pressure %in% 0:8, Hab_Type$Pressure, 0)
  
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
  five_area <- Hab_five[, c("Pressure", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ Pressure, data = ., FUN = sum)
  ten_area <- Hab_ten[, c("Pressure", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ Pressure, data = ., FUN = sum)
  twenty_area <- Hab_twenty[, c("Pressure", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ Pressure, data = ., FUN = sum)
  boundary_area <- Hab_boundary[, c("Pressure", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ Pressure, data = ., FUN = sum)
  Outside_area <- Hab_Type[, c("Pressure", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ Pressure, data = ., FUN = sum)
  
  # Calculate differences in areas

  ten_area_new <-replace(ten_area, is.na(ten_area), 0)
 
  

  twenty_area_new <- replace(twenty_area, is.na(twenty_area), 0)

  

  boundary_area_new <- replace(boundary_area, is.na(boundary_area), 0)


  Outside_area_new <- replace(Outside_area, is.na(Outside_area), 0)

  
  # Merge dataframes
  ten_area_new <- ten_area_new[, c("Pressure", "Area")]
  twenty_area_new <- twenty_area_new[, c("Pressure", "Area")]
  boundary_area_new <- boundary_area_new[, c("Pressure", "Area")]
  Outside_area_new <- Outside_area_new[, c("Pressure", "Area")]
  
  five_area$Location <- "Five"
  ten_area_new$Location <- "Ten"
  twenty_area_new$Location <- "Twenty"
  boundary_area_new$Location <- "Boundary"
  Outside_area_new$Location <- "Outside"
  
  Hab_final <- rbind(five_area, ten_area_new, twenty_area_new, boundary_area_new, Outside_area_new)
  
  # Filter out negative areas
  Hab_final$Area[Hab_final$Area < 0] <- 0
  Hab_final$Pressure[Hab_final$Pressure == 0] <- 9 #Change all 0s to 9s
  Hab_final$Pressure[is.na(Hab_final$Pressure)] <- 9
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    Hab_final$Location <- factor(Hab_final$Location, levels = order_locations)
  }
  Hab_final <- subset(Hab_final, Location != "Outside")#don't want to plot outside
  
  # Plotting
  plot <- ggplot(Hab_final, aes(fill = as.factor(Pressure), y = Area, x = Location)) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = Heading, values = c("9" = "#440154",
                                                               "8" = "#472d7b",
                                                               "7" = "#3b528b",
                                                               "6" = "#2c728e",
                                                               "5" = "#21918c",
                                                               "4" = "#28ae80",
                                                               "3" = "#5ec962",
                                                               "2" = "#addc30",
                                                               "1" = "#FDE725"),
                      labels = c(
                        "1" = "High",
                        "2" = "Medium",
                        "3" = "Low",
                        "4" = "Insufficient sensitivity evidence",
                        "5" = "No sensitivity asessment carried out",
                        "6" = "Not sensitive",
                        "7" = "No direct effects",
                        "8" = "Not relevant to biotope",
                        "9" = "Null values"
                      ))+
    ggtitle(Title)+
    xlab("Location") +
    ylab("Percentage area (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    theme_classic()
  return(list(plot = plot, Hab_final=Hab_final))
}

