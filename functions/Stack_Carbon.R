
Stack_Carbon <- function(SF_CODE, five_site, ten_hole, twenty_hole, boundary_hole, order_locations) {
  
  # Finding polygon intersections
  BC_five <- st_intersection(BC, five_site)
  BC_ten <- st_intersection(BC, ten_hole)
  BC_twenty <- st_intersection(BC, twenty_hole)
  BC_boundary <- st_intersection(BC, boundary_hole)
  
  BC_five <- st_transform(BC_five, crs = 3035)
  BC_ten <- st_transform(BC_ten, crs = 3035)
  BC_twenty <- st_transform(BC_twenty, crs = 3035)#set crs to standard
  BC_boundary<- st_transform(BC_boundary, crs = 3035)
  BC<- st_transform(BC, crs = 3035)
  
  five_site<- st_transform(five_site, crs = 3035)
  ten_hole <- st_transform(ten_hole, crs = 3035)
  twenty_hole <- st_transform(twenty_hole, crs = 3035)#set crs to standard
  boundary_hole<- st_transform(boundary_hole, crs = 3035)
  
  # Calculating areas
  BC_five$Area <- st_area(BC_five)
  BC_ten$Area <- st_area(BC_ten)
  BC_twenty$Area <- st_area(BC_twenty)
  BC_boundary$Area <- st_area(BC_boundary)
  BC$Area <- st_area(BC)
  
  five_site$Area <- st_area(five_site)
  ten_hole$Area <- st_area(ten_hole)
  twenty_hole$Area <- st_area(twenty_hole)
  boundary_hole$Area <- st_area(boundary_hole)
  
  # Replace NAs with 0s
  BC_five$Area[is.na(BC_five$Area)] <- 0
  BC_ten$Area[is.na(BC_ten$Area)] <- 0
  BC_twenty$Area[is.na(BC_twenty$Area)] <- 0
  BC_boundary$Area[is.na(BC_boundary$Area)] <- 0
  BC$Area[is.na(BC$Area)] <- 0
  
  # Aggregate areas
  five_area <- BC_five[, c(SF_CODE, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SF_CODE, data = ., FUN = sum)
  ten_area <- BC_ten[, c(SF_CODE, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SF_CODE, data = ., FUN = sum)
  twenty_area <- BC_twenty[, c(SF_CODE, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SF_CODE, data = ., FUN = sum)
  boundary_area <- BC_boundary[, c(SF_CODE, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SF_CODE, data = ., FUN = sum)
  Outside_area <- BC[, c(SF_CODE, "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SF_CODE, data = ., FUN = sum)
  
  # Calculate differences in areas
  ten_area_new <-replace(ten_area, is.na(ten_area), 0)

  
  twenty_area_new <- replace(twenty_area, is.na(twenty_area), 0)

  

  boundary_area_new <- replace(boundary_area, is.na(boundary_area), 0)


  Outside_area_new <- replace(Outside_area , is.na(Outside_area), 0)

  
  # Merge dataframes
  ten_area_new <- ten_area_new[, c(SF_CODE, "Area")]
  twenty_area_new <- twenty_area_new[, c(SF_CODE, "Area")]
  boundary_area_new <- boundary_area_new[, c(SF_CODE, "Area")]
  Outside_area_new <- Outside_area_new[, c(SF_CODE, "Area")]
  
  five_area$Location <- "Five"
  ten_area_new$Location <- "Ten"
  twenty_area_new$Location <- "Twenty"
  boundary_area_new$Location <- "Boundary"
  Outside_area_new$Location <- "Outside"
  
  five_area$site_area <- sum(five_site$Area)
  five_area$site_area <- st_drop_geometry(five_area$site_area)
  
  ten_area_new$site_area <- sum(ten_hole$Area)
  ten_area_new$site_area <- st_drop_geometry(ten_area_new$site_area)
  
  twenty_area_new$site_area <- sum(twenty_hole$Area)
  twenty_area_new$site_area <- st_drop_geometry(twenty_area_new$site_area)
  
  boundary_area_new$site_area <- sum(boundary_hole$Area)
  boundary_area_new$site_area <- st_drop_geometry(boundary_area_new$site_area)
  
  Outside_area_new$site_area <- sum(Outside_area_new$Area)
  Outside_area_new$site_area <- st_drop_geometry(Outside_area_new$site_area)
  
  Hab_final <- rbind(five_area, ten_area_new, twenty_area_new, boundary_area_new, Outside_area_new)
  
  # Filter out negative areas
  Hab_final$Area[Hab_final$Area < 0] <- 0
 
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    Hab_final$Location <- factor(Hab_final$Location, levels = order_locations)
  }
  Hab_final$site_area <- sub("\\[m\\^2\\]", "", Hab_final$site_area)
  Hab_final$Area<- sub("\\[m\\^2\\]", "",Hab_final$Area)
  Hab_final$Final_area <- (as.numeric((Hab_final$Area))/as.numeric((Hab_final$site_area)))*100
  options(scipen = 999)
  Hab_final <- subset(Hab_final, Location != "Outside")#don't want to plot outside
  percentage_sum <- Hab_final %>%
    group_by(Location) %>%
    summarise(Final_area = sum(Final_area))
  percentage_sum <- percentage_sum %>%
    mutate(Final_area = 100 - Final_area)
  percentage_sum$SF_CODE<-"Other"
  Hab_final <- bind_rows(Hab_final,percentage_sum)
  # Plotting
  plot <- ggplot(Hab_final, aes(fill = SF_CODE, y = Final_area, x = Location)) + 
    geom_col() +
    scale_fill_manual(name= "Habitat type", 
                      values = c("A2.2" = "#FDE725",
                                 "A2.3" = "#addc30",
                                 "A2.5" = "#5ec962",
                                 "A2.61" = "#28ae80",
                                 "A3" = "#21918c",
                                 "A5.2" = "#2c728e",
                                 "A5.3" = "#3b528b",
                                 "A5.53" = "#472d7b",
                                 "Other" = "#440154"
                      ), labels = c("A2.2" ="A2.2: Intertidal sand and muddy sand", "A2.3" = "A2.3: Intertidal mud",
                                    "A2.5" ="A2.5: Saltmarsh", "A2.61" ="A2.61: Seagrass beds","A3" ="A3: Kelp on rock substrate","A5.2" = "A5.2: Subtidal sand",
                                    "A5.3" = "A5.3: Subtidal mud", "A5.53" ="A5.53: Seagrass beds", "Other" ="Other habitat"))+
    ggtitle("Blue carbon habitat") +
    xlab("Location") +
    ylab("Percentage area of blue carbon habitat (%)") +
    theme_classic()
  
  return(list(plot = plot, Hab_final=Hab_final))
}
