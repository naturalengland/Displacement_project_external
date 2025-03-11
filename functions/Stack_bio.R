Stack_bio <- function(Hab_Type,uncertainty_sim,Title, five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading) {

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
  five_area <- Hab_five[, c("uncertainty_sim", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ uncertainty_sim, data = ., FUN = sum)
  ten_area <- Hab_ten[, c("uncertainty_sim", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ uncertainty_sim, data = ., FUN = sum)
  twenty_area <- Hab_twenty[, c("uncertainty_sim", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ uncertainty_sim, data = ., FUN = sum)
  boundary_area <- Hab_boundary[, c("uncertainty_sim", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ uncertainty_sim, data = ., FUN = sum)
  Outside_area <- Hab_Type[, c("uncertainty_sim", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ uncertainty_sim, data = ., FUN = sum)
  
  # Calculate differences in areas

  ten_area_new <-replace(ten_area, is.na(ten_area), 0)

  
  twenty_area_new <- replace(twenty_area, is.na(twenty_area), 0)

  boundary_area_new <- replace(boundary_area, is.na(boundary_area), 0)
Outside_area_new <- replace(Outside_area , is.na(Outside_area), 0)

  
  # Merge dataframes
  ten_area_new <- ten_area_new[, c("uncertainty_sim", "Area")]
  twenty_area_new <- twenty_area_new[, c("uncertainty_sim", "Area")]
  boundary_area_new <- boundary_area_new[, c("uncertainty_sim", "Area")]
  Outside_area_new <- Outside_area_new[, c("uncertainty_sim", "Area")]
  
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
  assign_scale <- function(uncertainty_sim) {
    uncertainty_sim <- 
                       ifelse(uncertainty_sim >= 0 & uncertainty_sim <= 0.25, "Very low",
                       ifelse(uncertainty_sim > 0.25 & uncertainty_sim <= 0.50, "Low",
                       ifelse(uncertainty_sim > 0.5 & uncertainty_sim <= 0.75, "Moderate",
                       ifelse(uncertainty_sim > 0.75 & uncertainty_sim < 1, "High",
                       ifelse(uncertainty_sim >=1, "Very high","Very low")))))
    return(uncertainty_sim)
  }
  Hab_final$uncertainty_sim <- assign_scale(Hab_final$uncertainty_sim)
  Hab_final$uncertainty_sim <- factor(Hab_final$uncertainty_sim, levels = c("Very high","High","Moderate","Low","Very low"))
  Hab_final <- subset(Hab_final, Location != "Outside")#don't want to plot outside
  
  # Plotting
  plot <- ggplot(Hab_final, aes(fill = as.factor(uncertainty_sim), y = Area, x = Location)) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(name = Heading, values = c("Very low" = "#440154",
                                                               "Low" = "#3b528b",
                                                               "Moderate" = "#21918c",
                                                               "High" = "#5ec962",
                                                               "Very high" = "#FDE725"))+
    ggtitle(Title)+
    xlab("Location") +
    ylab("Percentage area (%)") +
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    theme_classic()
  return(list(plot = plot, Hab_final=Hab_final))
}
