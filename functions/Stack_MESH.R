Stack_MESH <- function(Hab_Type,SUM_CONF,Title, five_site, ten_hole, twenty_hole, boundary_hole, order_locations,Heading) {

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
  five_area <- Hab_five[, c("SUM_CONF", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SUM_CONF, data = ., FUN = sum)
  ten_area <- Hab_ten[, c("SUM_CONF", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SUM_CONF, data = ., FUN = sum)
  twenty_area <- Hab_twenty[, c("SUM_CONF", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SUM_CONF, data = ., FUN = sum)
  boundary_area <- Hab_boundary[, c("SUM_CONF", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SUM_CONF, data = ., FUN = sum)
  Outside_area <- Hab_Type[, c("SUM_CONF", "Area")] %>% st_drop_geometry() %>% aggregate(. ~ SUM_CONF, data = ., FUN = sum)
  
  # remove nas
ten_area_new <-replace(ten_area, is.na(ten_area), 0)
  
twenty_area_new <- replace(twenty_area, is.na(twenty_area), 0)

boundary_area_new <- replace(boundary_area, is.na(boundary_area), 0)

Outside_area_new <- replace(Outside_area , is.na(Outside_area), 0)

  
  # Merge dataframes
  ten_area_new <- ten_area_new[, c("SUM_CONF", "Area")]
  twenty_area_new <- twenty_area_new[, c("SUM_CONF", "Area")]
  boundary_area_new <- boundary_area_new[, c("SUM_CONF", "Area")]
  Outside_area_new <- Outside_area_new[, c("SUM_CONF", "Area")]
  
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
  assign_scale <- function(SUM_CONF) {
    SUM_CONF <- 
                       ifelse(SUM_CONF >= 0 & SUM_CONF <= 20, "Very low",
                       ifelse(SUM_CONF > 20 & SUM_CONF <= 37, "Low",
                       ifelse(SUM_CONF > 37 & SUM_CONF <= 58, "Moderate",
                       ifelse(SUM_CONF > 58 & SUM_CONF <= 79, "High",
                       ifelse(SUM_CONF > 79 & SUM_CONF <= 100, "Very high","Very low")))))
    return(SUM_CONF)
  }
  Hab_final$SUM_CONF <- assign_scale(Hab_final$SUM_CONF)
  Hab_final$SUM_CONF <- factor(Hab_final$SUM_CONF, levels = c("Very high","High","Moderate","Low","Very low"))
  Hab_final <- subset(Hab_final, Location != "Outside")#don't want to plot outside
  
  # Plotting
  plot <- ggplot(Hab_final, aes(fill = as.factor(SUM_CONF), y = Area, x = Location)) + 
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
