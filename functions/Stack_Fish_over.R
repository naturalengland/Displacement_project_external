Stack_Fish_over <- function(tacsatEflalo, Fish_hotspot,spawning,Title, Heading,order_locations) {
  tacsatEflalo <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
  st_crs(tacsatEflalo) <- 4326
  idx     <- st_over(tacsatEflalo,Fish_hotspot)
  tacsatEflalo$spawning <- Fish_hotspot$spawning[idx]
  
  Hab_table <- tacsatEflalo %>%
    group_by(Location, spawning) %>%
    summarise(frequency = n()) %>%
    ungroup()
  
  Hab_table <- st_drop_geometry(Hab_table)
  
  Hab_table# we can look at ping frequency based on habitat type for each location
  
  #now have assigned a habitat type to each ping
  
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    tacsatEflalo$Location <- factor(tacsatEflalo$Location, levels = order_locations)
  }
  
  tacsatEflalo$spawning[is.na(tacsatEflalo$spawning)] <- 0
  tacsatEflalo2 <- tacsatEflalo %>%
    mutate(Ping = 1)
  

  
  assign_scale <- function(spawning) {
  spawning <- ifelse(spawning >= 0 & spawning <= 3, "0-3",
  ifelse(spawning >= 4 & spawning <= 6, "4-6",
  ifelse(spawning >= 7 & spawning <= 9, "7-9",
  ifelse(spawning >= 10 & spawning <= 12, "10-12",
  ifelse(spawning >= 13 , ">12","0-3")))))
    return(spawning)
  }
  tacsatEflalo2$Spawning <- assign_scale(tacsatEflalo2$spawning)
  tacsatEflalo2$Spawning <- factor(tacsatEflalo2$Spawning, levels = c("0-3", "4-6", "7-9","10-12",">12"))
  tacsatEflalo2 <- subset(tacsatEflalo2, Location != "Outside")#don't want to plot outside
  
  tacsatEflalo_five <- tacsatEflalo2%>%
    filter(Location == order_locations[[1]])
  
  tacsatEflalo2<- tacsatEflalo2 %>%
    group_by(Location, Spawning) %>%
    summarize(Ping = sum(Ping))          # Drop grouping for cleaner output
  
  plot<-ggplot(tacsatEflalo2, aes(fill=as.factor(Spawning), y=Ping, x=Location)) + 
    geom_bar(position="fill", stat="identity")+
    scale_fill_manual(name = Heading, values = c("0-3" = "#440154",
                                                 "4-6" = "#3b528b",
                                                 "7-9" = "#21918c",
                                                 "10-12" = "#5ec962",         
                                                 ">12" = "#FDE725"))+
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    ylab("Percentage of activity (%)")+
    ggtitle(Title)+#shows the proportion of pings on each habitat type
    theme_classic()
  
  plot2<-ggplot(tacsatEflalo_five, aes(fill=as.factor(Spawning), y=TIME/60, x=LE_GEAR)) + 
    geom_bar(position="stack", stat="identity")+
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
    ylab("Fishing effort (hrs)")+
    xlab("Gear type")+
    theme_classic()+
    ggtitle(Title)#shows number of pings of each habitat type depending on gear in five
  return(list(plot = plot,plot2=plot2, tacsatEflalo=tacsatEflalo))
}
