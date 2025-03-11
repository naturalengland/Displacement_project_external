
Stack_hab_over <- function(tacsatEflalo, order_locations) {
  
  tacsatEflalo <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
  st_crs(tacsatEflalo) <- 4326
  idx     <- st_over(tacsatEflalo,Hab_Type)
  tacsatEflalo$HAB_TYPE <- Hab_Type$HAB_TYPE[idx]
  
  Hab_table <- tacsatEflalo %>%
    group_by(Location, HAB_TYPE) %>%
    summarise(frequency = n()) %>%
    ungroup()
  
  Hab_table <- st_drop_geometry(Hab_table)
  
  Hab_table# we can look at ping frequency based on habitat type for each location
  
  #now have assigned a habitat type to each ping
  
  
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    tacsatEflalo$Location <- factor(tacsatEflalo$Location, levels = order_locations)
  }
  
  tacsatEflalo2 <- tacsatEflalo %>%
    mutate(Ping = 1)
  
  
  
  tacsatEflalo2$HAB_TYPE <- ifelse(tacsatEflalo2$HAB_TYPE %in% c("A5.35", "A5.44"), tacsatEflalo2$HAB_TYPE, "Other")
  tacsatEflalo2 <- subset(tacsatEflalo2, Location != "Outside")
  tacsatEflalo_five <- tacsatEflalo2%>%
    filter(Location == order_locations[[1]])
  
  plot<-ggplot(tacsatEflalo2, aes(fill=HAB_TYPE, y=Ping, x=Location)) + 
    geom_bar(position="fill", stat="identity")+
    
    scale_fill_manual(name= "Habitat type", values = 
                        c("A5.35" = "#440154",
                          "A5.44" = "#1F9E89","Other" = "#FDE725"
                        ),labels = c("A5.35", "A5.44", "Other"))+
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    ylab("Percentage of activity (%)")+
    ggtitle("Fishing activity: EUNIS Habitat type")+#shows the proportion of pings on each habitat type
    theme_classic()
  
 plot2<-ggplot(tacsatEflalo_five, aes(fill=HAB_TYPE, y=TIME/60, x=LE_GEAR)) + 
    geom_bar(position="stack", stat="identity")+
    
    scale_fill_manual(name= "Habitat type", values = 
                        c("A5.35" = "#440154",
                          "A5.44" = "#1F9E89","Other" = "#FDE725"),labels = c("A5.35", "A5.44", "Other"))+
   ylab("Fishing effort (hrs)")+
   xlab("Gear type")+
   theme_classic()+
    ggtitle("Fishing effort in area of closure")#shows number of pings of each habitat type depending on gear in five
  return(list(plot = plot,plot2=plot2, tacsatEflalo=tacsatEflalo))
}
