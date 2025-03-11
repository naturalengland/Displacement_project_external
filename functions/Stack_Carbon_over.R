Stack_Carbon_over <- function(tacsatEflalo, BC,SF_CODE,order_locations) {
  tacsatEflalo <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
  st_crs(tacsatEflalo) <- 4326
  idx     <- st_over(tacsatEflalo,BC)
  tacsatEflalo$SF_CODE <- BC$SF_CODE[idx]
  
  Hab_table <- tacsatEflalo %>%
    group_by(Location, SF_CODE) %>%
    summarise(frequency = n()) %>%
    ungroup()
  
  Hab_table <- st_drop_geometry(Hab_table)
  
  Hab_table# we can look at ping frequency based on habitat type for each location
  
  #now have assigned a habitat type to each ping
  
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    tacsatEflalo$Location <- factor(tacsatEflalo$Location, levels = order_locations)
  }
  
  tacsatEflalo$SF_CODE[is.na(tacsatEflalo$SF_CODE)] <- "Other" 
  tacsatEflalo2 <- tacsatEflalo %>%
    mutate(Ping = 1)
  

  
  tacsatEflalo_five <- tacsatEflalo2%>%
    filter(Location == order_locations[[1]])
  tacsatEflalo2 <- subset(tacsatEflalo2, Location != "Outside")#don't want to plot outside
  tacsatEflalo2 <- tacsatEflalo2 %>%
    mutate(Ping = 1)
  tacsatEflalo2<- tacsatEflalo2 %>%
    group_by(SF_CODE, Location) %>%
    summarize(Ping = sum(Ping))  
  
  plot<-ggplot(tacsatEflalo2, aes(fill=as.factor(SF_CODE), y=Ping, x=Location)) + 
    geom_bar(position="fill", stat="identity")+
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
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    ylab("Percentage of activity (%)")+
    ggtitle("Blue carbon habitat type")+#shows the proportion of pings on each habitat type
    theme_classic()
  
  plot2<-ggplot(tacsatEflalo_five, aes(fill=as.factor(SF_CODE), y=TIME/60, x=LE_GEAR)) + 
    geom_bar(position="stack", stat="identity")+
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
    ylab("Fishing effort (hrs)")+
    xlab("Gear type")+
    theme_classic()+
    ggtitle("Blue carbon habitat type")#shows number of pings of each habitat type depending on gear in five
  return(list(plot = plot,plot2=plot2, tacsatEflalo=tacsatEflalo))
}
