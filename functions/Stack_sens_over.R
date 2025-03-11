Stack_sens_over <- function(tacsatEflalo, Hab_Type,Pressure,Title,Heading, order_locations) {
  Hab_Type$Pressure <- ifelse(Hab_Type$Pressure %in% 1:8, Hab_Type$Pressure, 9)
  tacsatEflalo <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
  st_crs(tacsatEflalo) <- 4326
  idx     <- st_over(tacsatEflalo,Hab_Type)
  tacsatEflalo$Pressure <- Hab_Type$Pressure[idx]
  
  Hab_table <- tacsatEflalo %>%
    group_by(Location, Pressure) %>%
    summarise(frequency = n()) %>%
    ungroup()
  
  Hab_table <- st_drop_geometry(Hab_table)
  
  Hab_table# we can look at ping frequency based on habitat type for each location
  
  #now have assigned a habitat type to each ping
  
  # Reorder Locations if specified
  if (!missing(order_locations)) {
    tacsatEflalo$Location <- factor(tacsatEflalo$Location, levels = order_locations)
  }
  
  tacsatEflalo$Pressure[is.na(tacsatEflalo$Pressure)] <- 9
  tacsatEflalo2 <- tacsatEflalo %>%
    mutate(Ping = 1)
  
  tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                               ifelse(tacsatEflalo$Pressure == 2, 2,
                               ifelse(tacsatEflalo$Pressure == 3, 1, 
                               ifelse(tacsatEflalo$Pressure == 4, 3, 
                               ifelse(tacsatEflalo$Pressure == 5, 3, 
                               ifelse(tacsatEflalo$Pressure == 6, 0,
                               ifelse(tacsatEflalo$Pressure == 7, 0, 
                               ifelse(tacsatEflalo$Pressure == 8, 0, 
                               ifelse(tacsatEflalo$Pressure == 9, 3, 
                                ifelse(tacsatEflalo$Pressure == 0, 3, 3))))))))))
  tacsatEflalo_five <- tacsatEflalo2%>%
    filter(Location == order_locations[[1]])
  
  plot<-ggplot(tacsatEflalo2, aes(fill=as.factor(Pressure), y=Ping, x=Location)) + 
    geom_bar(position="fill", stat="identity")+
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
    scale_y_continuous(labels = scales::percent_format(scale = 100))+
    ylab("Percentage of activity (%)")+
    ggtitle(Title)+#shows the proportion of pings on each habitat type
    theme_classic()
  
  plot2<-ggplot(tacsatEflalo_five, aes(fill=as.factor(Pressure), y=TIME/60, x=LE_GEAR)) + 
    geom_bar(position="stack", stat="identity")+
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
    ylab("Fishing effort (hrs)")+
    xlab("Gear type")+
    theme_classic()+
    ggtitle(Title)#shows number of pings of each habitat type depending on gear in five
  return(list(plot = plot,plot2=plot2, tacsatEflalo=tacsatEflalo))
}

