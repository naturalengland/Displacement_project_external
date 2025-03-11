Carbon_map <- function(data, additional_shapefiles = NULL, additional_colors = NULL,additional_border_patterns = NULL,  x_limits, y_limits) {
  baseMap <- ggplot() + 
    geom_sf((data = BC_twenty), 
            mapping = aes(fill = as.factor(SF_CODE)), color = NA)+
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
    ggtitle("Blue carbon habitat type") +
    xlim(x_limits)+
    ylim(y_limits)+
    labs(x=xl$label,y=yl$label) + 
    theme_bw() +
    theme(plot.margin      = unit(c(10,10,10,10),"mm"),
          axis.title       = element_text(face = xl$font,size = rel(xl$cex))) 
  if (!is.null(additional_shapefiles)) {
    for (i in seq_along(additional_shapefiles)) {
      # Add the shapefile without border pattern
      color <- if (!is.null(additional_colors)) additional_colors[[i]] else "black"
        
      
      # Add the patterned border if specified
      if (!is.null(additional_border_patterns) && !is.null(additional_border_patterns[[i]])) {
        pattern <- additional_border_patterns[[i]]
        
        baseMap <- baseMap + 
          geom_sf(data = additional_shapefiles[[i]], fill = NA, color = color, linetype = pattern, linewidth = 0.4)+
          geom_sf(data = europa, colour = 1, fill = "grey") + 
          theme(panel.background = element_rect(fill = 'lightblue'))+
          theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
      }
    }
  }
  
  return(list(map = baseMap))
}
