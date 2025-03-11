Hab_map <- function(data, additional_shapefiles = NULL, additional_colors = NULL,additional_border_patterns = NULL, x_limits, y_limits) {
  data <- data %>%
    mutate(HAB_TYPE = if_else(HAB_TYPE%in% c("A5.35", "A5.44"), HAB_TYPE, "Other"))
   baseMap <- ggplot() + 
    geom_sf((data = data), 
            mapping = aes(fill = as.factor(HAB_TYPE)), color = NA)+
    scale_fill_manual(name= "Habitat type", 
                      values = c(c("A5.35" = "#440154",
                                   "A5.44" = "#1F9E89","Other" = "#FDE725")
                      ), labels = c("A5.35", "A5.44", "Other"))+
    ggtitle("Habitat type: EUNIS Classification") +
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