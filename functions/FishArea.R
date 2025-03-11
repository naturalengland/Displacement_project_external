FishArea <- function(Fish_hotspot_twenty,LE_TOT,scale_limits, x_limits, y_limits,Name,custom_boundaries, Title, additional_shapefiles = NULL,additional_colors = NULL, additional_border_patterns = NULL) {
 
  baseMap <- ggplot() + 
    geom_sf(data = Fish_hotspot_twenty, mapping = aes(fill = as.numeric(Fish_hotspot_twenty[[LE_TOT]])), color = NA) +
    scale_fill_viridis(name = Name,breaks = custom_boundaries, direction = 1) +
    xlim(x_limits) +                                                             
    ylim(y_limits) +
    labs(x = xl$label, y = yl$label,title = Title) + 
    theme_bw() +
    theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
          axis.title = element_text(face = xl$font, size = rel(xl$cex)))
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

