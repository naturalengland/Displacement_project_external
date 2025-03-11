plot_sensitivity_map <- function(data, sensitivity_column,Title,additional_shapefiles = NULL, additional_colors = NULL,additional_border_patterns = NULL,Heading, x_limits = NULL, y_limits = NULL) {
areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
  areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
  
  bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                matrix(st_bbox(areaRef), ncol = 2))
  rownames(bbox) <- c("x", "y")
  
  spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                    yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
  
  if (!is.null(x_limits)) {
    spatBound$xrange <- x_limits
  }
  if (!is.null(y_limits)) {
    spatBound$yrange <- y_limits
  }

  data <- data %>%
    mutate({{ sensitivity_column }} := ifelse({{ sensitivity_column }} %in% 1:8, {{ sensitivity_column }}, 9))

  baseMap <- ggplot() + 
    xlim(spatBound$xrange) +
    ylim(spatBound$yrange) +
    labs(x = xl$label, y = yl$label) + 
    theme_bw() +
    theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
          axis.title = element_text(face = xl$font, size = rel(xl$cex))) + 
    geom_sf(data = data, mapping = aes(fill = as.factor({{ sensitivity_column }})), color = NA) +
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
    ggtitle(Title)
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
