Heatmap <- function(coords, LE_TOT, scale_limits, legend_title, x_limits, y_limits, additional_shapefiles = NULL, additional_colors = NULL, additional_border_patterns = NULL) {
  areaInt <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
  areaRef <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
  
  bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                matrix(st_bbox(areaRef), ncol = 2))
  rownames(bbox) <- c("x", "y")
  spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                    yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
  grd <- createGrid(spatBound$x, spatBound$y, resx, resy, type = "GridDF", exactBorder = T)
  
  st_crs(grd) <- 4326
  
  coords$TR_AREA <- (coords[[LE_TOT]])
  
  grd$TR_AREA <- 0
  st_crs(grd) <- 4326
  
  idx <- st_over(coords, grd)
  coords$gridID <- idx
  agg <- aggregate(coords$TR_AREA, by = list(idx), FUN = sum, na.rm = T)
  colnames(agg) <- c("grID", "TR_AREA")
  
  grd$TR_AREA <- agg$TR_AREA[match(grd$grID, agg$grID)]
  
  ve_ref_counts <- aggregate(VE_REF ~ gridID, data = coords, FUN = function(x) length(unique(x)))
  
  grd$data1 <- ve_ref_counts$VE_REF[match(grd$grID, ve_ref_counts$gridID)]
  grd$data1[is.na(grd$data1)] <- 0
  
  grd <- grd %>%
    mutate(TR_AREA = ifelse(data1 < 3, 0, TR_AREA))
  
  grd$TR_AREA[is.na(grd$TR_AREA)] <- 0
  
  colSums(st_drop_geometry(grd[, c("TR_AREA")]), na.rm = T)
  
  grd <- grd %>%
    mutate(ratio = TR_AREA)
  
  grd <- grd %>%
    filter(as.numeric(ratio) != 0)
  
  grd$ratio <- gsub("\\[1/m\\^2\\]", "", grd$ratio)
  
  baseMap <- ggplot() + 
    geom_sf(data = grd, mapping = aes(fill = as.numeric(ratio)), color = NA) +
    scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = scale_limits, name = legend_title) +
    geom_sf(data = europa, colour = 1, fill = "grey") +
    xlim(x_limits) +                                                             
    ylim(y_limits) +
    labs(x = xl$label, y = yl$label) + 
    theme_classic() +
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
  
  return(list(map = baseMap, grid = grd, coords = coords))
}
