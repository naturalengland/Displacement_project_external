Heatmap<- function(coords,LE_TOT, scale_limits,legend_title, x_limits, y_limits,additional_shapefiles = NULL, additional_colors = NULL) {
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
    geom_sf(data = europa, colour = 1, fill = "grey") + 
    geom_sf(data = grd, mapping = aes(fill = as.numeric(ratio)), color = NA) +
    scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = scale_limits, name = legend_title) +
    geom_sf(data = europa, colour = 1, fill = "grey") +
    xlim(x_limits) +                                                             
    ylim(y_limits) +
    labs(x = xl$label, y = yl$label) + 
    theme_bw() +
    theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
          axis.title = element_text(face = xl$font, size = rel(xl$cex)))
  
  if (!is.null(additional_shapefiles)) {
    for (i in seq_along(additional_shapefiles)) {
      if (!is.null(additional_colors)) {
        color <- additional_colors[[i]]
      } else {
        color <- NULL
      }
      baseMap <- baseMap + geom_sf(data = additional_shapefiles[[i]], col = color, fill=NA)
    }
  }
  
  return(list(map = baseMap, grid = grd, coords=coords))
}


result <- Heatmap(coords, LE_TOT = "TIME", scale_limits = c(0, 250), legend_title = "Effort (kWh)", x_limits = c(-3.6,-3.2), y_limits = c(50.4,50.7), additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), additional_colors = list("darkblue","red", "blue", "yellow"))

Kwh<-result$map


All_plot<- (Value + Weight + Time + Kwh)+
  plot_annotation(tag_levels = 'A')#join together using patchwork

All_plot