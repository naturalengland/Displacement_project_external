createMap <- function(data, LE_TOT, scale_limits, legend_title, additional_shapefiles = NULL, additional_colors = NULL, x_limits = NULL, y_limits = NULL) {
  areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
  areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
  
  bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                matrix(st_bbox(areaRef), ncol = 2))
  rownames(bbox) <- c("x", "y")
  
  spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                    yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
  

  grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
  if (!is.null(x_limits)) {
    spatBound$xrange <- x_limits
  }
  if (!is.null(y_limits)) {
    spatBound$yrange <- y_limits
  }
  
  st_crs(grd) <- 4326
  
  grd$data <- 0
  
  tacsatEflalo <- intervalTacsat(tacsatEflalo, level = "vessel", fill.na = TRUE)
  
  idx <- st_over(coords, grd)
  tacsatEflalo$gridID <- idx
  
  grd$data[an(names(table(idx)))] <- aggregate(tacsatEflalo[[LE_TOT]], by = list(tacsatEflalo$gridID), FUN = sum, na.rm = TRUE)$x
  
  ve_ref_counts <- aggregate(VE_REF ~ gridID, data = tacsatEflalo, FUN = function(x) length(unique(x)))
  
  grd$data1 <- ve_ref_counts$VE_REF[match(grd$grID, ve_ref_counts$gridID)]
  grd$data1[is.na(grd$data1)] <- 0
  
  grd <- grd %>%
    mutate(data = ifelse(data1 < 3, 0, data))
  
  grd$data[grd$data == 0] <- NA
  
  baseMap <- ggplot() +
    geom_sf(data = europa, colour = 1, fill = "darkgreen") +
    geom_sf(data = grd, mapping = aes(fill = data), color = NA) +
    scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = scale_limits, name = legend_title) +
    geom_sf(data = europa, colour = 1, fill = "darkgreen") +
    xlim(spatBound$xrange) +
    ylim(spatBound$yrange) +
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
  
  return(list(map = baseMap, grid = grd))
}


