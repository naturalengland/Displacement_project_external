# Initialize an empty grid
grd_merged <- grd
grd_merged$data <- 0

# Assuming 'VE_REF' is a column in subset_total
vessel_list <- unique(tacsatEflalo$VE_REF)


for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TIME) / 60
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TIME) / 60
  
  #- Define the size of the grid cell
  resx <- 0.05
  resy <- 0.05
  
  coords <- st_as_sf(subset_total, coords = c("SI_LONG", "SI_LATI"))
  st_crs(coords) <- 4326  # set crs
  
  areaInt <- subset(ICESareas, Area_27 %in% c("4.a", "4.b", "4.c", "7.d", "7.e", "7.h", "7.f", "7.g", "7.a", "6.a", "7.b", "7.j.2"))
  areaRef <- subset(ICESareas, Area_27 %in% c("7.f", "7.e"))
  
  bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                matrix(st_bbox(areaRef), ncol = 2))
  rownames(bbox) <- c("x", "y")
  
  spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                    yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
  grd <- createGrid(spatBound$xrange, spatBound$yrange, resx, resy, type = "GridDF", exactBorder = TRUE)
  
  st_crs(grd) <- 4326
  
  grd$data <- 0
  
  subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
  
  idx <- st_over(coords, grd)
  subset_total$gridID <- idx
  
  grd$data[an(names(table(idx)))] <- aggregate(subset_total$TIME, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$data <- grd$data / 60  # Divide to get to hrs
  grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase
  
  grd_merged$data <- grd_merged$data + grd$data
}

sum(grd_merged$data)

save(grd_merged,file="./processed_data/grd_predict.RData")

grd_merged$data[grd_merged$data == 0] <- NA
baseMap <- ggplot() +
  geom_sf(data = europa, colour = 1, fill = "darkgreen") +
  geom_sf(data = grd_merged, mapping = aes(fill = data), color = NA) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "transparent", name = "Time (hrs)", limits = c(0, 800)) +
  geom_sf(data = europa, colour = 1, fill = "darkgreen") +
  xlim(x_limits = c(-3.6,-3.2)) +
  ylim(y_limits = c(50.4,50.7)) +
  labs(x = xl$label, y = yl$label) +
  geom_sf(data = five_site, colour = "white", fill = "white") +
  geom_sf(data = ten_site, colour = "blue", fill = NA) +
  geom_sf(data = twenty_site, colour = "yellow", fill = NA) +
  geom_sf(data = boundary, colour = "darkblue", fill = NA) +
  theme_bw() +
  theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
        axis.title = element_text(face = xl$font, size = rel(xl$cex)))


load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")


pdf(file = "./processed_data/Figure.2.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 4) 
baseMap 
dev.off()