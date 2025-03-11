load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
subset_vessel<-tacsatEflalo
vessel <- unique_vessels[1]
subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
time_sum <- sum(subset_df$TIME)
subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
total_time_sum <- sum(subset_total$TIME)

#- Define the size of the grid cell
resx <- 0.025
resy <- 0.025

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
grd$data <- grd$data  # Divide to get to hrs
grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase


# Initialize an empty grid
grd_merged <- grd
grd_merged$data <- 0

# Assuming 'VE_REF' is a column in subset_total
vessel_list <- unique(tacsatEflalo$VE_REF)


for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TIME)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TIME)
  
  #- Define the size of the grid cell
  resx <- 0.025
  resy <- 0.025
  
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
  grd$data <- grd$data  # Divide to get to hrs
  grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
  
  grd_merged$data <- grd_merged$data + grd$data
}

load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")

grd_merged$data[grd_merged$data <= 0] <- NA

baseMap <- ggplot() +
  geom_sf(data = grd_merged, mapping = aes(fill = data), color = NA) +
  scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,15), name = "Time increase (hrs)") +
  xlim(x_limits = c(-3.6,-3.2)) +
  ylim(y_limits = c(50.4,50.7)) +
  labs(x = xl$label, y = yl$label) +
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid", linewidth= 0.4 ) +
  geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
  geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
  geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
  geom_sf(data = europa, colour = 1, fill = "grey") +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
  theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
        axis.title = element_text(face = xl$font, size = rel(xl$cex)))
Time<-baseMap
save(grd_merged,file="./processed_data/Time_increase_data.RData")
load("./processed_data/Time_increase_data.RData")

############Now for swept area
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
subset_vessel<-tacsatEflalo
subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
time_sum <- sum(subset_df$TR_AREA)
subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
total_time_sum <- sum(subset_total$TR_AREA)

#- Define the size of the grid cell
resx <- 0.025
resy <- 0.025

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

grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase


# Initialize an empty grid
grd_merged <- grd
grd_merged$data <- 0

vessel_list <- unique(tacsatEflalo$VE_REF)

for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TR_AREA)
  #- Define the size of the grid cell
  resx <- 0.025
  resy <- 0.025
  
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
  
  grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
 
  grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
  grd$data[is.na(grd$data)] <- 0
  grd$data[is.nan(grd$data)] <- 0
  grd_merged$data <- grd_merged$data + grd$data
}
save(grd_merged,file="./processed_data/grd_sar.RData")


load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")

#Now for SAR

Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA

Hab_five_hole <- st_make_valid(Hab_five_hole)
st_crs(Hab_five_hole) <- 4326#setting crs
Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
Hab_five_hole$SAR<- Hab_five_hole$data/Hab_five_hole$surface

Hab_five_hole$SAR<-as.numeric(Hab_five_hole$SAR)
Hab_five_hole$SAR[Hab_five_hole$SAR <= 0] <- NA
baseMap <- ggplot() +
  geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(SAR)), color = NA) +
  scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.25), name = "Surface SAR increase") +
  xlim(x_limits = c(-3.6,-3.2)) +
  ylim(y_limits = c(50.4,50.7)) +
  labs(x = xl$label, y = yl$label) +
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
  geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
  geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
  geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
  geom_sf(data = europa, colour = 1, fill = "grey") +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
      axis.title = element_text(face = xl$font, size = rel(xl$cex)))
SAR<-baseMap
save(Hab_five_hole,file="./processed_data/grd_sar_hole.RData")
load("./processed_data/grd_sar_hole.RData")
##############################Now same but for subsurface
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
subset_vessel<-tacsatEflalo
subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
time_sum <- sum(subset_df$TR_AREA)
subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
total_time_sum <- sum(subset_total$TR_AREA)

#- Define the size of the grid cell
resx <- 0.025
resy <- 0.025

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

grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$data <- (grd$data) + ((grd$data * time_sum) / (total_time_sum))  # now for time increase


# Initialize an empty grid
grd_merged <- grd
grd_merged$data <- 0

vessel_list <- unique(tacsatEflalo$VE_REF)

for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TR_AREA)
  #- Define the size of the grid cell
  resx <- 0.025
  resy <- 0.025
  
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
  
  grd$data[an(names(table(idx)))] <- aggregate(subset_total$TR_AREA, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  
  grd$data <- ((grd$data * time_sum) / (total_time_sum))  # now for time increase
  grd$data[is.na(grd$data)] <- 0
  grd$data[is.nan(grd$data)] <- 0
  grd_merged$data <- grd_merged$data + grd$data
}
save(grd_merged,file="./processed_data/grd_sub.RData")


load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")

#Now for SUB SAR

Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA

Hab_five_hole <- st_make_valid(Hab_five_hole)
st_crs(Hab_five_hole) <- 4326#setting crs
Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
Hab_five_hole$SAR<- Hab_five_hole$data/Hab_five_hole$surface

Hab_five_hole$SAR<-as.numeric(Hab_five_hole$SAR)
Hab_five_hole$SAR[Hab_five_hole$SAR <= 0] <- NA
save(Hab_five_hole,file="./processed_data/grd_sub_hole.RData")
baseMap <- ggplot() +
  geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(SAR)), color = NA) +
  scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.1), name = "Subsurface SAR increase") +
  xlim(x_limits = c(-3.6,-3.2)) +
  ylim(y_limits = c(50.4,50.7)) +
  labs(x = xl$label, y = yl$label) +
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
  geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
  geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
  geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
  geom_sf(data = europa, colour = 1, fill = "grey") +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
      axis.title = element_text(face = xl$font, size = rel(xl$cex)))
SUB<-baseMap
load("./processed_data/grd_sub_hole.RData")
All_plot <- ggarrange(Time, SAR, SUB,
                      labels = c("A.", "B.", "C."),    # Plot labels
                      ncol = 2, nrow = 2,                    # Arrange in a 2x2 grid
                      label.x = -0.01,                        # Adjust the x position of the labels
                      label.y = 0.98) 
All_plot
###########Next we look at Swept sensitivity increase
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
#First we match sensitivity to ping
load("./processed_data/eflaloClean.RData")
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: abrasion from demersal trawling"
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#abrasion
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                      ifelse(tacsatEflalo$Pressure == 2, 2,
                      ifelse(tacsatEflalo$Pressure == 3, 1, 
                      ifelse(tacsatEflalo$Pressure == 4, 3, 
                      ifelse(tacsatEflalo$Pressure == 5, 3, 
                      ifelse(tacsatEflalo$Pressure == 6, 0,
                      ifelse(tacsatEflalo$Pressure == 7, 0, 
                      ifelse(tacsatEflalo$Pressure == 8, 0, 
                      ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
load("./processed_data/eflaloClean.RData")
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)

tacsatEflalo <- tacsatEflalo %>%
  mutate(
    High = if_else(Score == 3, TR_AREA, 0),
    Medium = if_else(Score == 2, TR_AREA, 0),
    Low = if_else(Score == 1, TR_AREA, 0),
    Not_relevant = if_else(Score == 0, TR_AREA, 0)
  )#separating based on scores

subset_vessel<-tacsatEflalo
subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
time_sum <- sum(subset_df$TR_AREA)#find total swept area of HypHPMA

subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
total_time_sum <- sum(subset_total$TR_AREA)#find total swept area

#- Define the size of the grid cell
resx <- 0.025
resy <- 0.025

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

grd$Low <- 0
grd$Medium<-0
grd$High<-0
grd$Not_relevant<-0
subset_total<-as.data.frame(subset_total)
subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)

idx <- st_over(coords, grd)
subset_total$gridID <- idx

grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x

grd$Low <- (grd$Low) + ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
grd$Medium <- (grd$Medium) + ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
grd$High <- (grd$High) + ((grd$High * time_sum) / (total_time_sum))  # now for time increase
grd$Not_relevant <- (grd$Not_relevant) + ((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase

grd$Low <-grd$Low*1
grd$Medium <-grd$Medium*2
grd$High <-grd$High*3
grd$Not_relevant <-grd$Not_relevant*0

grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant

# Initialize an empty grid
grd_merged <- grd
grd_merged$Low <-0
grd_merged$Medium <-0
grd_merged$High <-0
grd_merged$Not_relevant <-0

vessel_list <- unique(tacsatEflalo$VE_REF)


for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TR_AREA)
  #- Define the size of the grid cell
  resx <- 0.025
  resy <- 0.025
  
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
  
  grd$Low <- 0
  grd$Medium<-0
  grd$High<-0
  grd$Not_relevant<-0
  
  subset_total<-as.data.frame(subset_total)
  subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
  
  idx <- st_over(coords, grd)
  subset_total$gridID <- idx
  
  grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  
  grd$Low <- ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
  grd$Medium <- ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
  grd$High <- ((grd$High * time_sum) / (total_time_sum))  # now for time increase
  grd$Not_relevant <-((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase
  
  grd$Low <-grd$Low*1
  grd$Medium <-grd$Medium*2
  grd$High <-grd$High*3
  grd$Not_relevant <-grd$Not_relevant*0
  
  grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant
  
  grd$Low[is.na( grd$Low)] <- 0
  grd$Low[is.nan( grd$Low)] <- 0
  grd$Medium[is.na(grd$Medium)] <- 0
  grd$Medium[is.nan(grd$Medium)] <- 0
  grd$High[is.na(grd$High)] <- 0
  grd$High[is.nan(grd$High)] <- 0
  grd$Not_relevant[is.na(grd$Not_relevant)] <- 0
  grd$Not_relevant[is.nan(grd$Not_relevant)] <- 0
  grd$total[is.na(grd$total)] <- 0
  grd$total[is.nan(grd$total)] <- 0
  grd_merged$Low <- grd_merged$Low + grd$Low
  grd_merged$Medium <- grd_merged$Medium + grd$Medium
  grd_merged$High <- grd_merged$High + grd$High
  grd_merged$Not_relevant <- grd_merged$Not_relevant + grd$Not_relevant
  grd_merged$total <- grd_merged$total + grd$total
}

save(grd_merged,file="./processed_data/grd_sens.RData")


load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")


Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA

Hab_five_hole <- st_make_valid(Hab_five_hole)
st_crs(Hab_five_hole) <- 4326#setting crs
Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
Hab_five_hole$Total<-Hab_five_hole$Low + Hab_five_hole$Medium + Hab_five_hole$High
Hab_five_hole$Total[Hab_five_hole$Total <= 0] <- NA
Hab_five_hole$sur<-Hab_five_hole$Total/Hab_five_hole$surface


baseMap <- ggplot() +
  geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(sur)), color = NA) +
  scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.5), name = "SSR (AB) increase") +
  xlim(x_limits = c(-3.6,-3.2)) +
  ylim(y_limits = c(50.4,50.7)) +
  labs(x = xl$label, y = yl$label) +
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
  geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
  geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
  geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
  geom_sf(data = europa, colour = 1, fill = "grey") +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
      axis.title = element_text(face = xl$font, size = rel(xl$cex)))
SAR<-baseMap
save(Hab_five_hole,file="./processed_data/grd_sens_hole.RData")
load("./processed_data/grd_sens_hole.RData")
names(Hab_Type)[names(Hab_Type) == "Pressure"] <- "sens_Z10_6_D6"


####Next we do the same for Subsurface
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
#First we match sensitivity to ping
load("./processed_data/eflaloClean.RData")
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: penetration from demersal trawling"
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#penetration
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                       ifelse(tacsatEflalo$Pressure == 2, 2,
                       ifelse(tacsatEflalo$Pressure == 3, 1, 
                       ifelse(tacsatEflalo$Pressure == 4, 3, 
                       ifelse(tacsatEflalo$Pressure == 5, 3, 
                        ifelse(tacsatEflalo$Pressure == 6, 0,
                        ifelse(tacsatEflalo$Pressure == 7, 0, 
                        ifelse(tacsatEflalo$Pressure == 8, 0, 
                        ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
load("./processed_data/eflaloClean.RData")
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)

tacsatEflalo <- tacsatEflalo %>%
  mutate(
    High = if_else(Score == 3, TR_AREA, 0),
    Medium = if_else(Score == 2, TR_AREA, 0),
    Low = if_else(Score == 1, TR_AREA, 0),
    Not_relevant = if_else(Score == 0, TR_AREA, 0)
  )#separating based on scores

subset_vessel<-tacsatEflalo
subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
time_sum <- sum(subset_df$TR_AREA)#find total swept area of HypHPMA

subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
total_time_sum <- sum(subset_total$TR_AREA)#find total swept area

#- Define the size of the grid cell
resx <- 0.025
resy <- 0.025

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

grd$Low <- 0
grd$Medium<-0
grd$High<-0
grd$Not_relevant<-0
subset_total<-as.data.frame(subset_total)
subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)

idx <- st_over(coords, grd)
subset_total$gridID <- idx

grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x

grd$Low <- (grd$Low) + ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
grd$Medium <- (grd$Medium) + ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
grd$High <- (grd$High) + ((grd$High * time_sum) / (total_time_sum))  # now for time increase
grd$Not_relevant <- (grd$Not_relevant) + ((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase

grd$Low <-grd$Low*1
grd$Medium <-grd$Medium*2
grd$High <-grd$High*3
grd$Not_relevant <-grd$Not_relevant*0

grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant

# Initialize an empty grid
grd_merged <- grd
grd_merged$Low <-0
grd_merged$Medium <-0
grd_merged$High <-0
grd_merged$Not_relevant <-0

vessel_list <- unique(tacsatEflalo$VE_REF)


for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TR_AREA)
  #- Define the size of the grid cell
  resx <- 0.025
  resy <- 0.025
  
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
  
  grd$Low <- 0
  grd$Medium<-0
  grd$High<-0
  grd$Not_relevant<-0
  
  subset_total<-as.data.frame(subset_total)
  subset_total <- intervalTacsat(subset_total, level = "vessel", fill.na = TRUE)
  
  idx <- st_over(coords, grd)
  subset_total$gridID <- idx
  
  grd$Low[an(names(table(idx)))] <- aggregate(subset_total$Low, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$Medium[an(names(table(idx)))] <- aggregate(subset_total$Medium, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$High[an(names(table(idx)))] <- aggregate(subset_total$High, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  grd$Not_relevant[an(names(table(idx)))] <- aggregate(subset_total$Not_relevant, by = list(subset_total$gridID), FUN = sum, na.rm = TRUE)$x
  
  grd$Low <- ((grd$Low * time_sum) / (total_time_sum))  # now for time increase
  grd$Medium <- ((grd$Medium * time_sum) / (total_time_sum))  # now for time increase
  grd$High <- ((grd$High * time_sum) / (total_time_sum))  # now for time increase
  grd$Not_relevant <-((grd$Not_relevant * time_sum) / (total_time_sum))  # now for time increase
  
  grd$Low <-grd$Low*1
  grd$Medium <-grd$Medium*2
  grd$High <-grd$High*3
  grd$Not_relevant <-grd$Not_relevant*0
  
  grd$total<- grd$Low + grd$Medium + grd$High +grd$Not_relevant
  
  grd$Low[is.na( grd$Low)] <- 0
  grd$Low[is.nan( grd$Low)] <- 0
  grd$Medium[is.na(grd$Medium)] <- 0
  grd$Medium[is.nan(grd$Medium)] <- 0
  grd$High[is.na(grd$High)] <- 0
  grd$High[is.nan(grd$High)] <- 0
  grd$Not_relevant[is.na(grd$Not_relevant)] <- 0
  grd$Not_relevant[is.nan(grd$Not_relevant)] <- 0
  grd$total[is.na(grd$total)] <- 0
  grd$total[is.nan(grd$total)] <- 0
  grd_merged$Low <- grd_merged$Low + grd$Low
  grd_merged$Medium <- grd_merged$Medium + grd$Medium
  grd_merged$High <- grd_merged$High + grd$High
  grd_merged$Not_relevant <- grd_merged$Not_relevant + grd$Not_relevant
  grd_merged$total <- grd_merged$total + grd$total
}

save(grd_merged,file="./processed_data/grd_sub_sens.RData")


load("./processed_data/five.RData")#load our areas of interest
load("./processed_data/ten.RData")
load("./processed_data/twenty.RData")
load("./processed_data/boundary.RData")


Hab_five_hole<- st_difference(grd_merged, five_site)#removing squares within HypHPMA

Hab_five_hole <- st_make_valid(Hab_five_hole)
st_crs(Hab_five_hole) <- 4326#setting crs
Hab_five_hole$surface <- st_area(Hab_five_hole)/(1000*1000)#finding area of grid
Hab_five_hole$Total<-Hab_five_hole$Low + Hab_five_hole$Medium + Hab_five_hole$High
Hab_five_hole$Total[Hab_five_hole$Total <= 0] <- NA
Hab_five_hole$sur<-Hab_five_hole$Total/Hab_five_hole$surface


baseMap <- ggplot() +
  geom_sf(data = Hab_five_hole, mapping = aes(fill = as.numeric(sur)), color = NA) +
  scale_fill_viridis(discrete = FALSE, na.value = "transparent", limits = c(0,0.25), name = "SSR (SAB) increase") +
  xlim(x_limits = c(-3.6,-3.2)) +
  ylim(y_limits = c(50.4,50.7)) +
  labs(x = xl$label, y = yl$label) +
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4) +
  geom_sf(data = ten_site, colour = "darkgrey", fill = NA, linetype = "dashed", linewidth= 0.4) +
  geom_sf(data = twenty_site, colour = "darkgrey", fill = NA,linetype = "dotdash", linewidth= 0.4 ) +
  geom_sf(data = boundary, colour = "darkgrey", fill = NA, linetype = "dotted", linewidth= 0.4) +
  geom_sf(data = europa, colour = 1, fill = "grey") +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) 
theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
      axis.title = element_text(face = xl$font, size = rel(xl$cex)))
SUB<-baseMap
save(Hab_five_hole,file="./processed_data/grd_sub_sens_hole.RData")
load("./processed_data/grd_sub_sens_hole.RData")
#Now for confidences, subsurface abrasion
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D2"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- ifelse(tacsatEflalo$Confidence == 1, 3,
                      ifelse(tacsatEflalo$Confidence == 2, 2,
                      ifelse(tacsatEflalo$Confidence == 3, 1, 
                      ifelse(tacsatEflalo$Confidence == 4, 0, 
                      ifelse(tacsatEflalo$Confidence == 0, 0, 0)))))


#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con_in(
  coords, 
  scale_limits = c(0, 3), 
  legend_title = "Pressure confidence (SAB)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,1,2,3), # Specify the breaks
  labels = c("Not relevant", "Low", "Medium", "High")
)
pen_con<-result$map
pen_con
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "conf_Z10_6_D2"#return to normal


#Now for confidence in abrasion
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced

names(Hab_Type)[names(Hab_Type) == "conf_Z10_6_D6"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- ifelse(tacsatEflalo$Confidence == 1, 3,
                      ifelse(tacsatEflalo$Confidence == 2, 2,
                      ifelse(tacsatEflalo$Confidence == 3, 1, 
                      ifelse(tacsatEflalo$Confidence == 4, 0, 
                      ifelse(tacsatEflalo$Confidence == 0, 0, 0)))))


#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
#- Define the size of the grid cell
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con_in(
  coords, 
  scale_limits = c(0, 3), 
  legend_title = "Pressure confidence (AB)", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,1,2,3), # Specify the breaks
  labels = c("Not relevant", "Low", "Medium", "High"))
abr_con<-result$map
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "conf_Z10_6_D6"#return to normal

#Next confidence in biotope
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced

names(Hab_Type)[names(Hab_Type) == "uncertainty_sim"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- tacsatEflalo$Confidence
#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con_in(
  coords, 
  scale_limits = c(0, 1), 
  legend_title = "Confidence in simulation", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,0.25,0.50,0.75,1.0), # Specify the breaks
  labels = c("Very low", "Low", "Moderate", "High","Very high")
)
bio<-result$map
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "uncertainty_sim"#return to normal

#Now for MESH confidence
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
Hab_Type<-st_read("./raw_data/mapsquare.gpkg", layer = "mapsquare")
Hab_Type<- transform_and_make_valid(Hab_Type)#need confidence from hab map

names(Hab_Type)[names(Hab_Type) == "SUM_CONF"] <- "Confidence"#rename your confidence column
result<- Stack_con_over(tacsatEflalo, Hab_Type,Confidence)
tacsatEflalo<-result$tacsatEflalo#associates each ping with a confidence level  
tacsatEflalo$Score <- tacsatEflalo$Confidence

#add all confidences together within the square and divide by total number of pings
#Now they are linked and rescored we need associate swept area ratio with sensitivity and make a heatmap
#for abrasion we use LE_WIDTH (surface abrasion width)
tacsatEflalo$Score[is.na(tacsatEflalo$Score)] <- 0
coords      <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(coords) <- 4326#set coordinates

result <- SweptArea_Con_in(
  coords, 
  scale_limits = c(0, 100), 
  legend_title = "MESH confidence", 
  x_limits = c(-3.6, -3.2), 
  y_limits = c(50.4, 50.7), 
  additional_shapefiles = list(boundary, five_site, ten_site, twenty_site), 
  additional_colors = list("lightgrey", "lightgrey", "lightgrey", "lightgrey"),
  additional_border_patterns = list("dotted", "solid", "dashed", "dotdash"),
  breaks = c(0,20,37,58,79, 100), # Specify the breaks
  labels = c("0","Very low", "Low", "Moderate", "High","Very high")
)
MESH<-result$map
names(Hab_Type)[names(Hab_Type) == "Confidence"] <- "SUM_CONF"#return to normal

SAR<-SAR+
  theme_classic(base_size = 7.5)+
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
SUB<-SUB+
  theme_classic(base_size = 7.5)+
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
abr_con<-abr_con+
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
  theme_classic(base_size = 7.5)+
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
pen_con<-pen_con+
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
  theme_classic(base_size = 7.5)+          
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
bio<-bio+
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
  theme_classic(base_size = 7.5)+          
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
MESH<-MESH+
  geom_sf(data = five_site, colour = "darkgrey", fill = "lightblue",linetype ="solid" , linewidth= 0.4)+
  theme_classic(base_size = 7.5)+          
  theme(panel.background = element_rect(fill = 'lightblue'))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

ggp <- ggarrange(SAR,SUB,abr_con,pen_con,bio,MESH,
                 labels = c("A.", "B.","C.","D.","E.","F."),    # Plot labels
                 ncol = 2, nrow = 3,        # Arrange in a 2x2 grid
                 label.x = -0.01,            # Adjust the x position of the labels
                 label.y = 0.9)  


ggp#combining both plots
#Calculate increase after displacement
load("./processed_data/tacsatEflalo_ID.RData")
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", Total_Time = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  time_sum <- sum(subset_df$TIME)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_time_sum <- sum(subset_total$TIME)
  summary <- subset_vessel%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(Total_Time = sum(TIME))
  summary$Total_Time <- ((summary$Total_Time * time_sum) / (total_time_sum))
  Final_summary<- rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(Total_Time = sum(Total_Time))
#####SAR surface
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  summary <- subset_vessel%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(TR_AREA))
  summary$TR_AREA <- ((summary$TR_AREA * swept_sum) / (total_swept_sum))
  Final_summary<- rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))
five<-as.numeric((st_area(five_site))/1000000)
ten<-as.numeric((st_area(ten_site)-st_area(five_site))/1000000)
twenty<-as.numeric((st_area(twenty_site)-st_area(ten_site))/1000000)
Boundary<-as.numeric((st_area(boundary)-st_area(twenty_site))/1000000)
Boundary<-sum(Boundary)
summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA / twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )
###SUB SAR
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  summary <- subset_vessel%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(TR_AREA))
  summary$TR_AREA <- ((summary$TR_AREA * swept_sum) / (total_swept_sum))
  Final_summary<- rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))
five<-as.numeric((st_area(five_site))/1000000)
ten<-as.numeric((st_area(ten_site)-st_area(five_site))/1000000)
twenty<-as.numeric((st_area(twenty_site)-st_area(ten_site))/1000000)
Boundary<-as.numeric((st_area(boundary)-st_area(twenty_site))/1000000)
Boundary<-sum(Boundary)
summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA / twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )


#Now for SSR
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_WIDTH[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D6"] <- "Pressure"#rename your pressure column
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: abrasion from demersal trawling"
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#penetration
tacsatEflalo<-result$tacsatEflalo#associates each ping with a pressure  
tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                             ifelse(tacsatEflalo$Pressure == 2, 2,
                                    ifelse(tacsatEflalo$Pressure == 3, 1, 
                                           ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                  ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                         ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                       ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                              ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
tacsatEflalo$Total<-tacsatEflalo$Score*tacsatEflalo$TR_AREA
summary <- tacsatEflalo%>%
  group_by(Location) %>%
  summarize(Total = sum(Total))
summary <- st_drop_geometry(summary)
summary <- summary%>%
  mutate(
    Total = case_when(
      Location == "Five" ~ Total / five,
      Location == "Ten" ~ Total / ten,
      Location == "Twenty" ~ Total/ twenty,
      Location == "Boundary" ~ Total / Boundary,
      TRUE ~ Total # Default case, if location doesn't match any of the above
    )
  )

tacsatEflalo <- tacsatEflalo %>%
  mutate(
    High = if_else(Score == 3, TR_AREA, 0),
    Medium = if_else(Score == 2, TR_AREA, 0),
    Low = if_else(Score == 1, TR_AREA, 0),
    Not_relevant = if_else(Score == 0, TR_AREA, 0)
  )#separating based on scores

tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  subset_total$Low <-((subset_total$Low * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Medium <-  ((subset_total$Medium * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$High <-((subset_total$High * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Not_relevant <- ((subset_total$Not_relevant * swept_sum) / (total_swept_sum))  # now for time increase
  
  subset_total$Low<-subset_total$Low*1
  subset_total$Medium <-subset_total$Medium*2
  subset_total$High <-subset_total$High*3
  subset_total$Not_relevant <-subset_total$Not_relevant*0
  subset_total$total<- subset_total$Low + subset_total$Medium + subset_total$High
  
  summary <- subset_total%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(total))
  summary <- st_drop_geometry(summary)
  Final_summary<-rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))

summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Five" ~ TR_AREA / five,
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA/ twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )

#Now for subsurface SSR
load("./processed_data/tacsatEflalo_ID.RData")
load("./processed_data/eflaloClean.RData")##calculate SAR
tacsatEflalo$LE_WIDTH <- eflalo$LE_SUB[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
tacsatEflalo$TR_AREA <- (tacsatEflalo$TIME) * (tacsatEflalo$LE_WIDTH / 1000) * (tacsatEflalo$SI_SP *1.852)
Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)
names(Hab_Type)[names(Hab_Type) == "sens_Z10_6_D2"] <- "Pressure"#rename your pressure column
order_locations <- c("Five", "Ten", "Twenty", "Boundary", "Outside")
Title<-"Sensitivity: penetration from demersal trawling"
result <- Stack_sens_over(tacsatEflalo,Hab_Type,Pressure,Title, order_locations)#penetration
tacsatEflalo<-result$tacsatEflalo#associates each ping with a pressure  
tacsatEflalo$Pressure<-as.numeric(tacsatEflalo$Pressure)
tacsatEflalo$Score <- ifelse(tacsatEflalo$Pressure == 1, 3,
                             ifelse(tacsatEflalo$Pressure == 2, 2,
                                    ifelse(tacsatEflalo$Pressure == 3, 1, 
                                           ifelse(tacsatEflalo$Pressure == 4, 3, 
                                                  ifelse(tacsatEflalo$Pressure == 5, 3, 
                                                         ifelse(tacsatEflalo$Pressure == 6, 0,
                                                                ifelse(tacsatEflalo$Pressure == 7, 0, 
                                                                       ifelse(tacsatEflalo$Pressure == 8, 0, 
                                                                              ifelse(tacsatEflalo$Pressure == 0, 3, 3)))))))))#Next we find the swept area
tacsatEflalo$Total<-tacsatEflalo$Score*tacsatEflalo$TR_AREA
summary <- tacsatEflalo%>%
  group_by(Location) %>%
  summarize(Total = sum(Total))
summary <- st_drop_geometry(summary)
summary <- summary%>%
  mutate(
    Total = case_when(
      Location == "Five" ~ Total / five,
      Location == "Ten" ~ Total / ten,
      Location == "Twenty" ~ Total/ twenty,
      Location == "Boundary" ~ Total / Boundary,
      TRUE ~ Total # Default case, if location doesn't match any of the above
    )
  )


tacsatEflalo <- tacsatEflalo %>%
  mutate(
    High = if_else(Score == 3, TR_AREA, 0),
    Medium = if_else(Score == 2, TR_AREA, 0),
    Low = if_else(Score == 1, TR_AREA, 0),
    Not_relevant = if_else(Score == 0, TR_AREA, 0)
  )#separating based on scores

tacsatEflalo_five <- subset(tacsatEflalo, Location == "Five")
unique_vessels<- unique(tacsatEflalo_five$VE_REF)
tacsatEflalo <- subset(tacsatEflalo, VE_REF %in% unique_vessels)#filter down to 27 vessels displaced
vessel_list <- unique(tacsatEflalo$VE_REF)
Final_summary <- data.frame(Location = "Ten", TR_AREA = 0)
for (vessel in vessel_list) {
  subset_vessel <- tacsatEflalo[tacsatEflalo$VE_REF == vessel, ]
  subset_df <- subset_vessel[subset_vessel$Location == "Five", ]
  swept_sum <- sum(subset_df$TR_AREA)
  subset_total <- subset_vessel[subset_vessel$Location != "Five", ]
  total_swept_sum <- sum(subset_total$TR_AREA)
  subset_total$Low <-((subset_total$Low * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Medium <-  ((subset_total$Medium * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$High <-((subset_total$High * swept_sum) / (total_swept_sum))  # now for time increase
  subset_total$Not_relevant <- ((subset_total$Not_relevant * swept_sum) / (total_swept_sum))  # now for time increase
  
  subset_total$Low<-subset_total$Low*1
  subset_total$Medium <-subset_total$Medium*2
  subset_total$High <-subset_total$High*3
  subset_total$Not_relevant <-subset_total$Not_relevant*0
  subset_total$total<- subset_total$Low + subset_total$Medium + subset_total$High
  
  summary <- subset_total%>%
    filter(Location != "Five") %>%
    group_by(Location) %>%
    summarize(TR_AREA = sum(total))
  summary <- st_drop_geometry(summary)
  Final_summary<-rbind(Final_summary,summary)}

summary <-  Final_summary%>%
  group_by(Location) %>%
  summarize(TR_AREA = sum(TR_AREA))

summary <- summary%>%
  mutate(
    TR_AREA = case_when(
      Location == "Five" ~ TR_AREA / five,
      Location == "Ten" ~ TR_AREA / ten,
      Location == "Twenty" ~ TR_AREA/ twenty,
      Location == "Boundary" ~ TR_AREA / Boundary,
      TRUE ~ TR_AREA # Default case, if location doesn't match any of the above
    )
  )
#Fix flowchart
#Plan for pots and nets
#filter down to FPO, GN, GND, GNS
#find total catch for potting and netting, find top species weight and value caught and most common ports
