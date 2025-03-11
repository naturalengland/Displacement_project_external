library(sf)
library(terra)
library(tmap)
library(spData)
load("./processed_data/tacsatlocation.RData")#loading tacsateflalo based on loc
tacsatEflalo <- st_as_sf(tacsatEflalo,coords=c("SI_LONG","SI_LATI"))
st_crs(tacsatEflalo) <- 4326

Hab_Type<-st_read("./raw_data/mapsquare.gpkg", layer = "mapsquare")
Hab_Type <- st_make_valid(Hab_Type)
st_crs(Hab_Type) <- 4326#setting crs

idx     <- st_over(tacsatEflalo,Hab_Type)
tacsatEflalo$HAB_TYPE <- Hab_Type$HAB_TYPE[idx]

five_site <- st_read("./raw_data/5km.shp")
five_site <- transform_and_make_valid(five_site)



x<- subset(tacsatEflalo,Ping_Location == "Five")#filtering down to points within HypHPMA
y<- filter(tacsatEflalo,Ping_Location != "Five")#filtering to points outside HypHPMA

points  <- st_as_sf(x,coords=c("SI_LONG","SI_LATI"))
other_points  <- st_as_sf(y,coords=c("SI_LONG","SI_LATI"))
st_crs(points) <- 4326
st_crs(other_points) <- 4326#setting crs

#~~~~~ st_nearest_points() ~~~~~


Hab_five_hole<- st_difference(Hab_Type, five_site)#removing hab polygons within HypHPMA

habitat_types <- unique(points$HAB_TYPE)#finding unique hab types


ten<- st_intersection(Hab_five_hole, ten_site)#cut down to ten site save time
Hab_five_hole <- st_make_valid(ten)
st_crs(Hab_five_hole) <- 4326#setting crs

combined_results <- data.frame(
  HAB_TYPE = character(),  # adjust column names accordingly
  geometry = st_sfc()  # initialize an empty geometry column
)
for (habitat in habitat_types) {
  # Filter points for the current habitat type
  points_filtered <- points[points$HAB_TYPE == habitat, ]
  
  # Filter polygons for the same habitat type
  polygons_filtered <- Hab_five_hole[Hab_five_hole$HAB_TYPE == habitat, ]
  points_filtered %>% 
    mutate(
      my_linestring = st_nearest_points(geometry, polygons_filtered),
      closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
      distance = st_distance(geometry, polygons_filtered)[,1],
      snapped_point_cond = st_sfc(ifelse(as.numeric(distance) <= 400, st_geometry(closest_point), geometry), crs = st_crs(polygons_filtered))
    ) %>% 
    {. ->> closest_points_f1}
  combined_results <- rbind(combined_results, closest_points_f1)
}

pts <- st_network_blend(polygons_filtered, pts)



baseMap <- ggplot() + 
  geom_sf(data=europa,colour=1,fill=colgrey[5]) + 
  xlim(-3.6,-3.2)+
  ylim(50.4,50.7)+
  labs(x=xl$label,y=yl$label) + 
  theme_bw() +
  theme(plot.margin      = unit(c(10,10,10,10),"mm"),
        axis.title       = element_text(face = xl$font,size = rel(xl$cex))) 
baseMap + 
  geom_sf(data = boundary, fill = "light blue")+
  geom_sf(data = pts, aes(col = HAB_TYPE.y), size = 1)+
  geom_sf(data = five_site, aes(fill = 1), colour = "red", fill = NA)+
  geom_sf(data = ten_site, aes(fill = 1), colour = "blue", fill = NA)+
  geom_sf(data = twenty_site, aes(fill = 1), colour = "yellow", fill = NA)+
  scale_color_manual(name= "Habitat type", 
                     values = c("A5.3" = "#C8830A",
                                "A5.4" = "#DDBF29",
                                "A5.441" ="#E86F12",
                                "A5.511" = "#C78FAA",
                                "Other" = "gray"
                     ), labels = c("A5.35", "A5.44", "Other"))+
  ggtitle("Habitat type: EUNIS Classification") #plotted pings based on habitat.


pts <- st_network_blend( polygons_filtered$geom, pts$geometry )










#also opportunity to add a buffer on based on latest model, so vessels move to outside the buffer
five_site <- st_read("./raw_data/5km.shp")
five_site <- transform_and_make_valid(five_site)
load("./processed_data/tacsatlocation.RData")
x<- subset(tacsatEflalo,Ping_Location == "Five")
y<- filter(tacsatEflalo,Ping_Location != "Five")
points  <- st_as_sf(x,coords=c("SI_LONG","SI_LATI"))
other_points  <- st_as_sf(y,coords=c("SI_LONG","SI_LATI"))
st_crs(points) <- 4326
st_crs(other_points) <- 4326
library(sf)
library(terra)
library(tmap)
library(spData)

tm_shape(five_site) +
  tm_polygons() +
  tm_shape(points) +
  tm_symbols() +
  tm_add_legend("symbol", labels = "Inside points")
plot(five_site[,1])

five_site_bb = st_as_sfc(st_bbox(five_site))
plot(five_site_bb)
plot(five_site[0], add = TRUE)

five_site_bb2 = st_buffer(five_site_bb, 1)
plot(five_site_bb2, col = "grey")
plot(five_site[0], add = TRUE)

five_site_bb3 = st_difference(five_site_bb2, five_site)
plot(five_site_bb3, col = "grey")+
  plot(five_site[0], add = TRUE)

tm_shape(five_site_bb) +
  tm_polygons() +
  tm_shape(points) +
  tm_symbols() 

#~~~~~ st_nearest_points() ~~~~~

system.time(
  points %>% 
    mutate(
      my_linestring = st_nearest_points(geometry, Hab_five_hole),
      closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
      distance = st_distance(geometry, Hab_five_hole)[,1],
      snapped_point_cond = st_sfc(ifelse(as.numeric(distance) <= 400, st_geometry(closest_point), geometry), crs = st_crs(Hab_five_hole))
    ) %>% 
    {. ->> closest_points_f1}
)
# <0.2 secs


ggplot()+
  geom_sf(data = five_site, col = 'red')+
  geom_sf(data = closest_points_f1$snapped_point_cond, shape = 1, col = 'blue')

st_geometry(closest_points_f1) <- "snapped_point_cond"
closest_points_f1 = subset(closest_points_f1, select = -c(144,145,146,147) )

st_geometry(closest_points_f1) <- "geometry"

moved_points<-rbind(other_points,closest_points_f1)

ggplot()+
  geom_sf(data = five_site, fill = 'grey')+
  geom_sf(data = moved_points, shape = 1, col = 'blue')+
  coord_sf(xlim = c(-3.4, -3.32), ylim = c(50.52, 50.58)) +
  xlab("Longitude") + ylab("Latitude")