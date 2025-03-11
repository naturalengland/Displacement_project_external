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

five_site_bb2 = st_buffer(five_site_bb, 5000)
plot(five_site_bb2, col = "grey")
plot(five_site[0], add = TRUE)

five_site_bb3 = st_difference(five_site_bb2, five_site)
plot(five_site_bb3, col = "grey")+
plot(five_site[0], add = TRUE)



#~~~~~ st_nearest_points() ~~~~~

system.time(
  points %>% 
    mutate(
      my_linestring = st_nearest_points(geometry, five_site_bb),
      closest_point = st_cast(my_linestring, 'POINT')[seq(2, nrow(.)*2, 2)],
      distance = st_distance(geometry, five_site_bb)[,1],
      snapped_point_cond = st_sfc(ifelse(as.numeric(distance) <= 400, st_geometry(closest_point), geometry), crs = st_crs(five_site_bb))
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
geom_sf(data = five_site, fill = 'red')+
geom_sf(data = moved_points, shape = 1, col = 'blue')+
  coord_sf(xlim = c(-3.4, -3.32), ylim = c(50.52, 50.58)) +
  xlab("Longitude") + ylab("Latitude")