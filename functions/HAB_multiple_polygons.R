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

Hab_five_hole<- st_difference(Hab_Type, five_site)#removing hab polygons within HypHPMA

habitat_types <- unique(points$HAB_TYPE)#finding unique hab types


ten<- st_intersection(Hab_five_hole, ten_site)#cut down to ten site save time
Hab_five_hole <- st_make_valid(ten)
st_crs(Hab_five_hole) <- 4326#setting crs



combined_results <- data.frame(
  HAB_TYPE = character(),  # adjust column names accordingly
  geometry = st_sfc()  # initialize an empty geometry column
)

polygons_filtered <- Hab_five_hole %>%
  filter(HAB_TYPE == 'A5.44')
points_filtered <- points[points$HAB_TYPE == 'A5.44', ]

roads<-polygons_filtered 
my_points<-points_filtered 

for (habitat in habitat_types) {
  # Filter points for the current habitat type
  my_points <- points[points$HAB_TYPE == habitat, ]
  # Filter polygons for the same habitat type
  roads <- Hab_five_hole[Hab_five_hole$HAB_TYPE == habitat, ]
closest_points <- my_points %>% 
  rowwise() %>%
  mutate(
    ##  Get the nearest river segment linestring:
    nearest_segment = roads[st_nearest_feature(geometry, 
                                               roads),],
    ## Get the linestrings between each point and the closest segment:
    line_to_point = st_nearest_points(geometry, nearest_segment),
    ##  Retrieve the point from the line sf that was returned:
    closest_point = st_cast(line_to_point, 'POINT')[2],
    ##  Calculate the distance between the old and new point:     
    distance = st_distance(geometry, closest_point)[,1],
    ##  If under our limit of 100m distant, adopt the new geometry, 
    ##  else keep the original
    snapped_point_cond = st_sfc(ifelse(as.numeric(distance) <= 1, 
                                       st_geometry(closest_point),
                                       geometry), 
                                crs = st_crs(roads)))
combined_results <- rbind(combined_results, closest_points)
}




ggplot() +
  geom_sf(data = Hab_five_hole, aes(color = Hab_five_hole$HAB_TYPE))+
  geom_sf(data = combined_results$closest_point, aes(color = combined_results$HAB_TYPE))+
  coord_sf(xlim = c(-3.4, -3.32), ylim = c(50.52, 50.58))+
  theme(legend.position = "none")



