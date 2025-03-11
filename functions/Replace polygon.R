Hab_Type1<-st_read("./raw_data/UKSeaMap_polygon_issue_100225.gdb")
Hab_Type1<- transform_and_make_valid(Hab_Type1)

Hab_Type2<-st_read("./processed_data/MHabS1.gpkg", layer = "Inshore_BGR_displacement")
Hab_Type2<- transform_and_make_valid(Hab_Type2)

Hab_Type_inter<-st_difference(Hab_Type2, st_union(Hab_Type1))
Hab_Type_inter<- transform_and_make_valid(Hab_Type_inter)
Hab_Type<-st_union(Hab_Type_inter,Hab_Type1)

ggplot() +
  geom_sf(data = europa, fill = "grey", color = "darkgrey") +
  geom_sf(data=all_schools, aes(fill = Eunis_L3), color = "black", na.rm = TRUE)+
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-3.6, -3.2), ylim = c(50.4, 50.7))

protected_site <- all_schools %>%
  filter(Dataset_source_ID %in% c("NE_2103_358529"))

all_schools <- rbind(Hab_Type_inter,Hab_Type1)

# Specify path for your GDB (make sure the directory exists)
gdb_path <- "./processed_data/MHabS3.gpkg"

# Write the sf object to the GDB

st_write(all_schools, dsn = "./processed_data/MHabS1.gpkg", layer = "Inshore_BGR_displacement_updated", delete_layer = TRUE)

Hab_Type1 <- Hab_Type1[, !colnames(Hab_Type1) %in% c("Shape_Length", "Shape_Area")]

Hab_Type1$CP2_name <- "Eastern Channel"
Hab_Type1$CP2_id <- 3
Hab_Type1$SubReg_id <- "3c"
Hab_Type1<- st_set_geometry(Hab_Type1, "geom")