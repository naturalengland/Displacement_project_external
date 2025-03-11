library(vmstools)

source("functions//packages.R")

# Install and load required packages
for (package in required_packages) {
  install_and_load_package(package)
}

data(europa) #the following data is also required
class(europa)
data(ICESareas)
data("World")#map of the world
world <- ne_countries(scale = "large", returnclass = "sf")#boundaries using natural earth
source("functions//setcrs_makevalid.R")

resx        <- 0.01
resy        <- 0.01

areaInt <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
areaRef <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))

bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
              matrix(st_bbox(areaRef), ncol = 2))
rownames(bbox) <- c("x", "y")
spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                  yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
grd <- createGrid(spatBound$x, spatBound$y, resx, resy, type = "GridDF", exactBorder = T)

st_crs(grd) <- 4326

ggplot() + 
  geom_sf(data = grd, color = "blue", fill = NA) +   
  geom_sf(data = europa, colour = "grey", fill = "grey")+
  xlim(c(-3.6, -3.2)) +                                                             
  ylim(c(50.4, 50.7)) #for squares

bbox <- st_sfc(st_polygon(list(rbind(c(spatBound$x[1],spatBound$y[1]),     #for hexagon grid       
                                     c(spatBound$x[2],spatBound$y[1]),            
                                     c(spatBound$x[2],spatBound$y[2]),            
                                     c(spatBound$x[1],spatBound$y[2]),            
                                     c(spatBound$x[1],spatBound$y[1])))))        
grd <- st_make_grid(bbox,cellsize=c(resx,resy),square=F)
grd <- st_as_sf(data.frame(grID=1:length(grd)),grd)
st_crs(grd) <- 4326
load("./processed_data/twenty.RData")
twenty_site <- st_transform(twenty_site, 4326)
grd_twenty<- st_intersection(grd, twenty_site)
grd_twenty <- grd_twenty %>%
  mutate(id = row_number())
ggplot() + 
  geom_sf(data = grd_twenty, color = "blue", fill = NA) +   
  geom_sf(data = europa, colour = "grey", fill = "grey")+
  xlim(c(-3.6, -3.2)) +                                                             
  ylim(c(50.4, 50.7)) #for hexagon plot

Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
Hab_Type<- transform_and_make_valid(Hab_Type)

Hab_twenty<- st_intersection(Hab_Type,twenty_site)
Hab_small<- st_intersection(Hab_twenty,grd_twenty)

Hab_small$sens_Z10_6_D2[is.na(Hab_small$sens_Z10_6_D2)] <- 9
  ggplot() + 
  theme_bw() +
  geom_sf(data = Hab_small, mapping = aes(fill = as.factor(sens_Z10_6_D2))) +
  geom_sf(data = europa, fill = "grey") + 
  scale_fill_manual(name = "Sensitivity", values = c("9" = "#440154",
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
  xlim(c(-3.6, -3.2)) +                                                             
  ylim(c(50.4, 50.7)) #for hexagon plot
  Hab_small <- st_transform(Hab_small, 3035)
  Hab_small$area<-st_area(Hab_small)
  #rescoring sens for composite scoring
  Hab_small <-  Hab_small %>%
    mutate(sensitivity = case_when(
      sens_Z10_6_D2 == 1 ~ 3,   # Replace 3 with 1
      sens_Z10_6_D2 == 2 ~ 2,  
      sens_Z10_6_D2 == 3 ~ 1,
      sens_Z10_6_D2 == 4 ~ 3,
      sens_Z10_6_D2 == 5 ~ 3,
      sens_Z10_6_D2 == 6 ~ 0,
      sens_Z10_6_D2 == 7 ~ 0,
      sens_Z10_6_D2 == 8 ~ 0,
      sens_Z10_6_D2 == 9 ~ 3,
    ))#for penetration for demersal trawling
  Hab_small <- Hab_small %>%
    mutate(sens_area = area * sensitivity)
  #now we need to find the total sens_area of the polygon and divide by the total area of the polygon
  
  
  Hab_sum  <- Hab_small %>%
    group_by(id.1) %>%
    summarise(total_area = sum(area, na.rm = TRUE),
              total_sens_area = sum(sens_area, na.rm = TRUE))
  
  Hab_sum$total_sens_area <- gsub(" m²", "", Hab_sum$total_sens_area) 
  Hab_sum$total_area <- gsub(" m²", "", Hab_sum$total_area) 
  Hab_sum$Composite <-as.numeric(Hab_sum$total_sens_area)/as.numeric(Hab_sum$total_area)
  
  Hab_sum1 <- st_transform(Hab_sum, 4326)
  Hab_sum1<-st_drop_geometry(Hab_sum1)
  Hab_sum1$id<-Hab_sum1$id.1
  
  grd_joined <- grd_twenty %>%
    inner_join(Hab_sum1, by = "id")#joining by id for just the grid
  
  ggplot() + 
    theme_bw() +
    geom_sf(data = grd_joined, mapping = aes(fill = Composite)) +
    geom_sf(data = europa, fill = "grey") + 
    scale_fill_viridis(name = "Composite score",option = "C", direction = 1, breaks = c(0.01, 1, 2, 3), 
                       labels = c("Not sensitive", "Low", "Medium", "High"))+
    labs(title = "Sensitivity for penetration from demersal trawling" )+
    xlim(c(-3.6, -3.2)) +                                                             
    ylim(c(50.4, 50.7)) #for hexagon plot
  
##########################
#Now we do this for all sensitivities
  library(vmstools)
  
  source("functions//packages.R")
  
  # Install and load required packages
  for (package in required_packages) {
    install_and_load_package(package)
  }
  
  data(europa) #the following data is also required
  class(europa)
  data(ICESareas)
  data("World")#map of the world
  world <- ne_countries(scale = "large", returnclass = "sf")#boundaries using natural earth
  source("functions//setcrs_makevalid.R")
  
  resx        <- 0.01
  resy        <- 0.01
  
  areaInt <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
  areaRef <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
  
  bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
                matrix(st_bbox(areaRef), ncol = 2))
  rownames(bbox) <- c("x", "y")
  spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                    yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
#Remake the grid
  bbox <- st_sfc(st_polygon(list(rbind(c(spatBound$x[1],spatBound$y[1]),     #for hexagon grid       
                                       c(spatBound$x[2],spatBound$y[1]),            
                                       c(spatBound$x[2],spatBound$y[2]),            
                                       c(spatBound$x[1],spatBound$y[2]),            
                                       c(spatBound$x[1],spatBound$y[1])))))        
  grd <- st_make_grid(bbox,cellsize=c(resx,resy),square=F)
  grd <- st_as_sf(data.frame(grID=1:length(grd)),grd)
  st_crs(grd) <- 4326
  load("./processed_data/twenty.RData")
  twenty_site <- st_transform(twenty_site, 4326)
  grd_twenty<- st_intersection(grd, twenty_site)
  grd_twenty <- grd_twenty %>%
    mutate(id = row_number())
  # lets rescore all our columns
  Hab_Type<-st_read("./raw_data/clipped_NESSST_outputs.gpkg", layer = "BenHabSens_fishing_Filtered_inshore")
  Hab_Type<- transform_and_make_valid(Hab_Type)
  Hab_twenty<- st_intersection(Hab_Type,twenty_site)
  Hab_small<- st_intersection(Hab_twenty,grd_twenty)
  Hab_small <- st_transform(Hab_small, 3035)
  Hab_small$area<-st_area(Hab_small)
  Hab_small <- Hab_small %>%
    mutate(across(contains("sens"), ~ replace(., is.na(.), 9)))
  Hab_small <- Hab_small%>%#rescoring all columns containing sens for composite scoring
    mutate(across(contains("sens"), ~ recode(., `1` = 3, `2` = 2, `3` = 1, `4` = 3,`5` = 3,`6` = 0,`7` = 0,`8` = 0,`9` = 3,`0` = 3,.default = 3)))
  
 # now we rescore for the confidences for each sensitivity and make sure any NAs are set to 0
  Hab_small <- Hab_small %>%
  mutate(across(contains("conf"), ~ replace(., is.na(.), 0)))
   Hab_small <- Hab_small%>%#rescoring all columns containing sens for composite scoring
    mutate(across(contains("conf"), ~ recode(., `1` = 3, `2` = 2, `3` = 1, `4` = 0,`0` = 0,.default = 0)))
   Hab_small <- Hab_small %>%
     mutate(across(contains("uncertainty_sim"), ~ replace(., is.na(.), 0)))
   # Identify columns containing 'sens' or 'conf'
   sens_conf_columns <- grep("sens|conf|uncertainty_sim", names(Hab_small), value = TRUE)
   
   Hab_grouped <- Hab_small %>%
     mutate(across(all_of(sens_conf_columns), ~ . * area)) %>% # Multiply sens and conf columns by area
     group_by(id.1) %>% # Group by id
     summarise(across(c(all_of(sens_conf_columns), area), sum, na.rm = TRUE), .groups = "drop") # Sum selected columns
   

   Hab_grouped <- Hab_grouped %>%
     mutate(across(c(sens_conf_columns, "area"), 
                   ~ as.numeric(gsub(" m²", "", .))))#removing units
   
   Hab_grouped <- Hab_grouped %>%
     mutate(across(all_of(sens_conf_columns), 
                   ~ . / area))#dividing by area
  
   Hab_grouped <- st_transform(Hab_grouped, 4326)
   Hab_grouped <-st_drop_geometry(Hab_grouped)
   Hab_grouped$id<-Hab_grouped$id.1
   
   grd_joined <- grd_twenty %>%
     inner_join(Hab_grouped, by = "id")#joining by id for just the grid
   #now you can plot for confidences or sensitivities
   #Sensitivity for penetration from demersal trawling
   ggplot() + 
     theme_bw() +
     geom_sf(data = grd_joined, mapping = aes(fill = sens_Z10_6_D2)) +
     geom_sf(data = europa, fill = "grey") + 
     scale_fill_viridis(name = "Composite score",option = "C", direction = 1, breaks = c(0.01, 1, 2, 3), 
                        labels = c("Not sensitive", "Low", "Medium", "High"))+
     labs(title = "Sensitivity for penetration from demersal trawling" )+
     xlim(c(-3.6, -3.2)) +                                                             
     ylim(c(50.4, 50.7)) #for hexagon plot
   
   #Confidence for penetration from demersal trawling
   
   ggplot() + 
     theme_bw() +
     geom_sf(data = grd_joined, mapping = aes(fill = conf_Z10_6_D2)) +
     geom_sf(data = europa, fill = "grey") + 
     scale_fill_viridis(name = "Confidence",option = "C", direction = 1, breaks = c(0.01, 1, 2, 3), 
                        labels = c("None", "Low", "Medium", "High"))+
     labs(title = "Confidence in sensitivity assessment: penetration from demersal trawling" )+
     xlim(c(-3.6, -3.2)) +                                                             
     ylim(c(50.4, 50.7)) #for hexagon plot
   
   
  #uncertainty in sim
   
   ggplot() + 
     theme_bw() +
     geom_sf(data = grd_joined, mapping = aes(fill = uncertainty_sim)) +
     geom_sf(data = europa, fill = "grey") + 
     scale_fill_viridis(name = "Confidence",option = "C", direction = 1, breaks = c(0.015, 0.25, 0.5, 0.75, 1), 
                        labels = c("Very low", "Low", "Moderate", "High", "Very high"))+
     labs(title = "Confidence in in biotope assignment" )+
     xlim(c(-3.6, -3.2)) +                                                             
     ylim(c(50.4, 50.7)) #for hexagon plot