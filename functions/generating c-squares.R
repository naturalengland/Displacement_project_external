#You will require the VMStools package and associated packages
library(vmstools)
source("functions//packages.R")

# Install and load required packages
for (package in required_packages) {
  install_and_load_package(package)
}
data(ICESareas)

#How to generate c-squares
resx        <- 0.05
resy        <- 0.05#set the resolution of c-square required
areaInt <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
areaRef <- subset(ICESareas, Area_27 %in% c("4.a","4.b","4.c","7.d","7.e","7.h","7.f","7.g","7.a","6.a","7.b","7.j.2"))
#set your bounding box based on the ICES areas requires-this surrounds the UK
bbox <- cbind(matrix(st_bbox(areaInt), ncol = 2),
              matrix(st_bbox(areaRef), ncol = 2))#generate bounding box
rownames(bbox) <- c("x", "y")
spatBound <- list(xrange = c(floor(range(bbox["x", ])[1]), ceiling(range(bbox["x", ])[2])),
                  yrange = c(floor(range(bbox["y", ])[1]), ceiling(range(bbox["y", ])[2])))
grd <- createGrid(spatBound$x, spatBound$y, resx, resy, type = "GridDF", exactBorder = T)#create grid based on bounding box with the required resolution
