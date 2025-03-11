install_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}

# List of required packages
required_packages <- c("cowplot", "ggplot2","ggmap","sf", "sp", "gridExtra", "tmap", "icesVocab", 
                       "readxl", "tidyverse", "segmented", "mixtools", "tcltk", "grid","ggpubr",
                       "RColorBrewer", "ggthemes", "lubridate", "viridis", 
                       "raster", "maps", "mapdata", "dplyr", "rnaturalearth", "ggspatial", "scales","patchwork")
