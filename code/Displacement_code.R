library(vmstools)
library(maps);library(mapdata)
library("cowplot")
library("ggplot2")
library("sf")
library("sp")
library("dplyr")
library(gridExtra)
library(tmap)
library("readxl")
library("tidyverse")
require(segmented)
require(mixtools)
require(tcltk)
require(RColorBrewer)
require(ggthemes)
require(lubridate)
library(viridis)
data(europa) 
class(europa)
data(ICESareas)#All packages and data required for our code

#Defining our areas of interest
HPMA_site <- st_polygon(list(cbind(c(-3.39819,-3.39819,-3.32769, -3.32769,-3.39819), c(50.57261, 50.52761, 50.52761,50.57261, 50.57261))))
ten_site <- st_polygon(list(cbind(c(-3.43, -3.43,-3.29,-3.29,-3.43), c(50.59, 50.50,50.50, 50.59, 50.59))))
twenty_site <- st_polygon(list(cbind(c(-3.50, -3.50,-3.22, -3.22,-3.50), c(50.64,50.45,50.45,50.64,50.64))))

#Making valid

#Saving these for later use
