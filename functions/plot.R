
ggplot() + 
  geom_sf(data = world, fill = "darkgreen", col=NA) +
  geom_sf(data = boundary, fill = "lightblue", col=NA) +
  geom_sf(data = twenty_site, fill = "yellow", col=NA) +
  geom_sf(data = ten_site, fill = "blue", col=NA) +
  geom_sf(data = five_site, fill = "red", col=NA) +
  coord_sf(xlim = c(-10, 2), ylim = c(49, 59)) +
  theme(legend.position="none")+
  xlab("Longitude") + ylab("Latitude")

p <- plotTools(tacsatEflalo,level="gridcell",xlim=c(-5,0),ylim=c(49,52),zlim=NULL,log=F,gridcell=c(0.1,0.1),color=NULL,control.tacsat=list(clm="LE_TOT_VAL"))
p[[1]] +
  ggtitle("Catch value (£)")+
  geom_sf(data = boundary, aes(fill = 1), colour = "light blue", fill = NA)+
  geom_sf(data = five_site, aes(fill = 1), colour = "black", fill = NA)+
  geom_sf(data = ten_site, aes(fill = 1), colour = "blue", fill = NA)+
  geom_sf(data = twenty_site, aes(fill = 1), colour = "yellow", fill = NA)#plotting depending on total catch value

p <- plotTools(tacsatEflalo,level="gridcell",xlim=c(-5,0),ylim=c(49,52),zlim=NULL,log=F,gridcell=c(0.1,0.1),color=NULL,control.tacsat=list(clm="LE_TOT_KG"))
p[[1]] +
  ggtitle("Catch weight")+
  geom_sf(data = boundary, aes(fill = 1), colour = "light blue", fill = NA)+
  geom_sf(data = five_site, aes(fill = 1), colour = "black", fill = NA)+
  geom_sf(data = ten_site, aes(fill = 1), colour = "blue", fill = NA)+
  geom_sf(data = twenty_site, aes(fill = 1), colour = "yellow", fill = NA)#plotting depending on total catch weight

p <- plotTools(tacsatEflalo,level="gridcell",xlim=c(-5,0),ylim=c(49,52),zlim=NULL,log=F,gridcell=c(0.1,0.1),color=NULL,control.tacsat=list(clm="TIME"))
p[[1]] +
  ggtitle("Time (mins)")+
  geom_sf(data = boundary, aes(fill = 1), colour = "light blue", fill = NA)+
  geom_sf(data = five_site, aes(fill = 1), colour = "black", fill = NA)+
  geom_sf(data = ten_site, aes(fill = 1), colour = "blue", fill = NA)+
  geom_sf(data = twenty_site, aes(fill = 1), colour = "yellow", fill = NA)#plotting depending on total time spent fishing

#We can also plot weights, values and pings depending on gear type 

DRB <- subset(tacsatEflalo,LE_GEAR == "DRB")
OTB <- subset(tacsatEflalo,LE_GEAR == "OTB")
FPO <- subset(tacsatEflalo,LE_GEAR == "FPO")

p <- plotTools(OTB,level="gridcell",xlim=c(-5,0),ylim=c(49,52),zlim=NULL,log=F,gridcell=c(0.1,0.1),color=NULL,control.tacsat=list(clm="LE_TOT_VAL"))
p[[1]] +
  ggtitle("OTB (£)")+
  geom_sf(data = five_site, aes(fill = 1), colour = "black", fill = NA)+
  geom_sf(data = ten_site, aes(fill = 1), colour = "blue", fill = NA)+
  geom_sf(data = twenty_site, aes(fill = 1), colour = "yellow", fill = NA)#total catch value for OTB

p <- plotTools(FPO,level="gridcell",xlim=c(-5,0),ylim=c(49,52),zlim=NULL,log=F,gridcell=c(0.1,0.1),color=NULL,control.tacsat=list(clm="LE_TOT_VAL"))
p[[1]] +
  ggtitle("FPO (£)")+
  geom_sf(data = five_site, aes(fill = 1), colour = "black", fill = NA)+
  geom_sf(data = ten_site, aes(fill = 1), colour = "blue", fill = NA)+
  geom_sf(data = twenty_site, aes(fill = 1), colour = "yellow", fill = NA)#total catch value for FPO
#These are some simple plots we will do some more detailed ones later