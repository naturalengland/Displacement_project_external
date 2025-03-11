library("scatterpie")
ports <- read.csv("./raw_data/ports.csv")
n <- nrow(ports)
ggplot() + 
  geom_sf(data = europa, colour = 1, fill = "darkgreen") +
  geom_sf(data = five_site, colour = "red", fill = NA) +
  geom_sf(data = ten_site, colour = "blue", fill = NA) +
  geom_sf(data = twenty_site, colour = "yellow", fill = NA) +
  geom_sf(data = boundary, colour = "darkblue", fill = NA) +
  geom_scatterpie(aes(x=Long, y=Lat, group=region,r=Rad/100),
                  data=ports, cols=c("Potting", "Netting"), color=NA, alpha=.8) +
  geom_scatterpie_legend(ports$Rad/100, x=-3.12, y=50.15, labeller=function(x) round(((x * 100)^2)*pi)) +
  coord_sf(xlim = c(-3.8, -3),
           ylim = c(50.1,50.7))+
  xlab("Longitude") + ylab("Latitude")+
  ggtitle("Static fishing permits")+
  labs(fill="Gear Type")+
  theme_bw() +
  theme(plot.margin = unit(c(10, 10, 10, 10), "mm"),
        axis.title = element_text(face = xl$font, size = rel(xl$cex)))
