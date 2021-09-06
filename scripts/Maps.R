library(ggplot2);library(sf);library(rgdal);library(raster);library(ggpolypath);library(ggrepel)

NZ_coast<-readOGR("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")
NZ_lakes<-readOGR("./shapefiles", layer = "nz-lake-polygons-topo-150k")
big_lakes<-subset(NZ_lakes, !is.na(name_ascii))
protected_areas<-readOGR("./shapefiles", layer = "protected-areas")
CRS.latlon<-CRS("+proj=longlat +datum=WGS84 +no_defs")
protected_areas<-sp::spTransform(protected_areas, CRS.latlon)
natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))

bathy<-readOGR("./shapefiles", layer = "NZBathy_2016_contours")

alliso50<-sp::spTransform(subset(bathy, ELEVATION == -50), CRS.latlon)
alliso200<-sp::spTransform(subset(bathy, ELEVATION == -200), CRS.latlon)
alliso1000<-sp::spTransform(subset(bathy, ELEVATION == -1000), CRS.latlon)

base<-ggplot()+
  geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.9, fill = "white")+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab("Longitude")+
  ylab("Latitude")

fiord_fill = c("Marine Reserve" = "orange")

device<-data.frame(lat = c(-46.035,-46.08,-45.395,-45.395,-45.105,-45.1), lon = c(166.54,166.67,166.815,166.825,167.03,167.14), type = c("SoundTrap","SoundTrap","SoundTrap","FPOD","FPOD","FPOD"))
 
type_color = c("SoundTrap" = "red","FPOD" = "purple")

chalk_pres<-base+
  geom_polygon(mpa, mapping = aes(long,lat,group = group, fill = "Marine Reserve"), alpha = 1)+
  geom_path(alliso200, mapping = aes(long,lat,group = group), color = "steelblue", alpha = 0.7, size = 0.2)+
  geom_path(alliso50, mapping = aes(long,lat,group = group), color = "black", alpha = 0.9, size = 0.2)+
  scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.83, 0.12),
        legend.title = element_blank(),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 8),
        legend.spacing.y = unit(-0.02, "cm"),
        legend.box.background = element_rect(color = "white",fill = "white"),
        legend.key = element_rect(fill = NA),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7))+
  coord_sf(xlim = c(166.45,166.95), ylim = c(-46.18,-45.85), crs = 4269)+
  geom_point(device, mapping = aes(x = lon, y = lat, color = type), alpha = 0.8)+
  scale_color_manual(values = type_color)

midfiord<-base+
  geom_polygon(mpa, mapping = aes(long,lat,group = group, fill = "Marine Reserve"), alpha = 1)+
  geom_path(alliso200, mapping = aes(long,lat,group = group), color = "steelblue", alpha = 0.7, size = 0.2)+
  geom_path(alliso50, mapping = aes(long,lat,group = group), color = "black", alpha = 0.9, size = 0.2)+
  scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.83, 0.12),
    legend.title = element_blank(),
    legend.margin = margin(c(1, 1, 1, 1)),
    legend.key.size = unit(0.2, 'cm'),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(-0.02, "cm"),
    legend.box.background = element_rect(color = "white",fill = "white"),
    legend.key = element_rect(fill = NA),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7))+
  coord_sf(xlim = c(166.7,167.3), ylim = c(-45.02,-45.46), crs = 4269)+
  geom_point(device, mapping = aes(x = lon, y = lat, color = type), alpha = 0.8)+
  scale_color_manual(values = type_color)

deploy<-ggpubr::ggarrange(chalk_pres,midfiord, common.legend = T, legend = "bottom", labels = "AUTO", ncol = 1)

ggsave("./figures/deploy_DOC.svg", deploy, dpi = 320, width = 250, units = 'mm')
ggsave("./figures/deploy_DOC.png", deploy, dpi = 320, height = 150, units = 'mm')

##DUSKY
base+
  geom_polygon(mpa, mapping = aes(long,lat,group = group, fill = "Marine Reserve"), alpha = 1)+
  geom_path(alliso200, mapping = aes(long,lat,group = group), color = "steelblue", alpha = 0.7, size = 0.2)+
  geom_path(alliso50, mapping = aes(long,lat,group = group), color = "black", alpha = 0.9, size = 0.2)+
  scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.83, 0.12),
        legend.title = element_blank(),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 8),
        legend.spacing.y = unit(-0.02, "cm"),
        legend.box.background = element_rect(color = "white",fill = "white"),
        legend.key = element_rect(fill = NA),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7))+
  coord_sf(xlim = c(166.44,166.60), ylim = c(-45.64,-45.78), crs = 4269)+
  geom_point(device, mapping = aes(x = lon, y = lat, color = type), alpha = 0.8)+
  scale_color_manual(values = type_color)
