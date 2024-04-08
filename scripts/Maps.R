library(ggplot2);library(sf);library(rgdal);library(raster);library(ggpolypath);library(ggrepel)

shapefile_path<-"C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles"

NZ_coast<-sf::read_sf(shapefile_path, layer = "nz-coastlines-and-islands-polygons-topo-1500k")
NZ_lakes<-sf::read_sf(shapefile_path, layer = "nz-lake-polygons-topo-150k")
big_lakes<-subset(NZ_lakes, !is.na(name_ascii))
protected_areas<-sf::read_sf(shapefile_path, layer = "protected-areas")
MA<-sf::read_sf(shapefile_path, layer = "Marine_Protected_Areas_under_the_Marine_Management_Act")
FMA<-subset(MA, Name == "Fiordland Marine Area")

natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))

bathy<-sf::read_sf(shapefile_path, layer = "niwa-new-zealand-bathymetry-contours-2016")
alliso50<-subset(bathy, ELEVATION == -50)
alliso50<-as.data.frame(st_coordinates(alliso50))
alliso200<-subset(bathy, ELEVATION == -200)
alliso200<-as.data.frame(st_coordinates(alliso200))
alliso1000<-subset(bathy, ELEVATION == -1000)
alliso1000<-as.data.frame(st_coordinates(alliso1000))

fiord_labels<-data.frame(label = c("Lake\nManapouri","Piopiotahi-Milford Sound","Te H\u101pua-Sutherland Sound",
                                   "H\u101wea-Bligh Sound","Te Houhou-George Sound","Taitetimu-Caswell Sound",
                                   "Taiporoporo-Charles Sound","Hinenui-Nancy Sound","Te Awa-o-T\u16b-Thompson Sound",
                                   "Patea-Doubtful Sound","Te R\u101-Dagg Sound",
                                   "Te Puaitaha-Breaksea\nSound","Tamatea-Dusky\nSound","Taiari-Chalky Inlet",
                                   "Rakituma-Preservation Inlet"),# "Vancouver\nArm"),
                         lat = c(-45.51, -44.58, -44.72,
                                 -44.77, -44.85, -45.02,
                                 -45.05, -45.1, -45.15,
                                 -45.25, -45.38,
                                 -45.59, -45.75, -46.02,
                                 -46.1),# -45.5),
                         lon = c(167.55, 167.8, 167.55,
                                 167.5, 167.36, 167.15,
                                 167.08, 167.02, 166.97,
                                 166.9, 166.8,
                                 166.67, 166.47, 166.51,
                                 166.6))#, 166.98))

base<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "white")+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(data = mpa, aes(fill = "Marine Reserve"), alpha = 1)+
  geom_path(alliso200, mapping = aes(X,Y,group = L2), color = "steelblue", alpha = 0.7, linewidth = 0.2)+
  geom_path(alliso1000, mapping = aes(X,Y,group = L2), color = "steelblue2", alpha = 0.7, linewidth = 0.2)+
  geom_sf(data = big_lakes, alpha = 0.6, fill = "blue")+
  coord_sf(xlim = c(166.0,168), ylim = c(-46.2,-44.5), crs = 4269)+
  scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.83, 0.12),
        legend.title = element_blank(),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 5),
        legend.spacing.y = unit(-0.02, "cm"),
        legend.box.background = element_rect(color = "white",fill = "white"),
        legend.key = element_rect(fill = NA),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6))+
  geom_text_repel(data = fiord_labels, aes(x = lon, y = lat, label = label), size = 1.75, min.segment.length = 0, force_pull = 2, box.padding = 0.1,
                  nudge_x = c(0.29,-0.3,-0.4,
                              -0.3,-0.5,-0.4,
                              -0.5,-0.5,-1.5,
                              -0.3,-0.2,
                              -0.4,-0.3,-0.3,
                              -0.30),#0.23),
                  nudge_y = c(-0.05,0.01,0.04,
                              0.1,0.05,0.1,
                              0.08,0.05,0.01,
                              0,0,
                              0.08,-0.02,-0.04,
                              -0.15))#,-0.09))

fiord_fill = c("Marine Reserve" = "orange")

device<-data.frame(lat = c(-46.035,-46.08,-45.395,-45.395,-45.105,-45.1), lon = c(166.54,166.67,166.815,166.825,167.03,167.14), type = c("SoundTrap","SoundTrap","SoundTrap","FPOD","FPOD","FPOD"))
 
type_color = c("SoundTrap" = "red","FPOD" = "purple")

chalk_pres<-base+
  geom_sf(data = mpa, aes(fill = "Marine Reserve"), alpha = 1)+
  geom_path(alliso200, mapping = aes(X,Y,group = L2), color = "steelblue", alpha = 0.7, size = 0.2)+
  geom_path(alliso50, mapping = aes(X,Y,group = L2), color = "black", alpha = 0.7, size = 0.2)+
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
dusky<-base+
  geom_polygon(mpa, mapping = aes(long,lat,group = group, fill = "Marine Reserve"), alpha = 1)+
  geom_path(alliso200, mapping = aes(long,lat,group = group), color = "steelblue", alpha = 0.7, size = 0.2)+
  geom_path(alliso50, mapping = aes(long,lat,group = group), color = "black", alpha = 0.9, size = 0.2)+
  scale_fill_manual(values = fiord_fill)+
  theme(legend.position = "none",
        #legend.title = element_blank(),
        #legend.margin = margin(c(1, 1, 1, 1)),
        #legend.key.size = unit(0.2, 'cm'),
        #legend.text = element_text(size = 8),
        #legend.spacing.y = unit(-0.02, "cm"),
        #legend.box.background = element_rect(color = "white",fill = "white"),
        #legend.key = element_rect(fill = NA),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))+
  coord_sf(xlim = c(166.44,166.60), ylim = c(-45.64,-45.78), crs = 4269)+
  geom_point(device, mapping = aes(x = lon, y = lat, color = type), alpha = 0.8)+
  scale_color_manual(values = type_color)

ggsave("./figures/dusky.svg", dusky, dpi = 320, width = 250, units = 'mm')
ggsave("./figures/dusky.png", dusky, dpi = 320, height = 150, units = 'mm')

