library(marmap);library(dplyr);library(readxl);library(sf);library(ggrepel)

# deployments ----

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%
  filter(grepl("_01",Deployment_number))%>%
  filter(!grepl("FF01", Deployment_number))%>%
  #filter(!grepl("FF02", Deployment_number))%>%
  mutate(`Recorder type` = case_when(
    Fiord == "DAGG" ~ "ST + FPOD",
    TRUE ~ Recorder_type))%>%
  distinct(PROJECT, Fiord, Deployment_number, `Recorder type`, Latitude, Longitude)

shapefile_path<-"C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles"

# depth ----

fiordland_base <- marmap::getNOAA.bathy(lon1 = 165, lon2 = 169,
                        lat1 = -43.5, lat2 = -47.5, resolution = 0.5, keep = TRUE)

fiordland_base_raster<-marmap::as.raster(fiordland_base)

test_spdf <- as(fiordland_base_raster, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("Depth (m)", "x", "y")

nrow(test_df)
test_df<-test_df%>%
  filter(`Depth (m)` < 10)
nrow(test_df)

bathy<-sf::read_sf(shapefile_path, layer = "niwa-new-zealand-bathymetry-contours-2016")
alliso50<-subset(bathy, ELEVATION == -50)
alliso50<-as.data.frame(st_coordinates(alliso50))
alliso200<-subset(bathy, ELEVATION == -200)
alliso200<-as.data.frame(st_coordinates(alliso200))

# other shapesfiles ----

NZ_coast<-sf::read_sf(shapefile_path, layer = "nz-coastlines-and-islands-polygons-topo-1500k") #https://data.linz.govt.nz/layer/51560-nz-coastlines-and-islands-polygons-topo-1500k/

NZ_lakes<-sf::read_sf(shapefile_path, layer = "nz-lake-polygons-topo-150k") #https://data.linz.govt.nz/layer/50293-nz-lake-polygons-topo-150k/
big_lakes<-subset(NZ_lakes, !is.na(name_ascii))

protected_areas<-sf::read_sf(shapefile_path, layer = "protected-areas") #https://data.linz.govt.nz/layer/53564-protected-areas/
natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))

MA<-sf::read_sf(shapefile_path, layer = "Marine_Protected_Areas_under_the_Marine_Management_Act") # https://catalogue.data.govt.nz/dataset/marine-protected-areas-under-the-marine-management-act/resource/ad9dec70-5163-4d21-ab1c-216d6ff314ac
FMA<-subset(MA, Name == "Fiordland Marine Area")

# fiord labels ----
fiord_labels<-data.frame(label = c("Lake\nManapouri","Piopiotahi-Milford Sound","Te H\u101pua-Sutherland Sound",
                                   "H\u101wea-Bligh Sound","Te Houhou-George Sound","Taitetimu-Caswell Sound",
                                   "Taiporoporo-Charles Sound","Hinenui-Nancy Sound","Te Awa-o-T\u16b-\nThompson Sound",
                                   "Patea-Doubtful Sound","Te R\u101-Dagg Sound",
                                   "Te Puaitaha-\nBreaksea Sound","Tamatea-Dusky\nSound","Taiari-Chalky Inlet",
                                   "Rakituma-Preservation Inlet", "Motup\u14dhue-Bluff", "Rakiura-Stewart Island"),# "Vancouver\nArm"),
                         lat = c(-45.51, -44.55, -44.72,
                                 -44.77, -44.85, -45.01,
                                 -45.05, -45.1, -45.15,
                                 -45.27, -45.39,
                                 -45.59, -45.75, -46.02,
                                 -46.1, -46.597, -47),# -45.5),
                         lon = c(167.55, 167.8, 167.55,
                                 167.48, 167.35, 167.14,
                                 167.09, 167.03, 166.97,
                                 166.88, 166.78,
                                 166.67, 166.47, 166.51,
                                 166.6, 168.33, 167.75))#, 166.98))

fiord_labels_white<-data.frame(label = c("Taiporoporo-Charles Sound","Hinenui-Nancy Sound",
                                   "Te R\u101-Dagg Sound","Taiari-Chalky Inlet","Rakituma-Preservation Inlet"),
                         lat = c(-45.05,-45.1,
                                 -45.39,-46.02,-46.1),
                         lon = c(167.09, 167.03,
                                 166.78,166.51,166.6))

# map ----

bathy_FMA<-ggplot()+
  geom_tile(data=test_df, aes(x=x, y=y, fill=`Depth (m)`), alpha=0.8)+
  geom_sf(data = FMA, alpha = 0.05, color = "aquamarine", lwd = 0.4)+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "white", lwd = 0.1)+
  geom_path(alliso200, mapping = aes(X,Y,group = L2), color = "steelblue4", alpha = 0.7, linewidth = 0.1)+
  geom_path(alliso50, mapping = aes(X,Y,group = L2), color = "antiquewhite4", alpha = 0.7, linewidth = 0.1)+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab("Longitude")+
  ylab("Latitude")+
  #geom_sf(data = mpa, aes(fill = "Marine Reserve"), alpha = 1)+
  geom_sf(data = mpa, alpha = 1, fill = "orange", lwd = 0.1)+
  geom_sf(data = big_lakes, alpha = 0.6, fill = "steelblue2", lwd = 0.1)+
  geom_point(data = deploy, aes(x = Longitude, y = Latitude, shape = `Recorder type`), fill = "red", size = 1)+
  geom_text_repel(data=deploy%>%filter(grepl("FF",Deployment_number)), aes(x = Longitude, y = Latitude, label=c('2','1')), size = 2.3, min.segment.length = 0, nudge_y = c(0.05,0.05), nudge_x = c(-0.05,0.05))+
  scale_shape_manual(values = c(21:23))+
  coord_sf(xlim = c(165.8,168.35), ylim = c(-46.6,-44.25), crs = 4269)+
  #scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.90, 0.28),
        legend.title =  element_text(size = 6),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 5),
        #legend.spacing.y = unit(-0.02, "cm"),
        legend.box.background = element_rect(color = "white",fill = "white"),
        legend.key = element_rect(fill = NA),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))+
  geom_text_repel(data = fiord_labels, aes(x = lon, y = lat, label = label), size = 2.3, min.segment.length = 0, force_pull = 2, box.padding = 0.1,
                  nudge_x = c(0.29,-0.6,-0.6,
                              -0.6,-0.6,-0.6,
                              -0.7,-0.6,-0.8,
                              -0.8,-0.6,
                              -0.6,-0.6,-0.6,
                              -0.6,-0.6,-0.6),#0.23),
                  nudge_y = c(-0.05,0.1,0.15,
                              0.1,0.1,0.15,
                              0.1,0.07,0.01,
                              0,0,
                              0.08,-0.01,-0.1,
                              -0.15,0.02,0))#,-0.09))

bathy_FMA_white<-bathy_FMA+
  geom_text_repel(data = fiord_labels_white, aes(x = lon, y = lat, label = label), size = 2.3, color = "azure", min.segment.length = 0, force_pull = 2, box.padding = 0.1,
                  nudge_x = c(-0.7,-0.6,-0.6,
                              -0.6,-0.6),
                  nudge_y = c(0.1,0.07,0,
                              -0.1,-0.15))

###small NZ with box ----

dunedin<-data.frame(label = c("\u14ctepoti-Dunedin"),
                         lat = c(-45.880),
                         lon = c(170.501))

base<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "white", lwd = 0.1)+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))+
  xlab("Longitude")+
  ylab("Latitude")

NZ<-base+
  coord_sf(crs = 4269)+
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  xlab("")+
  ylab("")+
  geom_rect(mapping = aes(xmin = 165.8, xmax = 168.35, ymin = -46.6, ymax = -44.5), fill = NA, color = "gold2")+
  theme_void()+
  geom_point(data = dunedin, aes(x = lon, y = lat), size = 0.8, color = "gold2")


#### map a all together ----

map_bathy_FMA_white<-cowplot::ggdraw() +
  cowplot::draw_plot(bathy_FMA_white) +
  cowplot::draw_plot(NZ, x = 0.18, y = 0.65, width = 0.2, height = 0.3)
  
  ggsave("./figures/bathy_FMA_white.png", map_bathy_FMA_white, dpi = 700, height = 6, width = 4, units = 'in')
  ggsave("./figures/bathy_FMA_white.svg", map_bathy_FMA_white, dpi = 700, height = 6, width = 4, units = 'in')
  
