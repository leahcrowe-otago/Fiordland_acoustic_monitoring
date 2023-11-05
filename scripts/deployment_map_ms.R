library(marmap);library(dplyr);library(readxl)

# deployments ----
deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%
  filter(grepl("_01",Deployment_number))%>%
  filter(!grepl("FF01", Deployment_number))%>%
  filter(!grepl("FF02", Deployment_number))%>%
  mutate(`Recorder type` = case_when(
    Fiord == "DAGG" ~ "ST + FPOD",
    TRUE ~ Recorder_type))%>%
  distinct(PROJECT, Fiord, Deployment_number, `Recorder type`, Latitude, Longitude)

shapefile_path<-"C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles"

# depth ----

fiordland_base <- marmap::getNOAA.bathy(lon1 = 165, lon2 = 169,
                        lat1 = -43.5, lat2 = -47.5, resolution = 0.25)
#plot(fiordland_base, image = TRUE)
#marmap::scaleBathy(fiordland_base, deg = 2, x = "bottomleft", inset = 5)

fiordland_base_raster<-marmap::as.raster(fiordland_base)
#mapview::mapview(fiordland_base_raster)

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

NZ_coast<-sf::read_sf("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k") #https://data.linz.govt.nz/layer/51560-nz-coastlines-and-islands-polygons-topo-1500k/
#NZ_coast<-as.data.frame(st_coordinates(NZ_coast))

NZ_lakes<-sf::read_sf("./shapefiles", layer = "nz-lake-polygons-topo-150k") #https://data.linz.govt.nz/layer/50293-nz-lake-polygons-topo-150k/
big_lakes<-subset(NZ_lakes, !is.na(name_ascii))

protected_areas<-sf::read_sf("./shapefiles", layer = "protected-areas") #https://data.linz.govt.nz/layer/53564-protected-areas/
natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))

MA<-sf::read_sf("./shapefiles", layer = "Marine_Protected_Areas_under_the_Marine_Management_Act") # https://catalogue.data.govt.nz/dataset/marine-protected-areas-under-the-marine-management-act/resource/ad9dec70-5163-4d21-ab1c-216d6ff314ac
FMA<-subset(MA, Name == "Fiordland Marine Area")

# fiord labels ----
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

# map ----

#fiord_fill = c("Marine Reserve" = "orange")

bathy_FMA<-ggplot()+
  geom_tile(data=test_df, aes(x=x, y=y, fill=`Depth (m)`), alpha=0.8)+
  geom_sf(data = FMA, alpha = 0.1, color = "aquamarine")+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "white")+
  geom_path(alliso200, mapping = aes(X,Y,group = L2), color = "steelblue4", alpha = 0.7, linewidth = 0.2)+
  #geom_path(alliso50, mapping = aes(X,Y,group = L2), color = "black", alpha = 0.7, linewidth = 0.2)+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab("Longitude")+
  ylab("Latitude")+
  #geom_sf(data = mpa, aes(fill = "Marine Reserve"), alpha = 1)+
  geom_sf(data = mpa, alpha = 1, fill = "orange")+
  geom_sf(data = big_lakes, alpha = 0.6, fill = "steelblue2")+
  geom_point(data = deploy, aes(x = Longitude, y = Latitude, shape = `Recorder type`), color = "red")+
  coord_sf(xlim = c(166.0,168), ylim = c(-46.5,-44.25), crs = 4269)+
  #scale_fill_manual(values = fiord_fill)+
  theme(legend.position = c(0.90, 0.25),
        legend.title =  element_text(size = 6),
        legend.margin = margin(c(1, 1, 1, 1)),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 5),
        #legend.spacing.y = unit(-0.02, "cm"),
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


  ggsave("./figures/bathy_FMA.svg", bathy_FMA, dpi = 320, height = 6, width = 4, units = 'in')
  ggsave("./figures/bathy_FMA.png", bathy_FMA, dpi = 320, height = 6, width = 4, units = 'in')
  