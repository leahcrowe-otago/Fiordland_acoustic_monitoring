## dist around a barrier ----
#https://www.seascapemodels.org/rstats/2020/02/08/calculating-distances-in-R.html
## relevant to both

library(mapview)
library(sf)
library(dplyr)
library(fasterize)
library(raster)
library(dplyr)
library(terra)
library(tidyterra)
library(ggplot2)
library(viridis)

crs.utm.fiordland<-"+proj=utm +zone=58 +datum=WGS84 +units=m +no_defs"
crs.latlon<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

shapefile_path<-"C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles"
NZ_coast<-sf::read_sf(shapefile_path, layer = "nz-coastlines-and-islands-polygons-topo-1500k")
mapview(NZ_coast)

#box in the Tasman to hack the raster
polycoords<-data.frame(x = c(-45.5,-45.5,-45.6,-45.6),
                       y = c(165.8,165.9,165.9,165.8))

poly<-polycoords %>% 
  st_as_sf(coords = c("y", "x"), 
           crs = crs.latlon) %>% 
  st_bbox() %>% 
  st_as_sfc()

poly_utm<-st_transform(poly, crs = crs.utm.fiordland)

mapview(poly_utm)

## Dagg ----
### sightings ----
doubtfuldagg<-read.csv('./data/DoubtfulDagg.csv', header = T)

doubtfuldagg<-doubtfuldagg%>%
  distinct(group, time, date, Latitude, Longitude)%>%
  filter(time == '9:35:36' | time == '8:59:37')%>%
  mutate(label = 1:2)
#declare as lat long
doubtfuldagg_pos<-st_as_sf(x = doubtfuldagg, coords = c("Longitude", "Latitude"),
                           crs = crs.latlon)

mapview::mapview(doubtfuldagg_pos)

### raster ----
NZ_coast_crop<-st_crop(NZ_coast, c(xmin = 166.3, ymin = -46.3, xmax = 167.4, ymax = -44.9))
mapview(NZ_coast_crop)
NZ_coast_crop_utm<-st_transform(NZ_coast_crop, crs = crs.utm.fiordland)

NZ_coast_union<-NZ_coast_crop_utm %>%
   full_join(poly_utm, copy = T)

mapview(NZ_coast_union)

r <- raster::raster(extent(NZ_coast_union), nrows = 600, ncols = 800)
rNZ_coast <- fasterize(summarize(NZ_coast_union), r)
mapview(rNZ_coast)

crs(rNZ_coast)<-crs.utm.fiordland
mapview(rNZ_coast)

### distance ----
dagg_utm<-st_transform(doubtfuldagg_pos, crs = crs.utm.fiordland)

dagg_xy<-st_coordinates(dagg_utm)

dagg_icell<-cellFromXY(rNZ_coast, dagg_xy)
dagg_icell
rNZ_coast[dagg_icell[1]] <- 2
rNZ_coast[dagg_icell[2]]

d<-gridDistance(rNZ_coast, origin = 2,
             omit = 1)/1000

mapview(d)
#dist between points
d[dagg_icell]

d_rast<-rast(d)

### map ----

Fig4a<-ggplot2::ggplot() + tidyterra::geom_spatraster(data = d_rast)+
  scale_fill_viridis(option="magma", name = "km from #1")+
  coord_sf(xlim = c(166.3,167.4), ylim = c(-46.2,-44.99), crs = 4269)+
  geom_point(doubtfuldagg, mapping = aes(x = Longitude, y = Latitude), color = "white")+
  geom_text(doubtfuldagg, mapping = aes(x = Longitude, y = Latitude, label = label), color = "white", hjust=-0.5, vjust=-0.5)+
  theme(legend.position = c(0.82,0.2))

ggplot2::ggsave(paste0("./figures/Fig4a.png"), Fig4a, device = "png", dpi = 700, width = 100, height = 150, units = 'mm')

## Bluff ----
### sightings ----
sunshine<-read.csv('./data/Sunshine.csv', header = T)
ray<-read.csv('./data/Ray.csv', header = T)

sunray<-sunshine%>%
  bind_rows(ray)%>%
  filter(!is.na(Longitude))%>%
  arrange(date)%>%
  filter((date == "2022-07-12" & time == "16:52:25")| date == "2022-09-15")%>%
  filter(id_name == "RAY")%>%
  mutate(label = 1:2)

sunray_pos<-st_as_sf(x = sunray, coords = c("Longitude", "Latitude"),
                     crs = crs.latlon)

mapview::mapview(sunray_pos)

### raster ----

NZ_coast_crop<-st_crop(NZ_coast, c(xmin = 166.3, ymin = -47, xmax = 169.1, ymax = -44.9))
mapview(NZ_coast_crop)
NZ_coast_crop_utm<-st_transform(NZ_coast_crop, crs = crs.utm.fiordland)

#another box to hack raster to the east
polycoords2<-data.frame(x = c(-46.5,-47.0,-47.0,-46.5),
                       y = c(169.0,169.1,169.1,169.0))

poly2<-polycoords2 %>% 
  st_as_sf(coords = c("y", "x"), 
           crs = crs.latlon) %>% 
  st_bbox() %>% 
  st_as_sfc()

poly2_utm<-st_transform(poly2, crs = crs.utm.fiordland)

NZ_coast_union<-NZ_coast_crop_utm %>%
  full_join(poly_utm, copy = T)%>%
  full_join(poly2_utm, copy = T)

mapview(NZ_coast_union)

r <- raster::raster(extent(NZ_coast_union), nrows = 600, ncols = 800)
rNZ_coast <- fasterize(summarize(NZ_coast_union), r)
mapview(rNZ_coast)

crs(rNZ_coast)<-crs.utm.fiordland
mapview(rNZ_coast)

### distance ----

sunray_xy_utm<-st_transform(sunray_pos, crs = crs.utm.fiordland)
mapview(sunray_xy_utm)

sunray_xy<-st_coordinates(sunray_xy_utm)

sunray_icell<-cellFromXY(rNZ_coast, sunray_xy)
sunray_icell
rNZ_coast[sunray_icell[1]] <- 2
rNZ_coast[sunray_icell[2]]

b<-gridDistance(rNZ_coast, origin = 2,
                omit = 1)/1000

mapview(b)

#dist between points
b[sunray_icell]

b_rast<-rast(b)

### map ----

Fig4b<-ggplot2::ggplot() + tidyterra::geom_spatraster(data = b_rast)+
  scale_fill_viridis(option="magma", name = "km from #1")+
  coord_sf(xlim = c(166.3,168.8), ylim = c(-46.7,-45.1), crs = 4269)+
  geom_point(sunray, mapping = aes(x = Longitude, y = Latitude), color = "white")+
  geom_text(sunray, mapping = aes(x = Longitude, y = Latitude, label = label), color = "white", hjust=-0.5, vjust=1.5)+
  theme(legend.position = c(0.87,0.82))
  
ggplot2::ggsave(paste0("./figures/Fig4b.png"), Fig4b, device = "png", dpi = 700, width = 150, height = 150, units = 'mm')

Fig4ab<-ggpubr::ggarrange(Fig4a,Fig4b, labels = "auto", ncol = 2, widths = c(1,1.5))
ggplot2::ggsave(paste0("./figures/Fig4ab.png"), Fig4ab, device = "png", dpi = 700, width = 300, height = 150, units = 'mm')

###
