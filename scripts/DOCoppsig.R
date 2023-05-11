library(ggplot2);library(sf);library(rgdal);library(raster);library(ggpolypath);library(mapview);library(dplyr)

# read only one shape file
#DOCsig<-readOGR("C:/Users/leahm/Documents/Otago/FBD data management/FBD SQL/Opp sightings DOC", layer = "SeaSketch_data_Sightings_20161004_updated_by_Chloe")
#DOCsig<-list(DOCsig)
#MMSD_Sightings_1992_2003_20220809 no Hector's

# read in many shape files in a folder
list_opp_files<-list.files("C:/Users/leahm/Documents/Otago/FBD data management/FBD SQL/Opp sightings DOC/From Chloe May 2023")

unique_list<-unique(substr(list_opp_files, 1, nchar(list_opp_files)-4))

unique_list<-as.list((unique_list[!grepl(".shp",unique_list)]))

DOCsig<-lapply(unique_list, function(x) readOGR("C:/Users/leahm/Documents/Otago/FBD data management/FBD SQL/Opp sightings DOC/From Chloe May 2023", layer = x))

mapview(DOCsig)
NZ_coast<-readOGR("./shapefiles", layer = "nz-coastlines-and-islands-polygons-topo-1500k")

docsig_df<-lapply(DOCsig, function(x) fortify(as.data.frame(x)))

lapply(docsig_df, function(x) unique(x$SPECIES))

y<-0

lapply(docsig_df, function(x) {

y<<-y+1

hedo<-fortify(as.data.frame(x))%>%
          filter(grepl('HECTORS', toupper(SPECIES)) | grepl('HECTORS DOLPHIN', toupper(SPECIES)))%>%
          mutate(species = case_when(
            grepl('HECTORS', toupper(SPECIES)) ~ "HEDO",
            grepl('HECTORS DOLPHIN', toupper(SPECIES)) ~ "HEDO",
            TRUE ~ SPECIES))

print(hedo)

if (nrow(hedo) > 0){
write.csv(hedo, paste0("./data/docsigs",y,".csv"), row.names = F)
}
  })

alldocsig<-ggplot()+
  geom_polygon(NZ_coast, mapping = aes(long,lat,group = group), alpha = 0.9, fill = "white")+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_point(docsig_df%>%filter(coords.x2>-50), mapping = aes(x = coords.x1, y = coords.x2, color = species), alpha = 0.8)+
  coord_sf(xlim = c(166.3,168.3), ylim = c(-44,-46.25), crs = 4269)#+
  #facet_wrap(~YEAR)

ggsave("./figures/alldocsig.svg", alldocsig, dpi = 320, width = 250, units = 'mm')

##these data need to be taken with a grain of salt. people often think they see Dusky dolphins, but when you look at the photos, they are commons