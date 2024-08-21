library(ggplot2);library(sf);library(rgdal);library(raster);library(ggpolypath);library(mapview);library(dplyr)

# read in many shape files in a folder
list_sigopp_files<-list.files("./data/DOC_opp_sig_db/MMSD_sightings/sightings_shapefiles", recursive = T)
list_effopp_files<-list.files("./data/DOC_opp_sig_db/MMSD_tracks", recursive = T)
#list_opp_files<-list.files("C:/Users/leahm/Documents/Otago/FBD data management/FBD SQL/Opp sightings DOC/From Chloe May 2023")

unique_list_sigopp<-unique(substr(list_sigopp_files, 1, nchar(list_sigopp_files)-4))
unique_list_effopp<-unique(substr(list_effopp_files, 1, nchar(list_effopp_files)-4))
unique_list_effopp<-unique(gsub("\\.*$","",unique_list_effopp))

unique_list_sigopp<-as.list((unique_list_sigopp[!grepl(".shp",unique_list_sigopp)]))
unique_list_effopp<-as.list((unique_list_effopp[!grepl(".shp",unique_list_effopp)]))

stringr::str_extract(unique_list_effopp, "^.*(?=/MMSD)")
sub(".*/", "",unique_list_effopp)

DOCsig<-lapply(unique_list_sigopp, function(x) read_sf(paste0("./data/DOC_opp_sig_db/MMSD_sightings/sightings_shapefiles/",stringr::str_extract(x, "^.*(?=/MMSD)")), layer = sub(".*/", "",x)))
DOCeff<-lapply(unique_list_effopp, function(x) read_sf(paste0("./data/DOC_opp_sig_db/MMSD_tracks/",stringr::str_extract(x, "^.*(?=/MMSD)")), layer = sub(".*/", "",x)))
warnings()
#mapview::mapview(DOCeff[[3]])

#sigs#
DOCsig[[1]]<-st_transform(DOCsig[[1]], CRS("+proj=longlat +datum=WGS84 +no_defs"))
DOCsig[[2]]<-st_transform(DOCsig[[2]], CRS("+proj=longlat +datum=WGS84 +no_defs"))
DOCsig[[4]]<-st_transform(DOCsig[[4]], CRS("+proj=longlat +datum=WGS84 +no_defs"))

DOCsig[[1]]%>%mutate(coords.x1 = sf::st_coordinates(.)[,2],
       coords.x2 = sf::st_coordinates(.)[,1])

DOCsig_df<-lapply(DOCsig, function(x) as.data.frame(x%>%mutate(coords.x1 = sf::st_coordinates(.)[,2],
                                                               coords.x2 = sf::st_coordinates(.)[,1])))

names(DOCsig_df[[3]])

for (i in 1:length(DOCsig_df)){
  print(head(DOCsig_df[[i]]))
  
  if (i == 1 | i == 2 | i == 4){
    DOCsig_df[[i]]<-DOCsig_df[[i]]%>%dplyr::select(SPECIES, SPECIES_CL, FIELD_DATE, coords.x2, coords.x1, VESSEL, NOTES)%>%
      dplyr::rename("CERTAINTY" = SPECIES_CL)%>%
      mutate(SPECIES = stringr::str_replace(SPECIES, "BN","Bottlenose dolphin"),
             YEAR = year(dmy(FIELD_DATE)))%>%
      dplyr::select(-FIELD_DATE)
    
  } else {
    DOCsig_df[[i]]<-DOCsig_df[[i]]%>%dplyr::select(SPECIES, CERTAINTY, YEAR, coords.x2, coords.x1, VESSEL, NOTES)
  }
  
  print(head(DOCsig_df[[i]]))
}

all_sig<-bind_rows(DOCsig_df, .id = "column_label")%>%
  filter(VESSEL == "Southern Winds" | VESSEL == "Milford Wanderer")
unique(all_sig$SPECIES)

all_eff<-DOCeff[[2]]%>%
  subset(VESSEL_NAM == "Southern Winds" | VESSEL_NAM == "Milford Wanderer")

#### -----
as.data.frame(all_sig)
all_sig_df<-all_sig%>%
  filter(!(coords.x1 > 167 & coords.x2 < -45.55))%>% #group of values on land
  filter(!(coords.x2 > -45.44 & coords.x2 < -45.4 & coords.x1 > 167.149))%>% #another group of values on land
  filter(!(coords.x2 > -45.5 & coords.x2 <= -45.47 & coords.x1 > 167.12))%>% #another on land
  filter(coords.x1 <  167.93)%>% #not on land in Milford
  filter(!is.na(SPECIES))%>%
  mutate(SPECIES = gsub("?","",SPECIES, fixed = TRUE))%>%#remove question marks
  mutate(SPECIES = stringr::str_to_sentence(SPECIES))%>%
  mutate(SPECIES = case_when(
    SPECIES == "Killer whale / orca" ~ "Orca",
    SPECIES == "Humpback" ~ "Humpback whale",
    SPECIES == "Humpback Whale" ~ "Humpback whale",
    SPECIES == "Dusky & common" |  SPECIES == "Dusky + common" | 
      SPECIES == "Common + dusky" | SPECIES == "Dusky/common" |
      SPECIES == "Dusky/commons" | SPECIES == "Common/dusky" |
      SPECIES == "Dusky and common" ~ "Dusky and common dolphin",
    SPECIES == "Dusky or common" ~ "Dusky or common dolphin",
    SPECIES == "Dusky" | SPECIES == "Dusky dophin" | SPECIES == "Humpback whale, dusky dolphin" |
      SPECIES == "Dusky dolphins" ~ "Dusky dolphin",
    SPECIES == "Common dolphins" ~ "Common dolphin",
    SPECIES == "Bottlenose" | SPECIES == "Tt" | SPECIES == "Turscops" | 
      SPECIES == "Tursiops" | SPECIES == "Bottlenose and nz fur seals" ~ "Bottlenose dolphin",
    SPECIES == "Dolphins" ~ "Dolphin",
    SPECIES == "Hectors" | SPECIES == "Hectors dolphin" ~ "Hector's dolphin",
    TRUE ~ SPECIES))%>%
  mutate(CERTAINTY = case_when(
    CERTAINTY == "Unsure/NA" | CERTAINTY == "NA" | CERTAINTY == "-" | CERTAINTY == "Unsure which" |
      CERTAINTY == "[]" | is.na(CERTAINTY) | CERTAINTY == "[\"Unsure\"]" ~ "Unsure",
    CERTAINTY == "[\"Positive\"]" ~ "Positive",
    CERTAINTY == "[\"Possible\"]" ~ "Possible",
    TRUE ~ CERTAINTY
  ))%>%
  mutate(YEAR = case_when(
    YEAR == 20174 ~ 2017,
    YEAR == 20140 ~ 2014,
    TRUE ~ YEAR
  ))%>%
  filter(YEAR != 1900 & YEAR != 0)

unique(all_sig_df$SPECIES)
unique(all_sig_df$CERTAINTY)

bodo<-all_sig_df%>%
  mutate(CERTAINTY = case_when(
    SPECIES == "Common/bottlenose" ~ "Possible",
    SPECIES == "Unknown - prob bottlenose" ~ "Unsure",
    TRUE ~ CERTAINTY))%>%
  mutate(SPECIES = case_when(
    SPECIES == "Common/bottlenose" | SPECIES == "Unknown - prob bottlenose" | 
    SPECIES == "Bottlenose dolphin/ dusky dolphin" | SPECIES == "Bottlenose dolphin and dusky dolphin" |
    SPECIES == "Bottlenose and dusky dolphin" | SPECIES == "Dusky & bottlenose dolphin" ~ "Bottlenose dolphin",
    TRUE ~ SPECIES
  ))%>%
  filter(SPECIES == "Bottlenose dolphin")%>%
  filter(CERTAINTY == "Positive" | CERTAINTY == "Possible")

#before filtering for just "Bottlenose dolphin"
unique(bodo$SPECIES)

delph<-all_sig_df%>%
  mutate(CERTAINTY = case_when(
    SPECIES == "Common/bottlenose" ~ "Possible",
    SPECIES == "Dusky or common dolphin" ~ "Positive",
    SPECIES == "Unknown - prob bottlenose" ~ "Unsure",
    SPECIES == "Dusky dolphin - possibly" ~ "Possible",
    TRUE ~ CERTAINTY))%>%
  mutate(SPECIES = case_when(
      SPECIES == "Common/bottlenose" ~ "Common dolphin",
      SPECIES == "Bottlenose dolphin/ dusky dolphin" | SPECIES == "Bottlenose dolphin and dusky dolphin" |
        SPECIES == "Bottlenose and dusky dolphin" | SPECIES == "Dusky & bottlenose dolphin" | SPECIES == "Dusky dolphin - possibly" ~ "Dusky dolphin",
      TRUE ~ SPECIES))%>%
  filter(grepl("dolphin", tolower(SPECIES)))%>%
  filter(SPECIES != "Bottlenose dolphin" & SPECIES != "Hector's dolphin")%>%
  filter(CERTAINTY == "Positive" | CERTAINTY == "Possible")

unique(delph$SPECIES)

unique(all_sig_df$SPECIES)

other<-all_sig_df%>%
  mutate(CERTAINTY = case_when(
    SPECIES == "False killer whale?" ~ "Possible",
    TRUE ~ CERTAINTY))%>%
  mutate(SPECIES == case_when(
    SPECIES == "Pilot  whale" ~ "Pilot whale",
    #SPECIES == "False killer whale/?" ~ "False killer whale",
    TRUE ~ SPECIES
    ))%>%
  filter(SPECIES == "Orca" | SPECIES == "Pilot whale" | 
                 SPECIES == "False killer whale" | SPECIES == "Hector's dolphin") #add hector's

opp_sig_table<-bodo%>%
  bind_rows(delph)%>%
  bind_rows(other)%>%
  group_by(YEAR, SPECIES)%>%
  tally()%>%
  arrange(SPECIES)%>%
  tidyr::pivot_wider(names_from = "YEAR", values_from = "n")

saveRDS(opp_sig_table, file = paste0("./tables/opp_sig_table.rds"))

shapefile_path<-"C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles"
NZ_coast<-sf::read_sf(shapefile_path, layer = "nz-coastlines-and-islands-polygons-topo-1500k")

##these data need to be taken with a grain of salt. people often think they see Dusky dolphins, but when you look at the photos, they are commons

##############
## Maps ------

bodo_opp<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_point(bodo, mapping = aes(x = coords.x1, y = coords.x2, color = SPECIES), alpha = 0.8, size = 2)+
  coord_sf(xlim = c(166.3,168), ylim = c(-44.5,-46.25), crs = 4269)+
  theme(legend.position = "right",
        legend.title = element_blank())

########

delph_opp<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_point(delph, mapping = aes(x = coords.x1, y = coords.x2, color = SPECIES), alpha = 0.9, size = 2)+
  coord_sf(xlim = c(166.3,168), ylim = c(-44.5,-46.25), crs = 4269)+
  scale_color_viridis_d()+
  theme(legend.position = "right",
        legend.title = element_blank())

########

other_opp<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_point(other, mapping = aes(x = coords.x1, y = coords.x2, color = SPECIES), alpha = 0.9, size = 2)+
  coord_sf(xlim = c(166.3,168), ylim = c(-44.5,-46.25), crs = 4269)+
  scale_color_manual(values = c("#88CCEE", "#332288", "#999933","pink"))+
  theme(legend.position = "right",
        legend.title = element_blank())

opp_maps<-ggpubr::ggarrange(bodo_opp, delph_opp, other_opp, labels = "auto", align = "v")

ggsave("./figures/Supplement/opp_maps.png", opp_maps, dpi = 700, width = 200, height = 800, units = 'mm')

####
# effort ----

mapview::mapview()

all_eff1<-fortify(all_eff)

###
all_eff_sfc<-sf::st_as_sfc(all_eff)
###

#creating grid

#all_eff@data$ID<-1:99

all_eff1_sf<-all_eff%>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326, wkt = "group")

rownames(all_eff1_sf)

all_eff1_line<-all_eff1_sf %>% 
  dplyr::summarize() %>%
  sf::st_cast("LINESTRING") 

grid <- sf::st_make_grid(sf::st_bbox(all_eff1_sf), cellsize = 0.016667/2, square = T) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(cell = 1:nrow(.))

#mapview(grid)
#mapview(all_eff1_sf$geometry[100])

grid_accum<-NULL

for(i in 1:99){
  
intersection<-sf::st_intersection(grid, all_eff[i,])

grid_lines <- st_cast(intersection, "MULTILINESTRING") %>% 
  st_cast("LINESTRING")

grid_count<-grid_lines %>% 
  st_drop_geometry() %>% # no longer relevant...
  group_by(cell) %>% 
  tally() %>% 
  mutate(n1 = 1)

print(head(grid_count))

grid_accum<-grid_accum%>%
  bind_rows(grid_count)

print(nrow(grid_accum))

}

grid_total<-grid_accum%>%
  group_by(cell)%>%
  mutate(total_n1 = sum(n1),
         total = sum(n))%>%
  distinct(cell, total_n1)%>%
  ungroup()

grid_count_raster<-grid%>%
  left_join(grid_total, by = "cell")%>%
  #filter(!(cell %in% c(33558,33559,33560,33753,33754)))
  filter(!(cell %in% c(8437,8438)))

#saveRDS(grid_count_raster,'./figures/Supplement/grid_0.25.RDS')
#saveRDS(grid_count_raster,'./figures/Supplement/grid_0.5.RDS')

grid_count_raster<-readRDS('./figures/Supplement/grid_0.25.RDS')%>%
  mutate(total_n1_40 = case_when(
    total_n1 >= 40 ~ 40,
    TRUE ~ total_n1
    
  ))

effort_opp_map<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(grid_count_raster%>%filter(!is.na(total_n1_40)), mapping = aes(fill = total_n1_40, color = after_scale(fill)))+
  #geom_sf(all_eff1_line$geometry[7:20], mapping = aes())
  #geom_point(all_eff, mapping = aes(x=long,y=lat, group=group))+
  #geom_bin_2d(binwidth = c(0.016667*0.5, 0.016667*0.5))+
  #geom_bin_2d(data = fortify(all_eff), mapping = aes(x=long,y=lat), binwidth = c(0.016667*1, 0.016667*1))+#1x1 min grids
  coord_sf(xlim = c(166.3,167.5), ylim = c(-45,-46.25), crs = 4269)+
  scale_fill_continuous(type = "viridis")+
  theme(legend.position = "right",
        legend.title = element_blank())

ggsave("./figures/Supplement/effort_opp_maps_0.25min.png", effort_opp_map, dpi = 700, width = 200, height = 200, units = 'mm')
