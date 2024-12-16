library(readxl);library(odbc);library(dplyr);library(DBI);library(lubridate);library(ggplot2);library(sf)

shapefile_path<-"C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles"
NZ_coast<-sf::read_sf(shapefile_path, layer = "nz-coastlines-and-islands-polygons-topo-1500k")

data_log<-read_excel('~/git-otago/Fiordland_acoustic_monitoring/data/Data_Log_06Aug2024.xlsx')

nf_dates<-data_log%>%
  filter(Fiord == "CHARLES" | Fiord == "NANCY" | Fiord == "DAGG" | Fiord == "CHALKY" | Fiord == "PRESERVATION")%>%
  filter(Year >= 2022)%>%
  tidyr::pivot_longer(cols = c(Start_date, End_date), names_to = "date_type", values_to = "DATE")%>%
  #this only works for this query because we were never in a neighbouring fiord on more than two separate days
  distinct(DATE)

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value

survey_data<-dbReadTable(con, "survey_data_calfyear")

survey_data[survey_data == '']<-NA
survey_data$DATE<-ymd(survey_data$DATE)

survey_2022<-survey_data%>%
  filter(year(DATE) >= 2022 & year(DATE) <= 2023)%>%
  filter(LONGITUDE < 168.5)%>%
  mutate(date_event = paste0(DATE,"_",EVENT))

species = c("Bottlenose","Common","Dusky","Hector's")

nancy_2023<-data.frame(DATEIME = "2023-11-21 00:00:00", DATE = ymd("2023-11-21"), TIME = "00:00:00", LATITUDE = -45.17361,
                         LONGITUDE = 167.10012, EVENT_TYPE = "Encounter START", ENCOUNTER_TYPE = "Initial", SIGHTING_NUMBER = "1",
                         SPECIES = "Bottlenose", CALFYEAR = 2024, YEAR = 2023, SEASON = "SPRING", Year_season = '2023_SPRING')

sig_2022<-survey_2022%>%
  filter(ENCOUNTER_TYPE != "")%>%
  tidyr::fill(SIGHTING_NUMBER, SPECIES)%>%
  filter(SPECIES %in% species)%>%
  mutate(Year_season = paste0(YEAR,"_",SEASON))%>%
  bind_rows(nancy_2023)
  
sig_2022$Year_season<-factor(sig_2022$Year_season, levels = c("2022_SUMMER","2022_AUTUMN","2022_WINTER","2022_SPRING","2023_SUMMER","2023_AUTUMN","2023_WINTER","2023_SPRING"))

nf_tracks<-nf_dates%>%
  left_join(survey_data, by = "DATE")%>%
  filter(!is.na(SEASON))%>%
  mutate(Year_season = paste0(YEAR,"_",SEASON))

survey_ded<-survey_2022%>%
  bind_rows(data.frame(LONGITUDE = 166,LATITUDE = -46.5, DATE = ymd("2024-08-09"), EVENT = "99"))%>% #hack the bounding box by adding these bounds
  bind_rows(data.frame(LONGITUDE = 168,LATITUDE = -44.5, DATE = ymd("2024-08-10"), EVENT = "999")) # allows to actually get wanted grid

# dedicated effort map during acoustic monitoring project ----
library(sp)

coordinates(survey_ded)<-~LONGITUDE+LATITUDE
raster::crs(survey_ded)<-"+proj=longlat +datum=WGS84 +no_defs"
sf_split<-split(survey_ded, survey_ded$date_event)
sf_points<-lapply(sf_split, function(x) sf::st_as_sf(x, crs = 4326))
x_linestring<-lapply(sf_points, function(x) sf::st_combine(x) %>% st_cast("LINESTRING"))
length(x_linestring)

grid_l <- sf::st_make_grid(sf::st_bbox(survey_ded), cellsize = 1/240, square = T) %>% #0.5x0.5 min grid
  sf::st_as_sf() %>%
  dplyr::mutate(cell = 1:nrow(.))

grid_accum_ded<-NULL

for(i in 1:length(x_linestring)){

  intersection<-sf::st_intersection(grid_l, x_linestring[[i]])

  grid_count_ded<-intersection %>% 
    st_drop_geometry() %>% # no longer relevant...
    group_by(cell) %>% 
    tally() %>% 
    mutate(n1 = 1)
  
  print(head(grid_count_ded))
  
  grid_accum_ded<-grid_accum_ded%>%
    bind_rows(grid_count_ded)
  
  print(nrow(grid_accum_ded))
  
}

  grid_total_ded<-grid_accum_ded%>%
  group_by(cell)%>%
  mutate(total_n1 = sum(n1),
         total = sum(n))%>%
  distinct(cell, total_n1)%>%
  ungroup()

grid_count_raster_ded<-grid_l%>%
  left_join(grid_total_ded, by = "cell")%>%
  filter(!is.na(total_n1))

saveRDS(grid_count_raster_ded,'./figures/Supplement/ded_grid_0.5.RDS')

grid_count_raster_ded<-readRDS('./figures/Supplement/ded_grid_0.5.RDS')%>%  
  mutate(total_n1_40 = case_when(
    total_n1 >= 40 ~ 40,
    TRUE ~ total_n1))

effort_ded_map<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  geom_sf(grid_count_raster_ded, mapping = aes(fill = total_n1_40, color = after_scale(fill)))+
  coord_sf(xlim = c(166.3,167.5), ylim = c(-45,-46.25), crs = 4269)+
  scale_fill_continuous(type = "viridis")+
  theme(legend.position = "right",
        legend.title = element_blank())

effort_ded_map

ggsave("./figures/Supplement/effort_ded_map_0.5min.png", effort_ded_map, dpi = 700, width = 200, height = 200, units = 'mm')

# maps of sightings on dedicated surveys during the acoustic monitoring project ----

date_color<-viridis::viridis(length(unique(nf_tracks$Year_season)), alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
names(date_color) <- unique(nf_tracks$Year_season)
date_color_scale <- scale_colour_manual(name = "Date", values = date_color)
  
nf_effort_base<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  geom_path(nf_tracks, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT), color = Year_season), linewidth = 0.3)+
  theme_bw()+
  #labs(color = "Year_Season")+
  geom_path(data = sig_2022, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT,SIGHTING_NUMBER), color = Year_season), size = 1)+
  geom_point(data = sig_2022%>%filter(SPECIES %in% species & EVENT_TYPE == "Encounter START"), mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT,SIGHTING_NUMBER), shape = SPECIES), size = 3.3)+
  geom_point(data = sig_2022%>%filter(SPECIES %in% species & EVENT_TYPE == "Encounter START"), mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT,SIGHTING_NUMBER), fill = Year_season, shape = SPECIES, color = Year_season), size = 3)+
  scale_shape_manual(values = c(21:24))+
  coord_sf(xlim = c(166.3,167.5), ylim = c(-45,-46.25), crs = 4269)

#Charles & Nancy
north<-nf_effort_base+
  coord_sf(crs = 4269, xlim = c(166.99,167.22), ylim = c(-45.2,-45.03))

#Dagg
middle<-nf_effort_base+
  coord_sf(crs = 4269, xlim = c(166.75,166.91), ylim = c(-45.44,-45.37))

#Chalky & Preservation
south<-nf_effort_base+
  coord_sf(crs = 4269, xlim = c(166.4,166.91), ylim = c(-45.88,-46.2))

nms<-ggpubr::ggarrange(north, middle, south, common.legend = TRUE, labels = "auto", ncol = 1, legend = "right")

ggplot2::ggsave(paste0("./figures/Supplement/nf_effort.png"), nms, device = "png", dpi = 700, width = 200, height = 300, units = 'mm')

nms_eff<-ggpubr::ggarrange(nms, effort_ded_map, ncol = 2, labels = c("","d"))

ggplot2::ggsave(paste0("./figures/Supplement/nf_effort_ded.png"), nms_eff, device = "png", dpi = 700, width = 300, height = 200, units = 'mm')

# Taumoana acoustic detections during visual survey ----

survey<-survey_2022%>%
  filter(DATE < ymd("2022-11-30") & DATE > ymd("2022-02-20")) # date after all Dusky are deployed

#this also needed for day/night figures below
surv_dates<-survey%>%
  mutate(year_mo = format(parse_date_time(as.character(DATE), "ymd"), "%Y_%m"))%>%
  filter(year_mo != "2022_05" & year_mo != "2022_10")%>% # did not survey in Dusky, but did in Dagg, Doubtful, and southern fiords
  filter(LATITUDE < -45.20 & LATITUDE > -45.82)%>%
  distinct(DATE)

survey_plot<-surv_dates%>%
  mutate(year_mo = format(parse_date_time(as.character(DATE), "ymd"), "%Y_%m"))%>%
  left_join(survey_2022, by = 'DATE')

species = c("Bottlenose","Common","Dusky")

sig_plot<-survey_plot%>%
  filter(ENCOUNTER_TYPE != "")%>%
  tidyr::fill(SIGHTING_NUMBER, SPECIES)%>%
  filter(SPECIES %in% species)

sig_plot%>%
  filter(LONGITUDE < 166.6)%>%
  distinct(DATE)

acoustic_dusky<-all_Cet%>%
  filter(Fiord == "DUSKY" | grepl("MAR",Fiord))%>%
  left_join(deploy%>%filter(grepl("_01", Deployment_number))%>%dplyr::select(-Date), by = c("Fiord_recorder", "Fiord"))%>%
  inner_join(surv_dates, by = c("Date" = "DATE"))%>%
  dplyr::rename("DATE" = "Date")%>%
  arrange(DATE)

head(acoustic_dusky)

# data with day/night data ----

ST_time<-ST_dol%>%
  filter(Quality != '?')%>%
  ungroup()%>%
  mutate(DATE = Date,
         TIME = hms(format(as.POSIXct(Datetime), format = '%H:%M:%S')),
         Fiord_recorder = paste0(Fiord,'_',type))%>%
  dplyr::select(DATE, TIME, Fiord_recorder, Datetime, Fiord)

FPOD_time<-all_FPOD_Dol%>%
  mutate(DATE = Date,
         TIME = hms(format(as.POSIXct(Datetime), format = '%H:%M:%S')))%>%
  dplyr::select(DATE, TIME, Fiord_recorder, Datetime, Fiord)
  
acou_time<-ST_time%>%
  bind_rows(FPOD_time)%>%
  mutate(season = case_when(
           month(DATE) == 12 | month(DATE) <=2 ~ "Summer",
           month(DATE) >= 3 & month(DATE) <= 5 ~ "Autumn",
           month(DATE) >= 6 & month(DATE) <= 8 ~ "Winter",
           month(DATE) >= 9 & month(DATE) <= 11 ~ "Spring"
           ))

summary(acou_time)

# without daylight savings -----

maxdate_plot<-ymd("2023-11-30")

sun_times_woDS <-
  suncalc::getSunlightTimes(
    date = seq(
      as.Date(min(acou_time$DATE)),
      maxdate_plot,
      by = "day"
    ),
    lat = -45.725,
    lon = 166.525,
    tz = "Etc/GMT+13",
    keep = c("dawn", "nauticalDawn", "dusk", "nauticalDusk", "sunrise", "sunset")
  )

sun_times_woDS<-sun_times_woDS%>%
  mutate(date = as.Date(dawn))

tidy_sun_times_woDS <-sun_times_woDS %>%
  select(-lat, -lon) %>%
  tidyr::pivot_longer(-date, names_to = "event", values_to = "time") %>%
  mutate(
    #tz = strftime(time, "%Z"),
    time = hms::as_hms(time)
  )

min_deploy<-deploy%>%ungroup%>%group_by(Fiord_recorder)%>%filter(Datetime_retrieval_local == min(Datetime_retrieval_local, na.rm = T))
max_deploy<-deploy%>%ungroup%>%group_by(Fiord_recorder)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local, na.rm = T))

# acoustic detections by time of day and relative to day/night ----

acou_sunlight<-ggplot(acou_time)+
  geom_ribbon(tidy_sun_times_woDS%>%filter(event == "dusk"), mapping = aes(x = date, ymin = time, ymax = hms::as_hms("24:00:00")), color = NA, fill = "midnightblue", alpha = 0.4)+
  geom_ribbon(tidy_sun_times_woDS%>%filter(event == "dawn"), mapping = aes(x = date, ymin = time, ymax = hms::as_hms("00:00:00")), color = NA, fill = "midnightblue", alpha = 0.4)+
  geom_vline(min_deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(max_deploy, mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")+
  #geom_point(aes(x = DATE, y = TIME, color = as.factor(season)))+
  geom_point(aes(x = DATE, y = TIME), size = 0.5)+
  scale_y_time()+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_F-POD","NANCY_ST","DAGG_F-POD","DAGG_ST","MARINE-RESERVE-1_ST","MARINE-RESERVE-2_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_F-POD")), ncol = 2)+
  #facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","MARINE-RESERVE-1","MARINE-RESERVE-2","DUSKY","CHALKY","PRESERVATION")), ncol = 2)+
  theme_bw()+
  ylab("Time (HH:MM:SS)")+
  scale_x_date("Date",date_breaks="3 months", date_labels="%b-%Y")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  coord_cartesian(xlim = c(ymd("2022-02-15"), maxdate_plot))

acou_sunlight<-acou_sunlight+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-09-02"), xmax = ymd("2023-11-09"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #MR_2 = FF03
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2022-12-31"), xmax = ymd("2023-06-26"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2023-11-29"), xmax = maxdate_plot, ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #MR_1 = FF02
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-1_ST"), aes(xmin = ymd("2022-12-29"), xmax = ymd("2023-02-23"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-1_ST"), aes(xmin = ymd("2023-06-22"), xmax = maxdate_plot, ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2022-11-23"), xmax = ymd("2023-02-23"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-02"), xmax = ymd("2023-06-27"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-11-25"), xmax = maxdate_plot, ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-10-19"), xmax = ymd("2023-11-09"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_F-POD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_F-POD"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHARLES_F-POD"), aes(xmin = ymd("2023-10-21"), xmax = maxdate_plot, ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)

acou_sunlight$layers<-c(
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-04-30"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="goldenrod", alpha = 0.4, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-06-21"), xmax = ymd("2023-10-19"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="goldenrod", alpha = 0.4, inherit.aes = FALSE),
  acou_sunlight$layers)

acou_sunlight

ggplot2::ggsave(paste0("./figures/acou_sunlight.png"), acou_sunlight, device = "png", dpi = 700, width = 150, height = 300, units = 'mm')

### acoustic detection (day or night) during days when there was visaul survey in the outer Tamatea ----

sun<-sun_times_woDS%>%
  dplyr::select(date, dawn, dusk)

sun$date<-ymd(sun$date)

head(acou_time)

acou_dusky<-acou_time%>%
  filter(Fiord == "DUSKY" | grepl("MAR",Fiord))%>%
  left_join(deploy%>%filter(grepl("_01", Deployment_number))%>%dplyr::select(-Date), by = c("Fiord_recorder","Fiord"))%>%
  inner_join(surv_dates, by = c("DATE"))%>%
  left_join(sun, by = c("DATE" = "date"))%>%
  mutate(tod = case_when( #time of day
    ymd_hms(Datetime) < ymd_hms(dawn) | ymd_hms(Datetime) > ymd_hms(dusk) ~ "Night",
    ymd_hms(Datetime) > ymd_hms(dawn) | ymd_hms(Datetime) < ymd_hms(dusk) ~ "Day",
    TRUE ~ "Night"
  ))

sig_acou2<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  geom_path(survey_plot, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT), color = year_mo))+
  geom_path(sig_plot%>%filter(SPECIES == "Bottlenose"), mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,SIGHTING_NUMBER), color = year_mo), linewidth = 3, alpha = 0.4)+
  coord_sf(xlim = c(166.45, 166.6), ylim = c(-45.8, -45.67))+
  geom_point(deploy%>%filter(grepl("_01", Deployment_number)), mapping = aes(x = Longitude, y = Latitude), color = "red", size = 2, shape = 22)+
  geom_point(acou_dusky%>%filter(tod == "Night"), mapping = aes(x = Longitude, y = Latitude), color = "midnightblue", size = 2, shape = 15)+
  geom_point(acou_dusky%>%filter(tod == "Day"), mapping = aes(x = Longitude, y = Latitude), color = "yellow", size = 1, shape = 15)+
  theme_bw()+
  facet_wrap(~DATE)+
  theme(legend.position = "inside", legend.position.inside =  c(.9, .05))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(-45.7,-45.8, by = -0.05))+
  scale_x_continuous(breaks = seq(166.5,166.6, by = 0.05))

sig_acou2

ggplot2::ggsave(paste0("./figures/Supplement/sig_acou2.png"), sig_acou2, device = "png", dpi = 700, width = 200, height = 200, units = 'mm')

#bin all detections in all fiords by hour of detection ---- 

diel<-acou_time%>%
  left_join(sun, by = c("DATE" = "date"))%>%
  mutate(tod = case_when( #time of day
    ymd_hms(Datetime) < ymd_hms(dawn) | ymd_hms(Datetime) > ymd_hms(dusk) ~ "Night",
    ymd_hms(Datetime) > ymd_hms(dawn) | ymd_hms(Datetime) < ymd_hms(dusk) ~ "Day",
    TRUE ~ "Night"
  ),
  Hour = hour(TIME))%>%
  distinct(DATE, Fiord_recorder, tod, Hour)

diel_plot<-ggplot(diel, aes(x = Hour, fill = tod))+
  geom_histogram(stat = "count", position = "dodge", binwidth = 1, color = "black", alpha = 0.6)+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_F-POD","NANCY_ST","DAGG_F-POD","DAGG_ST","MARINE-RESERVE-1_ST","MARINE-RESERVE-2_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_F-POD")), scales = "free", ncol = 2)+
  theme_bw()+
  scale_fill_manual(values = c("yellow", "midnightblue"))+
  theme(legend.title = element_blank(),
        legend.position = "inside", legend.position.inside =  c(.75, .1))+
  ylab("Number of unique days")

diel_plot

ggplot2::ggsave(paste0("./figures/diel_hist.png"), diel_plot, device = "png", dpi = 700, width = 150, height = 300, units = 'mm')

diel_ab<-ggpubr::ggarrange(acou_sunlight,diel_plot, labels = "auto")
ggplot2::ggsave(paste0("./figures/diel_ab.png"), diel_ab, device = "png", dpi = 700, width = 300, height = 300, units = 'mm')

# 

acou_dusky<-acou_time%>%filter(DATE <= ymd("2023-01-01") & (Fiord == "MARINE-RESERVE-1" | Fiord == "MARINE-RESERVE-2"))%>%
  group_by(Fiord_recorder, DATE)%>%
  tally()

summary(acou_dusky)


date_list_dusky<-data.frame(dates = seq(min(acou_dusky$DATE), max(acou_dusky$DATE), by="days"))

date_list_dusky_m1<-date_list_dusky%>%
  mutate(Fiord_recorder = "MARINE-RESERVE-1_ST")

date_list_dusky_m2<-date_list_dusky%>%
  mutate(Fiord_recorder = "MARINE-RESERVE-2_ST")

acou_dusky_dates<-date_list_dusky_m1%>%
  bind_rows(date_list_dusky_m2)%>%
  left_join(acou_dusky, by = c("dates" = "DATE", "Fiord_recorder"))%>%
  replace(is.na(.), 0)

mr1mr2<-ggplot(acou_dusky_dates, aes(x = dates, y = n, color = Fiord_recorder))+
  geom_line(alpha = 0.5)+
  theme_bw()+
  theme(legend.title = element_blank(), 
        legend.position = "bottom")+
  ylab("Number of positive detections")+
  xlab("Day")+
  scale_x_date(date_breaks="1 month", date_labels="%b-%Y")
  
mr1mr2
ggplot2::ggsave(paste0("./figures/Supplement/mr1mr2.png"), mr1mr2, device = "png", dpi = 700, width = 200, height = 100, units = 'mm')
