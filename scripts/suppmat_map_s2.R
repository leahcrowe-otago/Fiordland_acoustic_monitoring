library(readxl);library(odbc);library(dplyr);library(DBI);library(lubridate);library(ggplot2)

data_log<-read_excel('./data/Data_Log_12Apr2024.xlsx')

nf_dates<-data_log%>%
  filter(Fiord == "CHARLES" | Fiord == "NANCY" | Fiord == "DAGG" | Fiord == "CHALKY" | Fiord == "PRESERVATION")%>%
  filter(Year >= 2022)%>%
  tidyr::pivot_longer(cols = c(Start_date, End_date), names_to = "date_type", values_to = "DATE")%>%
  #this only works for this query because we were never in a neighbouring fiord on more than two separate days
  distinct(DATE)


source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value

survey_data<-dbReadTable(con, "survey_data_calfyear")
survey_data$DATE<-ymd(survey_data$DATE)

survey_data[survey_data == '']<-NA

survey_2022<-survey_data%>%
  filter(year(DATE) >= 2022 & year(DATE) <= 2023)%>%
  filter(LONGITUDE < 168.5)

species = c("Bottlenose","Common","Dusky","Hector's")

sig_2022<-survey_2022%>%
  filter(ENCOUNTER_TYPE != "")%>%
  tidyr::fill(SIGHTING_NUMBER, SPECIES)%>%
  filter(SPECIES %in% species)%>%
  mutate(Year_season = paste0(YEAR,"_",SEASON))

sig_2022$Year_season<-factor(sig_2022$Year_season, levels = c("2022_SUMMER","2022_AUTUMN","2022_WINTER","2022_SPRING","2023_SUMMER","2023_AUTUMN","2023_WINTER","2023_SPRING"))

nf_tracks<-nf_dates%>%
  left_join(survey_data, by = "DATE")%>%
  filter(!is.na(SEASON))%>%
  mutate(Year_season = paste0(YEAR,"_",SEASON))

# hex_map<-ggplot(survey_2022, aes(x = LONGITUDE, y = LATITUDE, fill = factor(DATE)))+
#   geom_hex(fill = after_stat(count))
#   stat_summary_2d(aes(fill = after_stat(group)))
#   #stat_summary_hex(fun = function(z) count(unique(z)), binwidth = c(0.016667/2, 0.016667/2))+
#   coord_sf(crs = 4269)+
#   theme_bw()+
#   #theme(legend.position = "none")+
#   scale_fill_viridis_c()

#nice hex map but heat is from number of points
hex_map<-ggplot(survey_2022, aes(x = LONGITUDE, y = LATITUDE))+
  #0.5 x 0.5 min grids
  geom_hex(binwidth = c(0.016667/2, 0.016667/2))+
  coord_sf(crs = 4269)+
  theme_bw()+
  theme(legend.position = "none")+
  scale_fill_viridis_c()

ggsave("./figures/hex_map.svg", hex_map, dpi = 700, height = 6, width = 4, units = 'in')

date_color<-viridis::viridis(length(unique(nf_tracks$Year_season)), alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
names(date_color) <- unique(nf_tracks$Year_season)
date_color_scale <- scale_colour_manual(name = "Date", values = date_color)

nf_effort_base<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  geom_path(nf_tracks, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT), color = Year_season), size = 0.3)+
  theme_bw()+
  #labs(color = "Year_Season")+
  geom_path(data = sig_2022, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT,SIGHTING_NUMBER), color = Year_season), size = 1)+
  geom_point(data = sig_2022%>%filter(SPECIES %in% species & EVENT_TYPE == "Encounter START"), mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT,SIGHTING_NUMBER), shape = SPECIES), size = 3.3)+
  geom_point(data = sig_2022%>%filter(SPECIES %in% species & EVENT_TYPE == "Encounter START"), mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT,SIGHTING_NUMBER), fill = Year_season, shape = SPECIES, color = Year_season), size = 3)+
  scale_shape_manual(values = c(21:24))

#Charles & Nancy
north<-nf_effort_base+
  coord_sf(crs = 4269, xlim = c(166.99,167.22), ylim = c(-45.2,-45.03))

#Dagg
middle<-nf_effort_base+
  coord_sf(crs = 4269, xlim = c(166.75,166.91), ylim = c(-45.44,-45.37))

#Chalky & Preservation
south<-nf_effort_base+
  coord_sf(crs = 4269, xlim = c(166.4,166.91), ylim = c(-45.88,-46.2))

ggpubr::ggarrange(north, middle, south, common.legend = TRUE, labels = "auto", ncol = 1, legend = "right")

ggplot2::ggsave(paste0("./figures/Supplement/nf_effort.png"), device = "png", dpi = 700, width = 200, height = 400, units = 'mm')

### Taumoana ----

survey<-survey_2022%>%
  filter(DATE < ymd("2022-11-30") & DATE > ymd("2022-02-20")) #after all Dusky are deployed

surv_dates<-survey%>%
  mutate(year_mo = format(parse_date_time(as.character(DATE), "ymd"), "%Y_%m"))%>%
  #filter(LONGITUDE < 166.6)%>%
  filter(year_mo != "2022_05" & year_mo != "2022_10")%>%#did not survey in Dusky, but did in Dagg, Doubtul, and southern fiords
  filter(LATITUDE < -45.20 & LATITUDE > -45.835)%>%
  distinct(DATE)

survey_plot<-surv_dates%>%
  mutate(year_mo = format(parse_date_time(as.character(DATE), "ymd"), "%Y_%m"))%>%
  left_join(survey_2022, by = 'DATE')

species = c("Bottlenose","Common","Dusky","Hector's")

sig_plot<-survey_plot%>%
  filter(ENCOUNTER_TYPE != "")%>%
  tidyr::fill(SIGHTING_NUMBER, SPECIES)%>%
  filter(SPECIES %in% species)

sig_plot%>%
  filter(LONGITUDE < 166.6)%>%
  distinct(DATE)

acoustic_dusky<-all_Cet%>%
  filter(Fiord == "DUSKY" | grepl("MAR",Fiord))%>%
  left_join(deploy%>%filter(grepl("_01", Deployment_number))%>%dplyr::select(-Date), by = "Fiord_recorder")%>%
  inner_join(surv_dates, by = c("Date" = "DATE"))%>%
  dplyr::rename("DATE" = "Date")%>%
  arrange(DATE)

head(acoustic_dusky)

sig_acou<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  geom_path(survey_plot, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT), color = year_mo))+
  geom_path(sig_plot, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,SIGHTING_NUMBER), color = year_mo), linewidth = 3, alpha = 0.4)+
  coord_sf(xlim = c(166.45, 166.6), ylim = c(-45.79, -45.67))+
  geom_point(deploy%>%filter(grepl("_01", Deployment_number)), mapping = aes(x = Longitude, y = Latitude), color = "red", size = 2)+
  geom_point(acoustic_dusky, mapping = aes(x = Longitude, y = Latitude), color = "yellow", size = 1)+
  theme_bw()+
  facet_wrap(~DATE)+
  theme(legend.position = "inside", legend.position.inside =  c(.9, .05))

ggplot2::ggsave(paste0("./figures/Supplement/sig_acou.png"), sig_acou, device = "png", dpi = 700, width = 200, height = 200, units = 'mm')