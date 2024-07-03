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
  filter(LATITUDE < -45.20 & LATITUDE > -45.82)%>%
  distinct(DATE)
  #distinct(DATE, CREW)

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
  coord_sf(xlim = c(166.45, 166.6), ylim = c(-45.8, -45.67))+
  geom_point(deploy%>%filter(grepl("_01", Deployment_number)), mapping = aes(x = Longitude, y = Latitude), color = "red", size = 2)+
  geom_point(acoustic_dusky, mapping = aes(x = Longitude, y = Latitude), color = "yellow", size = 1)+
  theme_bw()+
  facet_wrap(~DATE)+
  theme(legend.position = "inside", legend.position.inside =  c(.9, .05))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(-45.7,-45.8, by = -0.05))+
  scale_x_continuous(breaks = seq(166.5,166.6, by = 0.05))

ggplot2::ggsave(paste0("./figures/Supplement/sig_acou.png"), sig_acou, device = "png", dpi = 700, width = 200, height = 200, units = 'mm')

ST_time<-ST_dol%>%
  ungroup()%>%
  #filter(Fiord == "DUSKY" | grepl("MAR",Fiord))%>%
  mutate(DATE = Date,
         TIME = hms(format(as.POSIXct(Datetime), format = '%H:%M:%S')))%>%
  #filter(DATE < ymd("2022-11-30") & DATE > ymd("2022-02-20"))
  dplyr::select(DATE, TIME, Fiord, type, Datetime)

FPOD_time<-all_FPOD_Dol%>%
  mutate(DATE = Date,
         TIME = hms(format(as.POSIXct(Datetime), format = '%H:%M:%S')))%>%
  #filter(DATE < ymd("2022-11-30") & DATE > ymd("2022-02-20"))
  dplyr::select(DATE, TIME, Fiord, type, Datetime)
  
acou_time<-ST_time%>%
  bind_rows(FPOD_time)%>%
  mutate(Fiord_recorder = paste0(Fiord,'_',type),
         season = case_when(
           month(DATE) == 12 | month(DATE) <=2 ~ "Summer",
           month(DATE) >= 3 & month(DATE) <= 5 ~ "Autumn",
           month(DATE) >= 6 & month(DATE) <= 8 ~ "Winter",
           month(DATE) >= 9 & month(DATE) <= 11 ~ "Spring"
           ))

summary(acou_time)

acou_time%>%
  filter(is.na(DATE))

## without daylight savings -----

sun_times_woDS <-
  suncalc::getSunlightTimes(
    date = seq(
      as.Date(min(acou_time$DATE)),
      as.Date(max(acou_time$DATE)),
      by = "day"
    ),
    lat = deploy$Latitude[1],
    lon = deploy$Longitude[1],
    tz = "Etc/GMT+13",
    keep = c("dawn", "nauticalDawn", "dusk", "nauticalDusk", "sunrise", "sunset")
  )

tidy_sun_times_woDS <-sun_times_woDS %>%
  select(-lat, -lon) %>%
  tidyr::pivot_longer(-date, names_to = "event", values_to = "time") %>%
  mutate(
    #tz = strftime(time, "%Z"),
    time = hms::as_hms(time)
  )

acou_time
acou_sunlight<-ggplot(acou_time)+
  geom_ribbon(tidy_sun_times_woDS%>%filter(event == "dusk"), mapping = aes(x = date, ymin = time, ymax = hms::as_hms("24:00:00")), color = "midnightblue", fill = "midnightblue", alpha = 0.4)+
  geom_ribbon(tidy_sun_times_woDS%>%filter(event == "dawn"), mapping = aes(x = date, ymin = time, ymax = hms::as_hms("00:00:00")), color = "midnightblue", fill = "midnightblue", alpha = 0.4)+
  #geom_point(aes(x = DATE, y = TIME, color = as.factor(season)))+
  geom_point(aes(x = DATE, y = TIME))+
  scale_y_time()+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_FPOD","NANCY_ST","DAGG_FPOD","DAGG_ST","MARINE-RESERVE-1_ST","MARINE-RESERVE-2_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_FPOD")), ncol = 3)+
  #facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","MARINE-RESERVE-1","MARINE-RESERVE-2","DUSKY","CHALKY","PRESERVATION")), ncol = 2)+
  theme_bw()+
  ylab("Time (HH:MM:SS)")+
  scale_x_date("Date",date_breaks="4 months", date_labels="%b-%Y", limits = c(min(acou_time$DATE),max(acou_time$DATE)))

acou_sunlight<-acou_sunlight+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-09-02"), xmax = ymd("2023-11-09"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #MR_2 = FF03
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2022-12-31"), xmax = ymd("2023-06-26"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2023-11-29"), xmax = ymd("2024-02-16"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #MR_1 = FF02
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-1_ST"), aes(xmin = ymd("2022-12-29"), xmax = ymd("2023-02-23"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2022-11-23"), xmax = ymd("2023-02-23"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-02"), xmax = ymd("2023-06-27"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-11-25"), xmax = ymd("2024-02-14"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-10-19"), xmax = ymd("2023-11-09"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_FPOD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_FPOD"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHARLES_FPOD"), aes(xmin = ymd("2023-10-21"), xmax = ymd("2024-01-25"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="black", alpha = 0.6, inherit.aes = FALSE)
  

acou_sunlight$layers<-c(
  #15/30 below everything else
  #geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-02-15"), xmax = ymd("2022-10-07"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE),
  #geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-01-02"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE),
  #geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-03-14"), xmax = ymd("2023-06-20"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE),
  #geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-02-16"), xmax = ymd("2022-07-13"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  #geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-09-02"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  #geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-02-21"), xmax = ymd("2022-11-16"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  #geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-04-28"), xmax = ymd("2023-10-19"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="red", alpha = 0.2, inherit.aes = FALSE),
  #handbrowse
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-04-30"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="blue", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-06-21"), xmax = ymd("2023-10-19"), ymin = hms::as_hms("00:00:00"), ymax = hms::as_hms("24:00:00")), fill="blue", alpha = 0.2, inherit.aes = FALSE),
  acou_sunlight$layers)

ggplot2::ggsave(paste0("./figures/Supplement/acou_sunlight.png"), acou_sunlight, device = "png", dpi = 700, width = 300, height = 200, units = 'mm')

## survey with daylight -----

sun_times_wDS <-
  suncalc::getSunlightTimes(
    date = seq(
      as.Date(min(acou_time$DATE)),
      as.Date(max(acou_time$DATE)),
      by = "day"
    ),
    lat = deploy$Latitude[1],
    lon = deploy$Longitude[1],
    tz = "Pacific/Auckland",
    keep = c("dawn", "nauticalDawn", "dusk", "nauticalDusk", "sunrise", "sunset")
  )

tidy_sun_times_wDS <-
  sun_times_wDS %>%
  select(-lat, -lon) %>%
  tidyr::pivot_longer(-date, names_to = "event", values_to = "time") %>%
  mutate(
    tz = strftime(time, "%Z"),
    time = hms::as_hms(time)
  )

sig_times<-sig_2022%>%filter(grepl("Encounter", EVENT_TYPE))

sig_effort_sunlight<-ggplot(sig_times%>%mutate(time = hms(format(as.POSIXct(DATETIME), format = '%H:%M:%S'))))+
  geom_ribbon(tidy_sun_times_wDS%>%filter(event == "dusk"), mapping = aes(x = date, ymin = time, ymax = hms::as_hms("24:00:00")), color = "darkblue", fill = "darkblue", alpha = 0.4)+
  geom_ribbon(tidy_sun_times_wDS%>%filter(event == "dawn"), mapping = aes(x = date, ymin = time, ymax = hms::as_hms("00:00:00")), color = "darkblue", fill = "darkblue", alpha = 0.4)+
  #geom_point(aes(x = DATE, y = TIME, color = as.factor(season)))+
  geom_point(survey_2022, mapping = aes(x = DATE, y = hms(format(as.POSIXct(DATETIME), format = '%H:%M:%S'))))+
  geom_point(aes(x = DATE, y = time, color = SPECIES))+
  scale_y_time()+
  theme_bw()+
  ylab("Time (HH:MM:SS)")+
  #the not plotted warning includes Jan/Feb 2022 effort before acoustic monitoring
  scale_x_date("Date",date_breaks="4 months", date_labels="%b-%Y", limits = c(min(acou_time$DATE),max(acou_time$DATE)))

survey_2022%>%filter(TIME < "05:00:00")

ggplot2::ggsave(paste0("./figures/Supplement/sig_effort_sunlight.png"), sig_effort_sunlight, device = "png", dpi = 700, width = 300, height = 200, units = 'mm')

### acoustics during the day ----

sun<-sun_times_woDS%>%
  dplyr::select(date, dawn, dusk)

sun$date<-ymd(sun$date)

head(acou_time)

acou_dusky<-acou_time%>%
  filter(Fiord == "DUSKY" | grepl("MAR",Fiord))%>%
  left_join(deploy%>%filter(grepl("_01", Deployment_number))%>%dplyr::select(-Date), by = c("Fiord_recorder","Fiord"))%>%
  left_join(sun, by = c("DATE" = "date"))%>%
  filter(Datetime > dawn & Datetime < dusk)%>%
  inner_join(surv_dates, by = c("DATE"))%>%
  arrange(DATE)

sig_acou<-ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "antiquewhite3", lwd = 0.1)+
  geom_path(survey_plot, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,EVENT), color = year_mo))+
  geom_path(sig_plot, mapping = aes(x = LONGITUDE, y = LATITUDE, group = paste0(DATE,SIGHTING_NUMBER), color = year_mo), linewidth = 3, alpha = 0.4)+
  coord_sf(xlim = c(166.45, 166.6), ylim = c(-45.8, -45.67))+
  geom_point(deploy%>%filter(grepl("_01", Deployment_number)), mapping = aes(x = Longitude, y = Latitude), color = "red", size = 2)+
  geom_point(acoustic_dusky, mapping = aes(x = Longitude, y = Latitude), color = "black", size = 1.5)+
  geom_point(acou_dusky, mapping = aes(x = Longitude, y = Latitude), color = "yellow", size = 1)+
  theme_bw()+
  facet_wrap(~DATE)+
  theme(legend.position = "inside", legend.position.inside =  c(.9, .05))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(-45.7,-45.8, by = -0.05))+
  scale_x_continuous(breaks = seq(166.5,166.6, by = 0.05))

ggplot2::ggsave(paste0("./figures/Supplement/sig_acou2.png"), sig_acou, device = "png", dpi = 700, width = 200, height = 200, units = 'mm')
