library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%
  filter(!grepl("FF01",Deployment_number))%>%
  filter(!grepl("FF02",Deployment_number))%>%
  mutate(Fiord = case_when(
    grepl("FF0", Deployment_number) ~ "MARINE-RESERVE",
    TRUE ~ Fiord))%>%
  mutate(Fiord_recorder = paste0(Fiord,"_",Recorder_type))

deploy_tz<-deploy%>%filter(Recorder_type == "ST")%>%dplyr::select(Deployment_number, daylight_adjust)

deploy_tz%>%filter(grepl("Dagg", Deployment_number))%>%filter(daylight_adjust == 1)

# FPOD data ----

FPOD_data_list<-list.files("./data/FPOD", pattern = "*train details.csv", full.names = T, recursive = T)

FPOD_data<-lapply(FPOD_data_list, function(x){
  y<-read.csv(x)
  colnames(y)[3]<-"Min" #Nick renamed this "Minutes" in a later v1
  y}
)
head(FPOD_data[7])
head(FPOD_data[6])

all_FPOD<-bind_rows(FPOD_data)%>%
  mutate(Fiord = toupper(stringr::str_extract(File, '^[^01]+')))%>%
  mutate(Datetime = ymd_hm("1899-12-30 00:00") + minutes(Min),
         Date = as.Date(Datetime))%>%
  filter(!(grepl("Charles0104", File) & Datetime >= ymd_hms("2023-05-13 15:44:00")))%>% #deployment end, cut out time transiting back to me
  filter(!(grepl("Preservation0105", File) & Datetime >= ymd_hms("2023-11-02 07:44:00")))%>% #eployment end, cut out time transiting back to me
  mutate(type = "FPOD")%>%
  mutate(Quality = Qn)

all_FPOD_Dol<-all_FPOD%>%  
  filter(SpClass == "Dol")

all_FPOD_Cet<-all_FPOD_Dol%>%
  group_by(Date, Fiord, Quality)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Quality)%>%
  mutate(Fiord_recorder = paste0(Fiord,"_FPOD"))
  

# ST/FinFinder review ----

ST_data_list<-list.files("./data/ST", pattern = "*.txt", full.names = T, recursive = T)

ST_data<-lapply(ST_data_list, function(x)
  read.delim(x, header = T)
)

all_ST<-bind_rows(ST_data)%>%
  mutate(Fiord = toupper(stringr::str_extract(Begin.Path, '^[^01]+')))%>%
  mutate(Fiord = substr(Fiord, 4, nchar(Fiord)))%>%
  mutate(Datetime = ymd_hms(Begin.Date.Time),
         Date = as.Date(Begin.Date.Time))%>%
  filter(Selection != "NA")%>%
  mutate(Deployment_number = stringr::str_extract(Begin.Path, '^([^_]*_){2}'))%>%#END BEFORE 2ND UNDERSCORE
  mutate(Deployment_number = substr(Deployment_number, 4, nchar(Deployment_number)-1))%>%
  left_join(deploy_tz, by = 'Deployment_number')%>%
  #adjust an hour during daylight savings time since this has happened fo the ST data automatically at some stage
  mutate(Datetime = case_when(
    Datetime >= "2022-04-03 03:00:00" & Datetime <= "2022-09-25 02:00:00" ~ Datetime + hours(1),
    Datetime >= "2023-04-02 03:00:00" & Datetime <= "2023-09-24 02:00:00" ~ Datetime + hours(1),
    TRUE ~ Datetime
  ))%>%
  mutate(Fiord = case_when(
    Fiord == "ANCHOR" ~ "DUSKY",
    Fiord == "FF" ~ "MARINE-RESERVE",
    TRUE ~ Fiord
  ))%>%
  mutate(type = "ST")%>%
  #FPOD is synced with GPS time but with ST time coming from file, also adjusted for each deployment in Dagg 15 bin comparison
  mutate(Datetime = case_when(
    Deployment_number == "Dagg01_01" & type == "ST" ~ Datetime + minutes(4),
    Deployment_number == "Dagg01_02" & type == "ST" ~ Datetime - minutes(3),
    Deployment_number == "Dagg01_03" & type == "ST" ~ Datetime - minutes(0),
    Deployment_number == "Dagg01_04" & type == "ST" ~ Datetime - minutes(0),
    Deployment_number == "Dagg01_05" & type == "ST" ~ Datetime - minutes(0),
    TRUE ~ Datetime
  ))

#write.csv(all_ST, paste0('./data/all_ST_', Sys.Date(),'.csv'), row.names = F)


all_ST_Cet<-all_ST%>%
  filter(Dolphin...y.n. == "y" | Dolphin...y.n. == "?")%>%
  filter(Species == "Bottlenose")%>%
  group_by(Date, Fiord)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  mutate(SpClass = "Dol",
         Quality = case_when(
           Dolphin...y.n. == "y" ~ "H",
           Dolphin...y.n. == "?" ~ "?",
         ))%>%
  distinct(Date, Fiord, DPD, SpClass, Quality)%>%
  mutate(Fiord_recorder = paste0(Fiord,"_ST"))%>%
  filter(Quality != '?')


all_ST%>%
  filter(Dolphin...y.n. == "")

all_ST%>%
  filter(is.na(Fiord))

# NBHF ----

#add in UNx as potential NBHF

unique(all_FPOD$SpClass)

nbhf_time<-all_FPOD%>%
  filter(SpClass == "NBHF" | SpClass == "UNx")%>%
  arrange(Date)

nbhf_day<-nbhf_time%>%
  filter(SpClass == "NBHF")%>%
  group_by(Date, Fiord, Quality)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Quality)

nbhf_day%>%
  group_by(Date)%>%
  tally()

#includes nbhf and unx for raw output from KERNO before manually validating
write.csv(nbhf_time, paste0('./data/Fiordland_nbhf_unx_', Sys.Date(),'_v1.csv'), row.names = F)


nrow(nbhf_time)
nrow(nbhf_time%>%filter(SpClass == "NBHF"))
nrow(nbhf_time%>%filter(SpClass == "UNx" & Fiord == "PRESERVATION"))

# acou_timeline(all_FPOD%>%filter(SpClass == "UNx"))+
#   facet_wrap(~Fiord)

# ADW validation

FPOD_data_list_ADW<-list.files("./data/FPOD ADW", pattern = "*train details.csv", full.names = T, recursive = T)

FPOD_data_ADW<-lapply(FPOD_data_list_ADW, function(x){
  y<-read.csv(x)
  colnames(y)[3]<-"Min" #Nick renamed this "Minutes" in a later v1
  y}
)

all_ADW<-bind_rows(FPOD_data_ADW)%>%
  mutate(Fiord = toupper(stringr::str_extract(File, '^[^01]+')))%>%
  mutate(Datetime = ymd_hm("1899-12-30 00:00") + minutes(Min),
         Date = as.Date(Datetime))%>%
  filter(!(grepl("Charles0104", File) & Datetime >= ymd_hms("2023-05-13 15:44:00")))%>% #deployment end, cut out time transiting back to me
  filter(!(grepl("Preservation0105", File) & Datetime >= ymd_hms("2023-11-02 07:44:00")))%>% #eployment end, cut out time transiting back to me
  mutate(type = "FPOD")%>%
  mutate(Quality = Qn)

unique(all_ADW$SpClass)

nbhf_time_ADW<-all_ADW%>%
  filter(SpClass == "NBHF")%>%
  arrange(Date)

nbhf_day_ADW<-nbhf_time_ADW%>%
  filter(SpClass == "NBHF")%>%
  group_by(Date, Fiord, Quality)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Quality)

nbhf_day_ADW%>%
  group_by(Date)%>%
  tally()

# plot function ----

acou_timeline<-function(x){
  ggplot(x)+
    geom_col(aes(x = Date, y = 1, fill = Quality))+
    ylim(c(0,1))+
    theme_bw()+
    scale_fill_brewer(palette = "Paired")+
    ylab("")+
    #xlim(c(min(as.Date(deploy$Datetime_deployment_local), na.rm = T), max(as.Date(deploy$Datetime_deployment_local), na.rm = T)))+
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(angle=45,hjust=1))+
          #strip.background = element_rect(aes(fill = Fiord), colour = "black", size = 1))+
    scale_x_date(date_breaks="1 month", date_labels="%b-%Y", limits = c(min(as.Date(deploy$Datetime_deployment_local), na.rm = T), max(as.Date(deploy$Datetime_retrieval_local), na.rm = T)))
}

# all together

all_Cet<-all_FPOD_Cet%>%
  rbind(all_ST_Cet)

unique(all_Cet$Fiord_recorder)

all_Cet$Quality<-factor(all_Cet$Quality, levels = c("?","L","M","H"))

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_FPOD","NANCY_ST","DAGG_FPOD","DAGG_ST","MARINE-RESERVE_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_FPOD")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(deploy%>%group_by(Fiord)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local)), mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")

all_Cet_plot<-all_Cet_plot+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-09-02"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE_ST"), aes(xmin = ymd("2022-12-31"), xmax = ymd("2023-06-26"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2022-07-06"), xmax = ymd("2023-02-23"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-02"), xmax = ymd("2023-06-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-10-19"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_FPOD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_FPOD"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #to be analysed
  geom_rect(data = data.frame(Fiord_recorder = "CHARLES_FPOD"), aes(xmin = ymd("2023-05-13"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-06-20"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-09-02"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE_ST"), aes(xmin = ymd("2023-06-26"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-06-21"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)
  
all_Cet_plot$layers<-c(
  #15/30 below everything else
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-02-15"), xmax = ymd("2022-10-07"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-01-02"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-03-14"), xmax = ymd("2023-06-20"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-02-16"), xmax = ymd("2022-07-13"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-09-02"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-02-21"), xmax = ymd("2022-11-16"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-04-28"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
                       all_Cet_plot$layers)

all_Cet_plot
ggsave('./figures/allcet_v1.png',all_Cet_plot, dpi = 300, width = 175, height = 175, units = "mm")

all_Cet_plot+
  xlim(c(ymd("2022-05-01"), ymd("2023-11-15")))

###

nbhf_day$Quality<-factor(nbhf_day$Quality, levels = c("?","L","M","H"))

all_FPOD_NBHF_plot<-acou_timeline(nbhf_day)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  geom_vline(deploy%>%filter(Recorder_type == 'FPOD'), mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(deploy%>%filter(Recorder_type == 'FPOD')%>%group_by(Fiord)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local)), mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")+
  geom_rect(data = data.frame(Fiord = "DAGG"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "PRESERVATION"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)

all_FPOD_NBHF_plot

ggsave('./figures/NBHF_v1.png', all_FPOD_NBHF_plot, dpi = 300, width = 175, height = 125, units = "mm")


ADW_nbhf<-acou_timeline(nbhf_day_ADW)+
  xlim(c(min(nbhf_day_ADW$Date),max(nbhf_day_ADW$Date)))

kerno_nbhf<-acou_timeline(all_FPOD%>%filter(SpClass == "NBHF" & Fiord == "DAGG"))+
  xlim(c(min(nbhf_day_ADW$Date),max(nbhf_day_ADW$Date)))

ggpubr::ggarrange(ADW_nbhf, kerno_nbhf, nrow = 2)

###

trunc_nbhf_othercet<-ggplot(all_FPOD%>%filter(SpClass == "NBHF" | SpClass == "Dol" | SpClass == "UNx"))+
  geom_col(aes(x = Date, y = 1, fill = Quality))+
  facet_wrap(~SpClass+Fiord, ncol = 3)+
  ylim(c(0,1))+
  #xlim(c(min(nbhf_time$Date)-1, max(nbhf_time$Date)+1))+
  ylab("")+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

trunc_nbhf_othercet
ggsave('./figures/trunc_nbhf_othercet_v1.png', trunc_nbhf_othercet, dpi = 300, width = 300, height = 80, units = "mm")

locations_FPOD<-deploy%>%filter(Recorder_type == 'FPOD' & !is.na(Recorder_start_local))%>%arrange(Fiord, Recorder_start_local)

write.csv(locations_FPOD, paste0('./data/Fiordland_FPOD_locations_', Sys.Date(),'.csv'), row.names = F)

#all things detected on FPOD
acou_timeline(all_FPOD)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","DUSKY","CHALKY","PRESERVATION")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

# days listening
listening<-deploy%>%
  ungroup()%>%
  filter(Deployment_number != "Charles01_05" & Deployment_number != "Nancy01_06" & 
       !(Deployment_number == "Dagg01_05" & Recorder_type == "ST") & !(Deployment_number == "Dagg01_06" & Recorder_type == "ST"))%>%
  mutate(Fiord_recorder = paste0(Fiord,"_",Recorder_type))%>%
  group_by(Fiord_recorder)%>%
  dplyr::summarise(min_date = as.Date(min(Datetime_deployment_local, na.rm= T)), max_date = as.Date(max(Datetime_retrieval_local, na.rm = T)))%>%
  ungroup()%>%
  # mutate(max_date = case_when(
  #   Fiord == "DUSKY" ~ as.Date("2022-07-06"),
  #   TRUE ~ max_date
  # ))%>%
  mutate(days = max_date - min_date)%>%
  ungroup()%>%
  mutate(dead = case_when(
    Fiord_recorder == "NANCY_ST" ~ (ymd("2022-11-27") - ymd("2022-10-07")) + (ymd("2023-03-14") - ymd("2023-01-02")),
    Fiord_recorder == "DAGG_ST" ~ (ymd("2022-11-27") - ymd("2022-11-15")) + (ymd("2023-11-09") - ymd("2023-09-02")), #once analysis is done, change to "2023-09-02"
    Fiord_recorder == "DAGG_FPOD" ~ (ymd("2023-11-09") - ymd("2023-05-24")),
    Fiord_recorder == "CHALKY_ST" ~ (ymd("2023-04-28") - ymd("2022-11-16")) + (ymd("2023-11-09") - ymd("2023-10-19")),
    Fiord_recorder == "PRESERVATION_FPOD" ~ (ymd("2023-04-28") - ymd("2023-03-15")),
    #no deployment FF03_03
    Fiord_recorder == "MARINE-RESERVE_ST" ~ (ymd("2023-06-23") - ymd("2022-12-31")),
    #change when Dusky analysed
    Fiord_recorder == "DUSKY_ST" ~ (ymd("2023-06-27") - ymd("2023-06-02")) + (ymd("2023-02-23") - ymd("2022-07-06")),
    TRUE ~ (ymd("2023-08-09") - ymd("2023-08-09"))
  ))%>%
  mutate(active = days - dead)

## all together, daily detection rate
daily_det_rate<-all_Cet%>%
  ungroup()%>%
  distinct(Fiord_recorder, Date)%>%
  group_by(Fiord_recorder)%>%
  tally()%>%
  left_join(listening, by = "Fiord_recorder")%>%
  mutate(det_pres = round(n/as.numeric(active), 2))%>%
  as.data.frame()%>%
  arrange(-det_pres)

daily_det_rate_table<-daily_det_rate%>%
  dplyr::select(Fiord_recorder, n, active, det_pres)%>%
  mutate(active = as.numeric(active))%>%
  dplyr::rename(`Acoustic detection days` = "n", `Recorder active days` = "active", `Detection rate (daily)` = "det_pres")

library(kableExtra)

daily_det_rate_table%>%
  kbl()%>%
  kable_classic(full_width = F, html_font = "Cambria")

## seasonally? I don't know if I really want to go down this route
## this is going to need to take into consideration periods of inactivity
season<-all_Cet%>%
  ungroup()%>%
  distinct(Fiord_recorder, Date)%>%
  mutate(season = case_when(
    month(Date) == 12 | month(Date) <= 2 ~ "summer",
    month(Date) >= 3 & month(Date) <= 5 ~ "autumn",
    month(Date) >= 6 & month(Date) <= 8 ~ "winter",
    month(Date) >= 9 & month(Date) <= 11 ~ "spring"
  ))%>%
  group_by(year(Date), Fiord_recorder, season)%>%
  tally()%>%
  left_join(listening, by = "Fiord_recorder")%>%
  mutate(det_pres = case_when(
    season == "summer" ~ n/90,
    season == "spring" ~ n/91,
    TRUE ~ n/92))%>%
  as.data.frame()

ggplot(season)+
  geom_col(aes(x = paste0(`year(Date)`,"_",season), y = det_pres, fill = season),color = "black", position = "stack", alpha = 0.7)+
  facet_wrap(~Fiord_recorder)

# Dagg ----

head(all_FPOD_Dol)

FPOD_DAGG<-all_FPOD_Dol%>%
  filter(Fiord == "DAGG")%>%
  dplyr::select(Date,Datetime,Quality,File)%>%
  mutate(Deployment_number = substr(File,1,8))%>%
  dplyr::select(-File)%>%
  mutate(type = "FPOD")

head(FPOD_DAGG)

ST_DAGG<-all_ST%>%
  filter(Fiord == "DAGG")%>%
  filter(Dolphin...y.n. == "y" | Dolphin...y.n. == "?")%>%
  mutate(Quality = case_when(
    Dolphin...y.n. == "y" ~ "H",
    Dolphin...y.n. == "?" ~ "?",
  ))%>%
  dplyr::select(Date,Datetime,Quality,Deployment_number, Begin.Path)%>%
  filter(Quality != "?")%>%
  mutate(Deployment_number = gsub("_", "", Deployment_number))%>%
  mutate(begin_time = ymd_hms(paste0("20",substr(Begin.Path, 24, nchar(Begin.Path) - 4))))%>%
  mutate(type = "ST")%>%
  mutate(begin_time = case_when(
    Deployment_number == "Dagg0101" & type == "ST" ~ begin_time + minutes(4),
    TRUE ~ begin_time
  ))

head(ST_DAGG)

DAGG<-FPOD_DAGG%>%
  bind_rows(ST_DAGG)%>%
  arrange(Datetime)

nrow(DAGG)
head(DAGG)

# Daily differences in DAGG ----

# 15 min ST sampling

sampling<-DAGG%>%
  filter(Deployment_number != "Dagg0103" & Deployment_number != "Dagg0105")%>%
  distinct(Date, type)%>%
  group_by(Date)%>%
  mutate(`Number of recorders` = n())%>%
  group_by(`Number of recorders`)%>%
  mutate(`Detection days` = n())%>%
  distinct(`Number of recorders`, `Detection days`)%>%
  ungroup()%>%
  mutate(`Percent of detection days` = round(`Detection days`/sum(`Detection days`) * 100, 0))%>%
  arrange(-`Number of recorders`)%>%
  mutate(`Listening days` = as.numeric(round(ymd_hms("2023-02-20 11:30:00") - ymd_hms("2022-02-16 13:39:00"), 0)) -
           as.numeric(round(ymd_hms("2022-11-27 16:06:00") - ymd_hms("2022-07-13 13:33:00"), 0)))%>%
  dplyr::select(`Listening days`, everything())

sampling$`Listening days`[2]<-""

sampling%>%
  kableExtra::kable(booktabs = T, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  add_header_above(c("15/30min ST & continuous FPOD" = 4))

# continuous ST sampling

cont<-DAGG%>%
  filter(Deployment_number == "Dagg0103" & Datetime <= ymd_hms("2022-11-14 22:30:24"))%>%
  distinct(Date, type)%>%
  group_by(Date)%>%
  mutate(`Number of recorders` = n())%>%
  group_by(`Number of recorders`)%>%
  mutate(`Detection days` = n())%>%
  distinct(`Number of recorders`, `Detection days`)%>%
  ungroup()%>%
  mutate(`Percent of detection days` = round(`Detection days`/sum(`Detection days`) * 100, 1))%>%
  arrange(-`Number of recorders`)%>%
  mutate(`Listening days` = as.numeric(round(ymd_hms("2022-11-14 22:30:24") - ymd_hms("2022-07-13 17:05:00"), 0)))%>%
  dplyr::select(`Listening days`, everything())

cont$`Listening days`[2]<-""

cont%>%
  kableExtra::kable(booktabs = T, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  add_header_above(c("continuous ST & FPOD" = 4))

# why might they not be getting detections on same day?
tally_detection<-DAGG %>%
  distinct(Date, type)%>%
  group_by(Date)%>%
  tally()

one_gear_detection<-tally_detection%>%
  filter(n != 2)%>%
  left_join(DAGG, by ='Date')

FPOD_only<-one_gear_detection%>%
  filter(type == "FPOD")

ST_only<-one_gear_detection%>%
  filter(type == "ST")

write.csv(one_gear_detection, paste0('./data/one_gear_detection_', Sys.Date(),'.csv'), row.names = F)

one_det<-read_excel("./data/one_gear_detection_2023-10-21.xlsx")
head(one_det)

one_det<-one_det%>%
  mutate(clicks = case_when(
    grepl("click", tolower(`Explanation?`)) ~ "Y",
    TRUE ~ "N"
  ),
  duty = case_when(
    grepl("duty", tolower(`Explanation?`)) ~ "Y",
    TRUE ~ "N"
  ))

#dagg01_01, 02, 04 15/30 sampling period
one_det%>%
  filter(Datetime <= ymd_hms("2023-02-20 11:30:00"))%>%
  filter(Datetime <= ymd_hms("2022-07-13 13:33:00") | Date >= ymd_hms("2022-11-27 16:06:00"))%>%
  distinct(Date, clicks, duty)%>%
  group_by(clicks, duty)%>%
  tally()

one_det%>%
  filter(Datetime <= ymd_hms("2023-02-20 11:30:00"))%>%
  filter(Datetime <= ymd_hms("2022-07-13 13:33:00") | Date >= ymd_hms("2022-11-27 16:06:00"))%>%
  filter(clicks == "N" & duty == "N")%>% as.data.frame()

#dagg01_03 continuous sampling period
one_det%>%
  distinct(Date, clicks, duty)%>%
  filter(Date >= ymd("2022-07-13") & Date <= ymd("2022-11-14"))


# binning into 15 min ----

##need list of all files, not just those with detections

Dagg_file_list<-list.files("./data/Leah_file_list", pattern = "*.csv", full.names = T, recursive = T)

Dagg_ST_files<-lapply(Dagg_file_list, function(x)
  read.csv(x, header = T)
)

Dagg_ST_files<-bind_rows(Dagg_ST_files)%>%
  arrange(begin_time)%>%
  mutate(Deployment_number = gsub("_","",substr(Begin.Path, 4, 12)))%>%
  mutate(begin_time = case_when(
    ymd_hms(begin_time) >= "2022-04-03 03:00:00" & ymd_hms(begin_time) <= "2022-09-25 02:00:00" ~ ymd_hms(begin_time) + hours(1),
    ymd_hms(begin_time) >= "2023-04-02 03:00:00" & ymd_hms(begin_time) <= "2023-09-24 02:00:00" ~ ymd_hms(begin_time) + hours(1),
    TRUE ~ ymd_hms(begin_time)
  ))%>%
  mutate( # adjust for time difference between FPOD and ST -- FPOD is synced with GPS time on deployment, ST may not have been when paramaterized with computers used primarily offline
    begin_time = case_when(
      Deployment_number == "Dagg0101" ~ ymd_hms(begin_time) + minutes(4),
      TRUE ~ ymd_hms(begin_time)))%>%
  mutate(end_time = case_when(
    Deployment_number != "Dagg0103" ~ ymd_hms(begin_time) + minutes(15),
    Deployment_number == "Dagg0103" ~ ymd_hms(begin_time) + minutes(60)),
         bin_num = 1:n())%>%
  #give wiggle room for binning FPOD data
  mutate(beg_time_FPOD = floor_date(ymd_hms(begin_time) - minutes(1), "minute"), #floor the minute because of the one minute binning? subtract a minute because can only really adjust FPOD time in software to within one minute
         end_time_FPOD = ceiling_date(ymd_hms(end_time) + minutes(1), "minute")) #cieling the minute because of the one minute binning? subtract a minute because can only really adjust FPOD time in software to within one minute


head(Dagg_ST_files)
summary(Dagg_ST_files)
nrow(Dagg_ST_files)

#percent of Dagg files with dolphin detections
nrow(ST_DAGG%>%distinct(Begin.Path))/nrow(Dagg_ST_files)

# assign bin number to detections from FinFinder
head(ST_DAGG)

ST_DAGG_bin<-ST_DAGG%>%
  mutate(Begin.Path = gsub("\\\\","/",Begin.Path))%>%
  left_join(Dagg_ST_files, by = c("Begin.Path", "Deployment_number"))

head(ST_DAGG_bin)

ST_DAGG_bin<-ST_DAGG_bin%>%
  dplyr::rename("begin_time" = "begin_time.x")%>%
  dplyr::select(-begin_time.y)

# assign bin number to detections from FPOD
head(FPOD_DAGG)

library(fuzzyjoin)
#the below takes about 20min Nov 2023
print(Sys.time())
# join FPOD data to bin, but give a one minute buffer
FPOD_DAGG_bin<-FPOD_DAGG%>%
  fuzzy_left_join(Dagg_ST_files, #join badData to df
                  by = c("Datetime" = "beg_time_FPOD", #variables to join by
                         "Datetime" = "end_time_FPOD"),
                  match_fun=list(`>=`, `<=`))
print(Sys.time())

write.csv(FPOD_DAGG_bin, paste0('./data/FPOD_DAGG_bin_', Sys.Date(),'.csv'), row.names = F)

FPOD_DAGG_bin<-FPOD_DAGG_bin%>%
  dplyr::rename("Deployment_number" = "Deployment_number.x")%>%
  dplyr::select(-Deployment_number.y)

head(FPOD_DAGG_bin)

FPOD_DAGG_bin$begin_time<-ymd_hms(FPOD_DAGG_bin$begin_time)

# all together now

both_bin<-ST_DAGG_bin%>%
  bind_rows(FPOD_DAGG_bin)%>%
  arrange(Datetime)

bins<-both_bin%>%
  distinct(Deployment_number, bin_num, type)%>%
  group_by(Deployment_number, bin_num)%>%
  tally()
  
nrow(bins)

bins_one<-bins%>%filter(n == 1)%>%
  left_join(Dagg_ST_files, by = "bin_num")
both<-bins%>%filter(n == 2)%>%
  left_join(Dagg_ST_files, by = "bin_num")

both
bins_one
#######

# ST deployments with ?
all_ST%>%
  filter(Dolphin...y.n. == "?")%>%
  distinct(Deployment_number)

ggplot()+
  geom_point(both_bin, mapping = aes(x = Datetime, y = type))+
  #xlim(c(ymd_hms("2022-03-21 00:00:01"),ymd_hms("2022-03-24 00:00:01")))+
  geom_rect(Dagg_ST_files, mapping = aes(xmin = beg_time_FPOD, xmax = end_time_FPOD, ymin = "Bin", ymax = "ST"), fill = "blue", alpha = 0.5)+
  #geom_label(Dagg_ST_files, mapping = aes(x = begin_time, y = "Bin"),label = Dagg_ST_files$bin_num)+
  geom_point(both, mapping = aes(x = begin_time + minutes(10), y = "Bin"), color = "red", size = 3)
  

both_bin%>%
  filter(bin_num == 7773)

both%>%filter(bin_num == 7773)
