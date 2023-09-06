library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%filter(Fiord != "DUSKY")%>%
  mutate(Fiord_recorder = paste0(Fiord,"_",Recorder_type))

# FPOD data ----

FPOD_data_list<-list.files("./data/FPOD", pattern = "*train details.csv", full.names = T, recursive = T)

FPOD_data<-lapply(FPOD_data_list, function(x)
  read.csv(x)
  )

all_FPOD<-bind_rows(FPOD_data)%>%
  mutate(Fiord = toupper(stringr::str_extract(File, '^[^01]+')))%>%
  mutate(Datetime = ymd_hm("1899-12-30 00:00") + minutes(Min),
         Date = as.Date(Datetime))%>%
  filter(!(grepl("Charles0104", File) & Datetime >= ymd_hms("2023-05-13 15:44:00")))

all_FPOD_Cet<-all_FPOD%>%  
  filter(SpClass == "Dol")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)%>%
  mutate(Fiord_recorder = paste0(Fiord,"_FPOD"))

# FinFinder review

ST_data_list<-list.files("./data/ST", pattern = "*.txt", full.names = T, recursive = T)

ST_data<-lapply(ST_data_list, function(x)
  read.delim(x, header = T)
)

all_ST<-bind_rows(ST_data)%>%
  mutate(Fiord = toupper(stringr::str_extract(Begin.Path, '^[^01]+')))%>%
  mutate(Fiord = substr(Fiord, 4, nchar(Fiord)))%>%
  mutate(Datetime = ymd_hms(Begin.Date.Time),
         Date = as.Date(Begin.Date.Time))

all_ST_Cet<-all_ST%>%
  filter(Dolphin...y.n. == "y" | Dolphin...y.n. == "?")%>%
  group_by(Date, Fiord)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  mutate(SpClass = "Dol",
         Qn = case_when(
           Dolphin...y.n. == "y" ~ "H",
           Dolphin...y.n. == "?" ~ "?",
         ))%>%
  distinct(Date, Fiord, DPD, SpClass, Qn)%>%
  mutate(Fiord_recorder = paste0(Fiord,"_ST"))%>%
  filter(Qn != '?')

# NBHF ----

nbhf_time<-all_FPOD%>%
  filter(SpClass == "NBHF")%>%
  arrange(Date)

nbhf_day<-nbhf_time%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

nbhf_day%>%
  group_by(Date)%>%
  tally()

write.csv(nbhf_time, paste0('./data/Fiordland_nbhf_', Sys.Date(),'_v1.csv'), row.names = F)

# plot function ----

acou_timeline<-function(x){
  ggplot(x)+
  geom_col(aes(x = Date, y = 1, fill = Qn))+
  ylim(c(0,1))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  ylab("")+
  xlim(c(min(as.Date(deploy$Datetime_deployment_local), na.rm = T), max(as.Date(deploy$Datetime_deployment_local), na.rm = T)))+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.text.x=element_text(angle=45,hjust=1))+
  scale_x_date(date_breaks="1 month", date_labels="%b-%Y")
}

# all together

all_Cet<-all_FPOD_Cet%>%
  rbind(all_ST_Cet)

all_Cet$Qn<-factor(all_Cet$Qn, levels = c("?","L","M","H"))

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_FPOD","NANCY_ST","DAGG_FPOD","DAGG_ST","CHALKY_ST","PRESERVATION_FPOD")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

all_Cet_plot<-all_Cet_plot+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_FPOD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)

ggsave('./figures/allcet_v1.png',all_Cet_plot, dpi = 300, width = 175, height = 125, units = "mm")

all_Cet_plot+
  xlim(c(ymd("2022-05-01"), ymd("2022-07-15")))

###

all_FPOD_NBHF_plot<-acou_timeline(nbhf_day)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  geom_vline(deploy%>%filter(Recorder_type == 'F-POD'), mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

ggsave('./figures/NBHF_v1.png', all_FPOD_NBHF_plot, dpi = 300, width = 175, height = 125, units = "mm")

###

trunc_nbhf_othercet<-ggplot(all_FPOD%>%filter(SpClass == "NBHF" | SpClass == "Dol"))+
  geom_col(aes(x = Date, y = 1, fill = Qn))+
  facet_wrap(~SpClass+Fiord, ncol = 3)+
  ylim(c(0,1))+
  xlim(c(min(nbhf_time$Date)-1, max(nbhf_time$Date)+1))+
  ylab("")+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave('./figures/trunc_nbhf_othercet_v1.png', trunc_nbhf_othercet, dpi = 300, width = 300, height = 80, units = "mm")

locations_FPOD<-deploy%>%filter(Recorder_type == 'F-POD' & !is.na(Recorder_start_local))

write.csv(locations_FPOD, paste0('./data/Fiordland_FPOD_locations_', Sys.Date(),'.csv'), row.names = F)

#all things detected
acou_timeline(all_FPOD)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

# days listening
listening<-deploy%>%
  group_by(Fiord)%>%
  dplyr::summarise(min_date = as.Date(min(Datetime_deployment_local, na.rm= T)), max_date = as.Date(max(Datetime_retrieval_local, na.rm = T)))%>%
  mutate(days = max_date - min_date)%>%
  mutate(dead = case_when(
    Fiord == "NANCY" ~ (ymd("2022-11-27") - ymd("2022-10-07")) + (ymd("2023-03-14") - ymd("2023-01-02")),
    #Fiord == "DAGG" ~ (ymd("2022-11-27") - ymd("2022-11-15")), #this is for soundtrap!
    Fiord == "CHALKY" ~ (ymd("2023-04-28") - ymd("2022-11-16")),
    Fiord == "PRESERVATION" ~ (ymd("2023-04-28") - ymd("2023-03-15")),
    TRUE ~ 0
  ))%>%
  mutate(active = days - dead)

## all together
all_FPOD_Cet%>%
  ungroup()%>%
  mutate(season = case_when(
    month(Date) == 12 | month(Date) <= 2 ~ "summer",
    month(Date) >= 3 & month(Date) <= 5 ~ "autumn",
    month(Date) >= 6 & month(Date) <= 8 ~ "winter",
    month(Date) >= 9 & month(Date) <= 11 ~ "spring"
  ))%>%
  distinct(Fiord, Date)%>%
  group_by(Fiord)%>%
  tally()%>%
  left_join(listening, by = "Fiord")%>%
  mutate(det_pres = n/as.numeric(active))

## seasonally? I don't know if I really want to go down this route

all_FPOD_Cet%>%
  ungroup()%>%
  distinct(Fiord, Date)%>%
  mutate(season = case_when(
    month(Date) == 12 | month(Date) <= 2 ~ "summer",
    month(Date) >= 3 & month(Date) <= 5 ~ "autumn",
    month(Date) >= 6 & month(Date) <= 8 ~ "winter",
    month(Date) >= 9 & month(Date) <= 11 ~ "spring"
  ))%>%
  group_by(Fiord, season)%>%
  tally()%>%
  left_join(listening, by = "Fiord")%>%
  mutate(det_pres = n/as.numeric(active))

