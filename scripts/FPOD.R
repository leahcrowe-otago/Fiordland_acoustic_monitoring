library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%filter(Fiord != "DUSKY")#%>%
  #filter(Datetime_deployment < "2023-03-01 00:00:00")

FPOD_data_list<-list.files("./data/FPOD data", pattern = "*.csv", full.names = T)

FPOD_data<-lapply(FPOD_data_list, function(x)
  read.csv(x)
  )

all<-bind_rows(FPOD_data)%>%
  mutate(Fiord = toupper(stringr::str_extract(File, '^[^01]+')))%>%
  mutate(Datetime = ymd_hm("1899-12-30 00:00") + minutes(Min),
         Date = as.Date(Datetime))%>%
  filter(!(grepl("Charles0104", File) & Datetime >= ymd_hms("2023-05-13 15:44:00")))

unique(all$Qn)

all$Qn<-factor(all$Qn, levels = c("?","L","M","H"))

unique(all$SpClass)

all_Cet<-all%>%  
  filter(SpClass == "Dol")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

nbhf_time<-all%>%
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
        legend.position = "bottom")
}

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

all_Cet_plot

ggsave('./figures/allcet_v1.png',all_Cet_plot, dpi = 300, width = 175, height = 125, units = "mm")

all_NBHF_plot<-acou_timeline(nbhf_day)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  geom_vline(deploy%>%filter(Recorder_type == 'F-POD'), mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

ggsave('./figures/NBHF_v1.png', all_NBHF_plot, dpi = 300, width = 175, height = 125, units = "mm")

all_Cet_plot+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord = "NANCY"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "NANCY"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "DAGG"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "CHALKY"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord = "PRESERVATION"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)

trunc_nbhf_othercet<-ggplot(all%>%filter(SpClass == "NBHF" | SpClass == "Dol"))+
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

#all things
acou_timeline(all)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")
