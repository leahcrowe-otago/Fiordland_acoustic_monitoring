library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%filter(Fiord != "DUSKY")#%>%
  #filter(Datetime_deployment < "2023-03-01 00:00:00")

charles01_01<-read.delim('./data/FPOD data/Charles0101 2022 02 15 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
charles01_02<-read.delim('./data/FPOD data/Charles0102 2022 05 06 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
charles01_03<-read.delim('./data/FPOD data/Charles0103 2022 07 13 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

dagg01_01<-read.delim('./data/FPOD data/Dagg0101 2022 02 16 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_02<-read.delim('./data/FPOD data/Dagg0102 2022 05 07 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_03<-read.delim('./data/FPOD data/Dagg0103 2022 07 13 FPOD_6193 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_04<-read.delim('./data/FPOD data/Dagg0104 2022 11 27 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_05<-read.delim('./data/FPOD data/Dagg0105 2023 02 20 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

pres01_01<-read.delim('./data/FPOD data/Preservation0101 2022 02 21 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
pres01_02<-read.delim('./data/FPOD data/Preservation0102 2022 05 08 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
pres01_03<-read.delim('./data/FPOD data/Preservation0103 2022 07 07 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)


charles01<-charles01_01%>%
  bind_rows(charles01_02)%>%
  bind_rows(charles01_03)%>%
  mutate(Fiord = "CHARLES")

dagg01<-dagg01_01%>%
  bind_rows(dagg01_02)%>%
  bind_rows(dagg01_03)%>%
  bind_rows(dagg01_04)%>%
  bind_rows(dagg01_05)%>%
  mutate(Fiord = "DAGG")

pres01<-pres01_01%>%
  bind_rows(pres01_02)%>%
  bind_rows(pres01_03)%>%
  mutate(Fiord = "PRESERVATION")

all<-charles01%>%
  bind_rows(dagg01)%>%
  bind_rows(pres01)%>%
  mutate(Date = as.Date(dmy_hm(Time)))%>%
  filter(Qn == "High" | Qn == "Mod" | Qn == "Low")

all$Qn<-factor(all$Qn, levels = c("Low","Mod","High"))

all_Cet<-all%>%  
  filter(SpClass == "Other cet")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

nbhf_time<-all%>%
  filter(SpClass == "NBHF")%>%
  arrange(dmy_hm(Time))

nbhf_day<-nbhf_time%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

nbhf_day%>%
  group_by(Date)%>%
  tally()

write.csv(nbhf_time, paste0('./data/Fiordland_nbhf_', Sys.Date(),'.csv'), row.names = F)

acou_timeline<-function(x){
  ggplot(x)+
  geom_col(aes(x = Date, y = 1, fill = Qn))+
  ylim(c(0,1))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  ylab("")+
  xlim(c(min(as.Date(deploy$Datetime_deployment), na.rm = T), max(as.Date(deploy$Datetime_deployment), na.rm = T)))+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom")
}

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment)), linetype = "twodash", color = "red")

ggsave('./figures/allcet.png',all_Cet_plot, dpi = 300, width = 175, height = 125, units = "mm")

all_NBHF_plot<-acou_timeline(nbhf_day)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  geom_vline(deploy%>%filter(Recorder_type == 'F-POD'), mapping = aes(xintercept = as.Date(Datetime_deployment)), linetype = "twodash", color = "red")

ggsave('./figures/NBHF.png', all_NBHF_plot, dpi = 300, width = 175, height = 125, units = "mm")

#soundtrap died shaded areas    
all_Cet_plot+
  geom_rect(data = data.frame(Fiord = "NANCY"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "DAGG"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "CHALKY"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)
  

trunc_nbhf_othercet<-ggplot(all%>%filter(SpClass == "NBHF" | SpClass == "Other cet"))+
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

ggsave('./figures/trunc_nbhf_othercet.png', trunc_nbhf_othercet, dpi = 300, width = 300, height = 80, units = "mm")

locations_FPOD<-deploy%>%filter(Recorder_type == 'F-POD' & !is.na(Recorder_start))

write.csv(locations_FPOD, paste0('./data/Fiordland_FPOD_locations_', Sys.Date(),'.csv'), row.names = F)
