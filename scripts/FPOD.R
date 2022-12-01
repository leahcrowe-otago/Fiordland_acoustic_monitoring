library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%filter(Fiord != "DUSKY")%>%
  filter(Datetime_deployment < "2022-12-01 00:00:00")

charles01_01<-read.delim('./data/FPOD data/Charles0101 2022 02 15 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
charles01_02<-read.delim('./data/FPOD data/Charles0102 2022 05 06 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
charles01_03<-read.delim('./data/FPOD data/Charles0103 2022 07 13 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

dagg01_01<-read.delim('./data/FPOD data/Dagg0101 2022 02 16 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_02<-read.delim('./data/FPOD data/Dagg0102 2022 05 07 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_03<-read.delim('./data/FPOD data/Dagg0103 2022 07 13 FPOD_6193 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

pres01_01<-read.delim('./data/FPOD data/Preservation0101 2022 02 21 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
pres01_02<-read.delim('./data/FPOD data/Preservation0102 2022 05 08 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

charles01<-charles01_01%>%
  bind_rows(charles01_02)%>%
  bind_rows(charles01_03)%>%
  mutate(Fiord = "CHARLES")

dagg01<-dagg01_01%>%
  bind_rows(dagg01_02)%>%
  bind_rows(dagg01_03)%>%
  mutate(Fiord = "DAGG")

pres01<-pres01_01%>%
  bind_rows(pres01_02)%>%
  mutate(Fiord = "PRESERVATION")

all<-charles01%>%
  bind_rows(dagg01)%>%
  bind_rows(pres01)%>%
  mutate(Date = as.Date(dmy_hm(Time)))%>%
  filter(Qn == "High" | Qn == "Mod")

all$Qn<-factor(all$Qn, levels = c("Mod","High"))

all_Cet<-all%>%  
  filter(SpClass == "Other cet")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

all_NBHF<-all%>%
  filter(SpClass == "NBHF")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)


acou_timeline<-function(x){
  ggplot(x)+
  geom_col(aes(x = Date, y = 1, fill = Qn))+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  ylim(c(0,1))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment)), linetype = "twodash", color = "red")+
  ylab("")+
  xlim(c(min(as.Date(deploy$Datetime_deployment), na.rm = T), max(as.Date(deploy$Datetime_deployment), na.rm = T)))+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())
}

all_Cet_plot<-acou_timeline(all_Cet)

ggsave('./figures/allcet.png',all_Cet_plot, dpi = 300, width = 175, height = 125, units = "mm")
acou_timeline(all_NBHF)

nbhf<-all%>%
  filter(SpClass == "NBHF")%>%
  arrange(dmy_hm(Time))
  as.data.frame()
  
all_Cet_plot+
  geom_rect(data = data.frame(Fiord = "NANCY"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "DAGG"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="grey", alpha = 0.5, inherit.aes = FALSE)
  