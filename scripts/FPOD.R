library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%filter(Fiord != "DUSKY")

charles01_01<-read.delim('./data/FPOD data/Charles0101 2022 02 15 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
charles01_02<-read.delim('./data/FPOD data/Charles0102 2022 05 06 FPOD_6192 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

dagg01_01<-read.delim('./data/FPOD data/Dagg0101 2022 02 16 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
dagg01_02<-read.delim('./data/FPOD data/Dagg0102 2022 05 07 FPOD_6194 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

pres01_01<-read.delim('./data/FPOD data/Preservation0101 2022 02 21 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)
pres01_02<-read.delim('./data/FPOD data/Preservation0102 2022 05 08 FPOD_6190 file0 train details.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 40, row.names = NULL)

charles01<-charles01_01%>%
  bind_rows(charles01_02)%>%
  mutate(Fiord = "CHARLES")

dagg01<-dagg01_01%>%
  bind_rows(dagg01_02)%>%
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

all_Tt<-all%>%  
  filter(SpClass == "Other cet")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

ggplot(all_Tt)+
  geom_col(aes(x = Date, y = 1, fill = Qn))+
  facet_wrap(~factor(Fiord, , levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  ylim(c(0,1))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment)))+
  ylab("")

all_NBHF<-all%>%
  filter(SpClass == "NBHF")%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)

ggplot(all_NBHF)+
  geom_col(aes(x = Date, y = 1, fill = Qn))+
  facet_wrap(~factor(Fiord, , levels = c("CHARLES","NANCY","DAGG","CHALKY","PRESERVATION")), ncol = 1)+
  ylim(c(0,1))+
  theme_bw()+
  scale_fill_brewer(palette = "Paired")+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment)))+
  ylab("")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
