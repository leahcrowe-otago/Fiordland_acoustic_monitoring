library(ggplot2);library(lubridate);library(dplyr);library(readxl)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%filter(Fiord != "DUSKY")


pres01_01<-read.delim('./data/FPOD data/Pres01_01 DPM day.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 9, row.names = NULL)
pres01_02<-read.delim('./data/FPOD data/Pres01_02 DPM day.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 9, row.names = NULL)
charles01_01<-read.delim('./data/FPOD data/Charles01_01 DPM day.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 10, row.names = NULL)
charles01_02<-read.delim('./data/FPOD data/Charles01_02 DPM day.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 9, row.names = NULL)
dagg01_01<-read.delim('./data/FPOD data/Dagg01_01 DPM day.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 10, row.names = NULL)
dagg01_02<-read.delim('./data/FPOD data/Dagg01_02 DPM day.txt', header = T, sep ="\t", quote = "\"",dec = ".", skip = 10, row.names = NULL)

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
  mutate(Date = as.Date(ChunkEnd))%>%
  group_by(Date, Fiord)%>%
  mutate(DPM = sum(DPM))%>%
  mutate(presence = case_when(
    DPM > 0 ~ 1,
    DPM == 0 ~ 0
  ))

ggplot(all)+
  geom_col(aes(x = Date, y = presence, fill = Fiord))+
  facet_wrap(~Fiord, ncol = 1)+
  ylim(c(0,1))+
  theme_bw()+
  theme(legend.position = "none")+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(deploy$Datetime_deployment)))
