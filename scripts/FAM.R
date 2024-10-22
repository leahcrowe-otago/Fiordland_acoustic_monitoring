library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%
  filter(!grepl("FF01",Deployment_number))%>%
  mutate(Fiord = case_when(
    grepl("FF02", Deployment_number) ~ "MARINE-RESERVE-1",
    grepl("FF03", Deployment_number) ~ "MARINE-RESERVE-2",
    TRUE ~ Fiord))%>%
  mutate(Recorder_type = case_when(
    grepl("FPOD", Recorder_type) ~ "F-POD",
    TRUE ~ Recorder_type))%>%
  mutate(Fiord_recorder = paste0(Fiord,"_",Recorder_type))

deploy_tz<-deploy%>%filter(Recorder_type == "ST")%>%dplyr::select(Deployment_number, daylight_adjust)

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
  filter(!(grepl("Preservation0105", File) & Datetime >= ymd_hms("2023-11-02 07:44:00")))%>% #deployment end, cut out time transiting back to me
  filter(!(grepl("Charles0105", File) & Datetime <= ymd_hms("2023-05-13 15:53:00")))%>% #deployment begin, cut out time transiting to site
  mutate(type = "FPOD")%>%
  mutate(Quality = Qn)

all_FPOD_Dol<-all_FPOD%>%  
  filter(SpClass == "Dol")%>%
  filter(Quality != '?' & Quality != "L")%>%
  mutate(Fiord_recorder = paste0(Fiord,"_F-POD"))

hist(all_FPOD_Dol$medianKHz)

all_FPOD_Cet<-all_FPOD_Dol%>%
  group_by(Date, Fiord, Quality, Fiord_recorder)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Quality, Fiord_recorder)

all_FPOD_Cet%>%
  ungroup()%>%
  distinct(Fiord, Date)%>%
  tally()

# ST/FinFinder review ----

ST_data_list<-list.files("./data/ST", pattern = "*.txt", full.names = T, recursive = F)

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
    grepl("FF02", Deployment_number) == TRUE ~ "MARINE-RESERVE-1",
    grepl("FF03", Deployment_number) == TRUE ~ "MARINE-RESERVE-2",
    TRUE ~ Fiord
  ))%>%
  mutate(type = "ST")%>%
  #FPOD is synced with GPS time but with ST time coming from file
  mutate(Datetime = case_when(
    Deployment_number == "Dagg01_01" & type == "ST" ~ Datetime + minutes(4),
    Deployment_number == "Dagg01_02" & type == "ST" ~ Datetime - minutes(3),
    Deployment_number == "Dagg01_03" & type == "ST" ~ Datetime - minutes(0),
    Deployment_number == "Dagg01_04" & type == "ST" ~ Datetime - minutes(0),
    Deployment_number == "Dagg01_05" & type == "ST" ~ Datetime - minutes(0),
    TRUE ~ Datetime
  ))%>%
  mutate(Species = case_when(
    Dolphin...y.n. == "y" & Species == "" & Date == "2023-03-29" ~ "Bottlenose",
    TRUE ~ Species
  ))
#write.csv(all_ST, paste0('./data/all_ST_', Sys.Date(),'.csv'), row.names = F)

ST_dol<-all_ST%>%
  filter(Dolphin...y.n. == "y" | Dolphin...y.n. == "?")%>%
  #filter(Species == "Bottlenose")%>%
  group_by(Date, Fiord)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  mutate(SpClass = "Dol",
         Quality = case_when(
           Dolphin...y.n. == "y" ~ "H",
           Dolphin...y.n. == "?" ~ "?",
         ))%>%
  mutate(Fiord_recorder = paste0(Fiord,"_ST"))

all_ST_Cet<-ST_dol%>%
  distinct(Date, Fiord, DPD, SpClass, Quality, Fiord_recorder)%>%
  filter(Quality != '?')

ST_dol%>%
  filter(Species == "")%>%
  dplyr::select(Species, Dolphin...y.n.)

all_ST%>%
  filter(is.na(Fiord))

# plot function ----

maxdate_plot<-ymd("2023-11-30")

acou_timeline<-function(x){
  ggplot(x)+
    geom_col(aes(x = Date, y = 1), fill = "#33A02C")+
    ylim(c(0,1))+
    theme_bw()+
    ylab("")+
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(angle=45,hjust=1))+
    scale_x_date(date_breaks="1 month", date_labels="%b-%Y")+
    coord_cartesian(xlim = c(ymd("2022-02-15"), maxdate_plot))
}

# all together

all_Cet<-all_FPOD_Cet%>%
  rbind(all_ST_Cet)

unique(all_Cet$Fiord_recorder)

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_F-POD","NANCY_ST","DAGG_F-POD","DAGG_ST","MARINE-RESERVE-1_ST","MARINE-RESERVE-2_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_F-POD")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(deploy%>%group_by(Fiord)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local)), mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")

all_Cet_plot<-all_Cet_plot+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-09-02"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #MR_2 = FF03
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2022-12-31"), xmax = ymd("2023-06-26"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2023-11-29"), xmax = maxdate_plot, ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #MR_1 = FF02
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-1_ST"), aes(xmin = ymd("2022-12-29"), xmax = ymd("2023-02-23"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-1_ST"), aes(xmin = ymd("2023-06-22"), xmax = maxdate_plot, ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2022-11-23"), xmax = ymd("2023-02-23"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-02"), xmax = ymd("2023-06-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-11-25"), xmax = maxdate_plot, ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-10-19"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_F-POD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_F-POD"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHARLES_F-POD"), aes(xmin = ymd("2023-10-21"), xmax = maxdate_plot, ymin = 0, ymax = 1), fill="black", alpha = 0.6, inherit.aes = FALSE)

all_Cet_plot$layers<-c(
  #15/30 below everything else
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-02-15"), xmax = ymd("2022-10-07"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-01-02"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-03-14"), xmax = ymd("2023-06-20"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-02-16"), xmax = ymd("2022-07-13"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-09-02"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-02-21"), xmax = ymd("2022-11-16"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-04-28"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="blue", alpha = 0.1, inherit.aes = FALSE),
  #handbrowse
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-04-30"), ymin = 0, ymax = 1), fill="goldenrod", alpha = 0.4, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-06-21"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="goldenrod", alpha = 0.4, inherit.aes = FALSE),
  all_Cet_plot$layers)

all_Cet_plot
ggsave('./figures/allcet_v1_all.png',all_Cet_plot, dpi = 300, width = 200, height = 220, units = "mm")

#detection rate, S_h

source('./scripts/listening.R', local = TRUE)$value

## all together, daily detection rate
dagg_b<-all_Cet%>%
  filter(Fiord == 'DAGG')%>%
  mutate(Fiord_recorder = "DAGG_BOTH")%>%
  filter(Date < "2023-05-24")
  
daily_det_rate<-all_Cet%>%
  bind_rows(dagg_b)%>%
  ungroup()%>%
  distinct(Fiord_recorder, Date)%>%
  group_by(Fiord_recorder)%>%
  tally()%>%
  left_join(listening, by = "Fiord_recorder")%>%
  mutate(det_pres = round(n/as.numeric(active), 2))%>%
  as.data.frame()%>%
  arrange(-det_pres)

library(webshot2);library(magick)

daily_det_rate_table<-daily_det_rate%>%
  dplyr::select(Fiord_recorder, n, active, det_pres)%>%
  mutate(active = as.numeric(active))%>%
  dplyr::rename(`Acoustic detection days` = "n", `Device active days` = "active", `Acoustic detection rate (daily)` = "det_pres")

saveRDS(daily_det_rate_table, file = paste0("./tables/daily_det_rate_table_all.rds"))

#### capture histories ----
deploy%>%
  group_by(Fiord)%>%
  filter(Datetime_deployment_local == min(Datetime_deployment_local) | Recorder_stop_local == max(Recorder_stop_local))%>%
  dplyr::select(Fiord, Datetime_deployment_local, Recorder_stop_local)%>%
  arrange(Fiord, Datetime_deployment_local)

#charles
charles<-all_Cet%>%filter(Fiord == "CHARLES")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(FPOD = 1)
summary(charles)
charles_dates<-data.frame(date = as.Date(c(ymd("2022-02-15"):ymd("2023-10-20"))))
charles_ch<-charles_dates%>%left_join(charles, by = c("date" = "Date"))%>%
  dplyr::select(date, FPOD)
charles_ch[is.na(charles_ch)] <- 0
nrow(charles_ch)
saveRDS(charles_ch, file = paste0("./data/charles_ch.rds"))

#nancy
nancy<-all_Cet%>%filter(Fiord == "NANCY")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(ST = 1)
summary(nancy)
nancy_dates<-data.frame(date = as.Date(c(ymd("2022-02-16"):ymd("2022-10-06"),ymd("2022-11-28"):ymd("2023-01-01"),ymd("2023-03-15"):ymd("2023-11-19"))))
nancy_ch<-nancy_dates%>%left_join(nancy, by = c("date" = "Date"))%>%
  dplyr::select(date, ST)%>%
  mutate(samp = case_when(
    date >= ymd("2023-06-20") ~ 0,
    TRUE ~ 1
  ),
  analysis = 0)
nancy_ch[is.na(nancy_ch)] <- 0
nrow(nancy_ch)
saveRDS(nancy_ch, file = paste0("./data/nancy_ch.rds"))

#dagg
dagg<-all_Cet%>%filter(Fiord == "DAGG")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  filter(!(Date >= ymd("2022-11-14") & Date <= ymd("2022-11-27")))%>%
  filter(Date < ymd("2023-05-24"))%>%
  mutate(val = 1)%>%
  tidyr::pivot_wider(names_from = Fiord_recorder, values_from = val)
summary(dagg)
dagg_dates<-data.frame(date = as.Date(c(ymd("2022-02-16"):ymd("2022-11-13"),ymd("2022-11-28"):ymd("2023-05-23"))))
dagg_ch<-dagg_dates%>%left_join(dagg, by = c("date" = "Date"))%>%
  mutate(samp = case_when(
    date >= ymd("2022-07-14") & date <= ymd("2022-11-14") ~ 0,
    TRUE ~ 1
  ),
  analysis = case_when(
   date >= ymd("2023-02-20") & date <= ymd("2023-04-30") ~ 1,
   TRUE ~ 0))
dagg_ch[is.na(dagg_ch)] <- 0
nrow(dagg_ch)
saveRDS(dagg_ch, file = paste0("./data/dagg_ch.rds"))

#chalky
chalky<-all_Cet%>%filter(Fiord == "CHALKY")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(ST = 1)
summary(chalky)
chalky_dates<-data.frame(date = as.Date(c(ymd("2022-02-22"):ymd("2022-11-16"),ymd("2023-04-28"):ymd("2023-10-18"))))
chalky_ch<-chalky_dates%>%left_join(chalky, by = c("date" = "Date"))%>%
  dplyr::select(date, ST)%>%
  mutate(samp = 1,
    analysis = case_when(
      date >= ymd("2023-06-21") ~ 1,
      TRUE ~ 0))
chalky_ch[is.na(chalky_ch)] <- 0
nrow(chalky_ch)
saveRDS(chalky_ch, file = paste0("./data/chalky_ch.rds"))

#preservation
pres<-all_Cet%>%filter(Fiord == "PRESERVATION")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(FPOD = 1)
summary(pres)
# last day ended: 2023-11-02 07:44:00
pres_dates<-data.frame(date = as.Date(c(ymd("2022-02-22"):ymd("2023-03-15"),ymd("2023-04-29"):ymd("2023-11-01"))))
pres_ch<-pres_dates%>%left_join(pres, by = c("date" = "Date"))%>%
  dplyr::select(date, FPOD)
pres_ch[is.na(pres_ch)] <- 0
nrow(pres_ch)
saveRDS(pres_ch, file = paste0("./data/pres_ch.rds"))

#anchor
dusky<-all_Cet%>%filter(Fiord == "DUSKY")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(ST = 1)
summary(dusky)
dusky_dates<-data.frame(date = as.Date(c(ymd("2022-02-21"):ymd("2022-11-22"),ymd("2023-02-24"):ymd("2023-06-01"),ymd("2023-06-27"):ymd("2023-11-24"))))
dusky_ch<-dusky_dates%>%left_join(dusky, by = c("date" = "Date"))%>%
  dplyr::select(date, ST)%>%
  mutate(samp = 0)
dusky_ch[is.na(dusky_ch)] <- 0
nrow(dusky_ch)
saveRDS(dusky_ch, file = paste0("./data/dusky_ch.rds"))

#MR-1
mr1<-all_Cet%>%filter(Fiord == "MARINE-RESERVE-1")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(ST = 1)
summary(mr1)
mr1_dates<-data.frame(date = as.Date(c(ymd("2022-02-19"):ymd("2022-12-28"),ymd("2023-02-24"):ymd("2023-06-21"))))
mr1_ch<-mr1_dates%>%left_join(mr1, by = c("date" = "Date"))%>%
  dplyr::select(date, ST)%>%
  mutate(samp = 0)
mr1_ch[is.na(mr1_ch)] <- 0
nrow(mr1_ch)
saveRDS(mr1_ch, file = paste0("./data/mr1_ch.rds"))

#MR-2
mr2<-all_Cet%>%filter(Fiord == "MARINE-RESERVE-2")%>%
  ungroup()%>%
  distinct(Date, Fiord_recorder)%>%
  mutate(ST = 1)
summary(mr2)
mr2_dates<-data.frame(date = as.Date(c(ymd("2022-02-20"):ymd("2022-12-31"),ymd("2023-06-27"):ymd("2023-11-28"))))
mr2_ch<-mr2_dates%>%left_join(mr2, by = c("date" = "Date"))%>%
  dplyr::select(date, ST)%>%
  mutate(samp = 0)
mr2_ch[is.na(mr2_ch)] <- 0
nrow(mr2_ch)
saveRDS(mr2_ch, file = paste0("./data/mr2_ch.rds"))
##
