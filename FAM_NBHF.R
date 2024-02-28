##was originally in FAM.R



# NBHF ----

#add in UNx as potential NBHF

unique(all_FPOD$SpClass)

nbhf_time<-all_FPOD%>%
  filter(SpClass == "NBHF")%>%
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
write.csv(nbhf_time, paste0('./data/Fiordland_nbhf', Sys.Date(),'_v1.csv'), row.names = F)


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
