library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%
  filter(!grepl("FF01",Deployment_number))%>%
  filter(!grepl("FF02",Deployment_number))%>%
  mutate(Fiord = case_when(
    grepl("FF0", Deployment_number) ~ "FIVE_FINGERS",
    TRUE ~ Fiord))%>%
  mutate(Fiord_recorder = paste0(Fiord,"_",Recorder_type))

deploy_tz<-deploy%>%filter(Recorder_type == "ST")%>%dplyr::select(Deployment_number, daylight_adjust)

deploy_tz%>%filter(grepl("Dagg", Deployment_number))%>%filter(daylight_adjust == 1)
# FPOD data ----

FPOD_data_list<-list.files("./data/FPOD", pattern = "*train details.csv", full.names = T, recursive = T)

FPOD_data<-lapply(FPOD_data_list, function(x)
  read.csv(x)
)

all_FPOD<-bind_rows(FPOD_data)%>%
  mutate(Fiord = toupper(stringr::str_extract(File, '^[^01]+')))%>%
  mutate(Datetime = ymd_hm("1899-12-30 00:00") + minutes(Min),
         Date = as.Date(Datetime))%>%
  filter(!(grepl("Charles0104", File) & Datetime >= ymd_hms("2023-05-13 15:44:00")))%>%
  filter(!(grepl("Preservation0105", File) & Datetime >= ymd_hms("2023-11-02 07:44:00")))

all_FPOD_Dol<-all_FPOD%>%  
  filter(SpClass == "Dol")

all_FPOD_Cet<-all_FPOD_Dol%>%
  group_by(Date, Fiord, Qn)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Qn)%>%
  mutate(Fiord_recorder = paste0(Fiord,"_FPOD"))

# FinFinder review ----

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
  mutate(Datetime = case_when( #normalize time to first deployment -- FPOD time has been adjusted in the software
    daylight_adjust == 1 ~ Datetime + hours(1),
    daylight_adjust == 0 ~ Datetime
  ))%>%
  mutate(Fiord = case_when(
    Fiord == "ANCHOR" ~ "DUSKY",
    Fiord == "FF" ~ "FIVE_FINGERS",
    TRUE ~ Fiord
  ))

#write.csv(all_ST, paste0('./data/all_ST_', Sys.Date(),'.csv'), row.names = F)


all_ST_Cet<-all_ST%>%
  filter(Dolphin...y.n. == "y" | Dolphin...y.n. == "?")%>%
  filter(Species == "Bottlenose")%>%
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


all_ST%>%
  filter(Dolphin...y.n. == "")

all_ST%>%
  filter(is.na(Fiord))

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
    #xlim(c(min(as.Date(deploy$Datetime_deployment_local), na.rm = T), max(as.Date(deploy$Datetime_deployment_local), na.rm = T)))+
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(angle=45,hjust=1))+
    scale_x_date(date_breaks="1 month", date_labels="%b-%Y", limits = c(min(as.Date(deploy$Datetime_deployment_local), na.rm = T), max(as.Date(deploy$Datetime_retrieval_local), na.rm = T)))
}

# all together

all_Cet<-all_FPOD_Cet%>%
  rbind(all_ST_Cet)

unique(all_Cet$Fiord_recorder)

all_Cet$Qn<-factor(all_Cet$Qn, levels = c("?","L","M","H"))

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_FPOD","NANCY_ST","DAGG_FPOD","DAGG_ST","FIVE_FINGERS_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_FPOD")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(deploy%>%group_by(Fiord)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local)), mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")

all_Cet_plot<-all_Cet_plot+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-09-02"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "FIVE_FINGERS_ST"), aes(xmin = ymd("2022-12-31"), xmax = ymd("2023-06-26"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2022-07-06"), xmax = ymd("2023-02-23"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-10-19"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_FPOD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_FPOD"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #to be analysed
  geom_rect(data = data.frame(Fiord_recorder = "CHARLES_FPOD"), aes(xmin = ymd("2023-05-13"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-06-20"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-04-30"), xmax = ymd("2023-09-02"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "FIVE_FINGERS_ST"), aes(xmin = ymd("2023-06-26"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-27"), xmax = ymd("2023-11-20"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-06-21"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)

all_Cet_plot

ggsave('./figures/allcet_v1.png',all_Cet_plot, dpi = 300, width = 175, height = 175, units = "mm")

all_Cet_plot+
  xlim(c(ymd("2022-05-01"), ymd("2023-11-15")))

###

nbhf_day$Qn<-factor(nbhf_day$Qn, levels = c("L","M","H"))

all_FPOD_NBHF_plot<-acou_timeline(nbhf_day)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  geom_vline(deploy%>%filter(Recorder_type == 'FPOD'), mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(deploy%>%filter(Recorder_type == 'FPOD')%>%group_by(Fiord)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local)), mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")+
  geom_rect(data = data.frame(Fiord = "DAGG"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord = "PRESERVATION"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)

all_FPOD_NBHF_plot

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

#all things detected on FPOD
acou_timeline(all_FPOD)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","DAGG","PRESERVATION")), ncol = 1)+
  facet_wrap(~factor(Fiord, levels = c("CHARLES","NANCY","DAGG","DUSKY","CHALKY","PRESERVATION")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")

# days listening
listening<-deploy%>%
  ungroup()%>%
  group_by(Fiord, Recorder_type)%>%
  dplyr::summarise(min_date = as.Date(min(Datetime_deployment_local, na.rm= T)), max_date = as.Date(max(Datetime_retrieval_local, na.rm = T)))%>%
  ungroup()%>%
  # mutate(max_date = case_when(
  #   Fiord == "DUSKY" ~ as.Date("2022-07-06"),
  #   TRUE ~ max_date
  # ))%>%
  mutate(days = max_date - min_date)%>%
  ungroup()%>%
  mutate(dead = case_when(
    Fiord == "NANCY" ~ (ymd("2022-11-27") - ymd("2022-10-07")) + (ymd("2023-03-14") - ymd("2023-01-02")),
    Fiord == "DAGG" & Recorder_type == "ST" ~ (ymd("2022-11-27") - ymd("2022-11-15")) + (ymd("2023-11-09") - ymd("2023-09-02")), 
    Fiord == "DAGG" & Recorder_type == "FPOD" ~ (ymd("2023-11-09") - ymd("2023-05-24")),
    Fiord == "CHALKY" ~ (ymd("2023-04-28") - ymd("2022-11-16")) + (ymd("2023-11-09") - ymd("2023-10-19")),
    Fiord == "PRESERVATION" ~ (ymd("2023-04-28") - ymd("2023-03-15")),
    #no deployment FF03_03
    Fiord == "FIVE_FINGERS" ~ (ymd("2023-06-23") - ymd("2022-12-31")),
    #change when Dusky analysed
    Fiord == "DUSKY" ~ (ymd("2023-06-23") - ymd("2022-07-06")),
    TRUE ~ (ymd("2023-08-09") - ymd("2023-08-09"))
  ))%>%
  mutate(active = days - dead)

## all together
all_Cet%>%
  ungroup()%>%
  distinct(Fiord, Date)%>%
  group_by(Fiord)%>%
  tally()%>%
  left_join(listening, by = "Fiord")%>%
  mutate(det_pres = n/as.numeric(active))%>%
  as.data.frame()%>%
  arrange(det_pres)

## seasonally? I don't know if I really want to go down this route

season<-all_Cet%>%
  ungroup()%>%
  distinct(Fiord, Fiord_recorder, Date)%>%
  mutate(season = case_when(
    month(Date) == 12 | month(Date) <= 2 ~ "summer",
    month(Date) >= 3 & month(Date) <= 5 ~ "autumn",
    month(Date) >= 6 & month(Date) <= 8 ~ "winter",
    month(Date) >= 9 & month(Date) <= 11 ~ "spring"
  ))%>%
  group_by(Fiord, season)%>%
  tally()%>%
  left_join(listening, by = "Fiord")%>%
  mutate(det_pres = n/as.numeric(active))%>%
  as.data.frame()

ggplot(season)+
  geom_col(aes(x = season, y = det_pres, fill = season),color = "black", position = "stack", alpha = 0.7)+
  facet_wrap(~paste0(Fiord,"-",Recorder_type))

## Dagg ----

head(all_FPOD_Dol)

FPOD_DAGG<-all_FPOD_Dol%>%
  filter(Fiord == "DAGG")%>%
  dplyr::select(Date,Datetime,Qn,File)%>%
  mutate(type = "FPOD")%>%
  mutate(Deployment_number = substr(File,1,8))%>%
  dplyr::select(-File)

head(FPOD_DAGG)

ST_DAGG<-all_ST%>%
  filter(Fiord == "DAGG")%>%
  filter(Dolphin...y.n. == "y" | Dolphin...y.n. == "?")%>%
  mutate(Qn = case_when(
    Dolphin...y.n. == "y" ~ "H",
    Dolphin...y.n. == "?" ~ "?",
  ))%>%
  dplyr::select(Date,Datetime,Qn,Deployment_number, Begin.Path)%>%
  filter(Qn != "?")%>%
  mutate(type = "ST")%>%
  mutate(Deployment_number = gsub("_", "", Deployment_number))%>%
  mutate(begin_time = ymd_hms(paste0("20",substr(Begin.Path, 24, nchar(Begin.Path) - 4))))

head(ST_DAGG)

DAGG<-FPOD_DAGG%>%
  bind_rows(ST_DAGG)%>%
  arrange(Datetime)

nrow(DAGG)
head(DAGG)

########
DAGG%>%
  filter(Date <= "2023-02-20")%>%
  filter(Date < "2022-11-15" | Date > "2022-11-27")%>%
  distinct(Date, type)%>%
  group_by(type)%>%
  tally()

tally_detection<-DAGG %>%
  distinct(Date, type)%>%
  group_by(Date)%>%
  tally()

tally_detection%>%
  filter(Date <= "2023-02-20")%>%
  filter(Date < "2022-11-15" | Date > "2022-11-27")%>%
  group_by(n)%>%
  tally()

23/(78)

one_gear_detection<-tally_detection%>%
  filter(n != 2)%>%
  left_join(DAGG, by ='Date')

FPOD_only<-one_gear_detection%>%
  filter(type == "FPOD")

ST_only<-one_gear_detection%>%
  filter(type == "ST")

#write.csv(one_gear_detection, paste0('./data/one_gear_detection_', Sys.Date(),'.csv'), row.names = F)

# binning into 15 min ----

##need list of all files, not just those with detections

Dagg_file_list<-list.files("./data/Leah_file_list", pattern = "*.csv", full.names = T, recursive = T)

Dagg_ST_files<-lapply(Dagg_file_list, function(x)
  read.csv(x, header = T)
)

Dagg_ST_files<-bind_rows(Dagg_ST_files)%>%
  arrange(begin_time)%>%
  mutate(end_time = ymd_hms(begin_time) + minutes(15),
         Deployment_number = gsub("_","",substr(Begin.Path, 4, 12)),
         bin_num = 1:n())%>%
  mutate(beg_time_FPOD = ymd_hms(begin_time) - minutes(1),
         end_time_FPOD = ymd_hms(end_time) + minutes(1))

head(Dagg_ST_files)
summary(Dagg_ST_files)

head(ST_DAGG)

ST_DAGG_bin<-ST_DAGG%>%
  mutate(Begin.Path = gsub("\\\\","/",Begin.Path))%>%
  left_join(Dagg_ST_files, by = c("Begin.Path", "Deployment_number"))

head(FPOD_DAGG)
library(fuzzyjoin)
print(Sys.time())
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

head(ST_DAGG_bin)

ST_DAGG_bin<-ST_DAGG_bin%>%
  dplyr::rename("begin_time" = "begin_time.x")%>%
  dplyr::select(-begin_time.y)

head(ST_DAGG_bin)

bins<-ST_DAGG_bin%>%
  bind_rows(FPOD_DAGG_bin)%>%
  distinct(Deployment_number, bin_num, type)%>%
  group_by(Deployment_number, bin_num)%>%
  tally()
  
nrow(bins)
bins%>%filter(n == 1)
bins%>%filter(n == 2)


ST_bin%>%
  distinct(type, bin)

FPOD_bin%>%
  distinct(type, bin)

# ST deployments with ?
all_ST%>%
  filter(Dolphin...y.n. == "?")%>%
  distinct(Deployment_number)
