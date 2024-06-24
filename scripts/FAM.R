library(ggplot2);library(lubridate);library(dplyr);library(readxl);library(viridis)

deploy<-read_excel("./data/Fiordland deployment locations.xlsx")
deploy<-deploy%>%
  filter(!grepl("FF01",Deployment_number))%>%
  mutate(Fiord = case_when(
    grepl("FF02", Deployment_number) ~ "MARINE-RESERVE-1",
    grepl("FF03", Deployment_number) ~ "MARINE-RESERVE-2",
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
  filter(!(grepl("Preservation0105", File) & Datetime >= ymd_hms("2023-11-02 07:44:00")))%>% #deployment end, cut out time transiting back to me
  filter(!(grepl("Charles0105", File) & Datetime <= ymd_hms("2023-05-13 15:53:00")))%>% #deployment begin, cut out time transiting to site
  mutate(type = "FPOD")%>%
  mutate(Quality = Qn)

all_FPOD_Dol<-all_FPOD%>%  
  filter(SpClass == "Dol")

all_FPOD_Cet<-all_FPOD_Dol%>%
  group_by(Date, Fiord, Quality)%>%
  mutate(DPD = n())%>% #DPD = detections per day
  distinct(Date, Fiord, SpClass, DPD, Quality)%>%
  mutate(Fiord_recorder = paste0(Fiord,"_FPOD"))%>%
  filter(Quality != '?' & Quality != "L")

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

# plot function ----

acou_timeline<-function(x){
  ggplot(x)+
    geom_col(aes(x = Date, y = 1, fill = Quality))+
    ylim(c(0,1))+
    theme_bw()+
    scale_fill_manual(values = c("#B2DF8A","#33A02C"))+
    ylab("")+
    #xlim(c(min(as.Date(deploy$Datetime_deployment_local), na.rm = T), max(as.Date(deploy$Datetime_deployment_local), na.rm = T)))+
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          axis.text.x=element_text(angle=45,hjust=1))+
          #strip.background = element_rect(aes(fill = Fiord), colour = "black", size = 1))+
    scale_x_date(date_breaks="1 month", date_labels="%b-%Y")+
    coord_cartesian(xlim = c(ymd("2022-02-15"), ymd("2023-11-20")))
}

# all together

all_Cet<-all_FPOD_Cet%>%
  rbind(all_ST_Cet)

unique(all_Cet$Fiord_recorder)

#all_Cet$Quality<-factor(all_Cet$Quality, levels = c("?","L","M","H"))
all_Cet$Quality<-factor(all_Cet$Quality, levels = c("M","H"))

all_Cet_plot<-acou_timeline(all_Cet)+
  facet_wrap(~factor(Fiord_recorder, levels = c("CHARLES_FPOD","NANCY_ST","DAGG_FPOD","DAGG_ST","MARINE-RESERVE-1_ST","MARINE-RESERVE-2_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_FPOD")), ncol = 1)+
  geom_vline(deploy, mapping = aes(xintercept = as.Date(Datetime_deployment_local)), linetype = "twodash", color = "red")+
  geom_vline(deploy%>%group_by(Fiord)%>%filter(Datetime_retrieval_local == max(Datetime_retrieval_local)), mapping = aes(xintercept = as.Date(Datetime_retrieval_local)), linetype = "twodash", color = "red")

all_Cet_plot<-all_Cet_plot+
  #soundtrap died shaded areas 
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-10-07"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-01-02"), xmax = ymd("2023-03-14"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-09-02"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #MR_2 = FF03
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2022-12-31"), xmax = ymd("2023-06-26"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2023-11-29"), xmax = ymd("2024-02-16"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #MR_1 = FF02
  geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-1_ST"), aes(xmin = ymd("2022-12-29"), xmax = ymd("2023-02-23"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2022-11-23"), xmax = ymd("2023-02-23"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-02"), xmax = ymd("2023-06-27"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-11-25"), xmax = ymd("2024-02-14"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-11-16"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-10-19"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #FPOD died
  geom_rect(data = data.frame(Fiord_recorder = "PRESERVATION_FPOD"), aes(xmin = ymd("2023-03-15"), xmax = ymd("2023-04-28"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_FPOD"), aes(xmin = ymd("2023-05-24"), xmax = ymd("2023-11-09"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHARLES_FPOD"), aes(xmin = ymd("2023-10-21"), xmax = ymd("2024-01-25"), ymin = 0, ymax = 1), fill="black", alpha = 0.5, inherit.aes = FALSE)+
  #to be analysed
  #geom_rect(data = data.frame(Fiord_recorder = "MARINE-RESERVE-2_ST"), aes(xmin = ymd("2023-06-26"), xmax = ymd("2024-02-16"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  #geom_rect(data = data.frame(Fiord_recorder = "DUSKY_ST"), aes(xmin = ymd("2023-06-27"), xmax = ymd("2024-02-14"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-08-14"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="yellow", alpha = 0.5, inherit.aes = FALSE)

  
all_Cet_plot$layers<-c(
  #15/30 below everything else
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-02-15"), xmax = ymd("2022-10-07"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-01-02"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "NANCY_ST"), aes(xmin = ymd("2023-03-14"), xmax = ymd("2023-06-20"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-02-16"), xmax = ymd("2022-07-13"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-09-02"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2022-02-21"), xmax = ymd("2022-11-16"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-04-28"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE),
  #handbrowse
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-04-30"), ymin = 0, ymax = 1), fill="blue", alpha = 0.2, inherit.aes = FALSE), 
  geom_rect(data = data.frame(Fiord_recorder = "CHALKY_ST"), aes(xmin = ymd("2023-06-21"), xmax = ymd("2023-10-19"), ymin = 0, ymax = 1), fill="blue", alpha = 0.2, inherit.aes = FALSE),
  all_Cet_plot$layers)

all_Cet_plot
ggsave('./figures/allcet_v1.png',all_Cet_plot, dpi = 300, width = 175, height = 175, units = "mm")

# days listening
listening<-deploy%>%
  ungroup()%>%
  mutate(Fiord_recorder = paste0(Fiord,"_",Recorder_type))%>%
  group_by(Fiord_recorder)%>%
  dplyr::summarise(min_date = as.Date(min(Datetime_deployment_local, na.rm= T)), max_date = as.Date(max(Datetime_retrieval_local, na.rm = T)))%>%
  ungroup()%>%
  mutate(days = max_date - min_date)%>%
  ungroup()%>%
  mutate(dead = case_when(
    Fiord_recorder == "CHARLES_FPOD" ~ (ymd("2024-01-25") - ymd("2023-10-21")),
    Fiord_recorder == "NANCY_ST" ~ (ymd("2022-11-27") - ymd("2022-10-07")) + (ymd("2023-03-14") - ymd("2023-01-02")),
    Fiord_recorder == "DAGG_FPOD" ~ (ymd("2023-11-09") - ymd("2023-05-24")),
    Fiord_recorder == "DAGG_ST" ~ (ymd("2022-11-27") - ymd("2022-11-15")) + (ymd("2023-11-09") - ymd("2023-09-02")), 
    # MR_1 = FF02
    Fiord_recorder == "MARINE-RESERVE-1_ST" ~ (ymd("2023-02-23") - ymd("2022-12-29")),
    #no deployment FF03_03
    Fiord_recorder == "MARINE-RESERVE-2_ST" ~ (ymd("2024-02-16") - ymd("2023-11-29")) + (ymd("2023-06-26") - ymd("2022-12-31")),
    #change when Dusky analysed
    Fiord_recorder == "DUSKY_ST" ~ (ymd("2023-02-23") - ymd("2022-11-23")) + (ymd("2023-06-27") - ymd("2023-06-02")) + (ymd("2024-02-14") - ymd("2023-11-25")),
    Fiord_recorder == "CHALKY_ST" ~ (ymd("2023-04-28") - ymd("2022-11-16")) + (ymd("2023-11-09") - ymd("2023-10-19")),
    Fiord_recorder == "PRESERVATION_FPOD" ~ (ymd("2023-04-28") - ymd("2023-03-15")),
    TRUE ~ (ymd("2023-08-09") - ymd("2023-08-09"))
  ))%>%
  mutate(active = days - dead)

## all together, daily detection rate
dagg_both<-all_Cet%>%
  filter(Fiord == 'DAGG')%>%
  mutate(Fiord_recorder = "DAGG_BOTH")
  
daily_det_rate<-all_Cet%>%
  bind_rows(dagg_both)%>%
  ungroup()%>%
  distinct(Fiord_recorder, Date)%>%
  group_by(Fiord_recorder)%>%
  tally()%>%
  left_join(listening, by = "Fiord_recorder")%>%
  mutate(min_date = tidyr::replace_na(min_date, ymd("2022-02-16")),
         max_date = tidyr::replace_na(max_date, ymd("2023-11-09")),
         days = tidyr::replace_na(days, ymd("2023-11-09") - ymd("2022-02-16")),
         dead = tidyr::replace_na(dead, ymd("2023-11-09") - ymd("2023-09-02")),
         active = days - dead)%>%
  mutate(det_pres = round(n/as.numeric(active), 2))%>%
  as.data.frame()%>%
  arrange(-det_pres)

library(webshot2);library(magick)

daily_det_rate_table<-daily_det_rate%>%
  dplyr::select(Fiord_recorder, n, active, det_pres)%>%
  mutate(active = as.numeric(active))%>%
  dplyr::rename(`Acoustic detection days` = "n", `Recorder active days` = "active", `Detection rate (daily)` = "det_pres")

saveRDS(daily_det_rate_table, file = paste0("./tables/daily_det_rate_table.rds"))

#NEED TO MANUALLY SAVE, CAN'T GET THE SAVE_IMAGE COMMAND TO WORK

# Dagg and/or Nancy

Nancy_Dagg<-all_Cet%>%
  filter(Fiord == "NANCY" | Fiord == "DAGG")%>%
  ungroup()%>%
  distinct(Date)

num_Nancy_Dagg_Feb<-Nancy_Dagg%>%
  filter(Date >= "2022-02-16" & Date < "2023-02-16")%>%
  tally()

num_Nancy_Dagg_Feb/365

num_Nancy_Dagg_Sep<-Nancy_Dagg%>%
  filter(Date <= "2023-09-02" & Date > "2022-09-02")%>%
  tally()

num_Nancy_Dagg_Sep/365

num_Nancy_Dagg_Nov<-Nancy_Dagg%>%
  filter(Date <= "2023-11-20" & Date > "2022-11-20")%>%
  tally()

num_Nancy_Dagg_Nov/365

# Dagg ----

head(all_FPOD_Dol)

FPOD_DAGG<-all_FPOD_Dol%>%
  filter(Fiord == "DAGG")%>%
  dplyr::select(Date,Datetime,Quality,File)%>%
  mutate(Deployment_number = substr(File,1,8))%>%
  mutate(Deployment_number = paste0(substr(Deployment_number,1,6),"_",substr(Deployment_number,7,8)))%>%
  dplyr::select(-File)%>%
  mutate(type = "FPOD")%>%
  filter(Quality == "H" | Quality == "M")

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
  #mutate(Deployment_number = gsub("_", "", Deployment_number))%>%
  mutate(begin_time = ymd_hms(paste0("20",substr(Begin.Path, 24, nchar(Begin.Path) - 4))))%>%
  mutate(type = "ST")%>%
  mutate(begin_time = case_when(
    Deployment_number == "Dagg0101" & type == "ST" ~ begin_time + minutes(4),
    TRUE ~ begin_time
  ))

head(ST_DAGG)

DAGG<-FPOD_DAGG%>%
  bind_rows(ST_DAGG)%>%
  arrange(Datetime)%>%
  left_join(deploy, by = c("Deployment_number","type" = "Recorder_type"))%>%
  dplyr::select("Date" = "Date.x", Datetime, Quality, Deployment_number, type, Recorder_dur_day)

nrow(DAGG)
head(DAGG)

# Daily differences in DAGG ----

# 15 min ST sampling

#remove continuous and remove handbrowse
sampling_15ST<-DAGG%>%
  filter(Deployment_number != "Dagg01_03" & Deployment_number != "Dagg01_05")%>%
  distinct(Date, type, Deployment_number, Recorder_dur_day)%>%
  filter(Date < "2023-05-24")%>%
  #keep detection on 2022-05-07 for Dagg01_01 only
  filter(!(Date == "2022-05-07" & Deployment_number == "Dagg01_02"))%>%
  group_by(Date)%>%
  mutate(`Number of recorders` = n())
 
total_compare_days<-sampling_15ST%>%
  ungroup()%>%
  distinct(Deployment_number, day_whole = round(Recorder_dur_day,0))%>%
  group_by(Deployment_number)%>%
  filter(day_whole == min(day_whole))%>%
  ungroup()%>%
  mutate(tot_dur = sum(day_whole))

sampling<-sampling_15ST%>%
  distinct(Date, `Number of recorders`)%>%
  group_by(`Number of recorders`)%>%
  tally()%>%
  mutate(`Detection days` = n)%>%
  distinct(`Number of recorders`, `Detection days`)%>%
  ungroup()%>%
  mutate(`Percent of detection days` = round(`Detection days`/sum(`Detection days`) * 100, 0))%>%
  arrange(-`Number of recorders`)%>%
  mutate(`Listening days` = unique(total_compare_days$tot_dur))%>%
  dplyr::select(`Listening days`, everything())

sampling$`Listening days`[2]<-""

sampling%>%
  kableExtra::kable(booktabs = T, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  add_header_above(c("15/30min ST & continuous FPOD" = 4))

capture_15ST<-sampling_15ST%>%
  dplyr::select(Date, type, `Number of recorders`)%>%
  ungroup()%>%
  tidyr::pivot_wider(names_from = type, values_from = `Number of recorders`)
  

capture_15ST[capture_15ST == 2] <- 1
capture_15ST[is.na(capture_15ST)] <- 0

capture_15ST<-capture_15ST%>%
  mutate(type = "samp",
         cum = 1: n(),
         FPOD_cum = cumsum(FPOD),
         ST_cum = cumsum(ST))

dagg_summ<-DAGG%>%
  filter(Date < "2022-11-15" | Date > "2022-11-27")%>%
  filter(Date < "2023-05-24")%>%
  #keep detection on 2022-05-07 for Dagg01_01 only
  filter(!(Date == "2022-05-07" & Deployment_number == "Dagg01_02"))%>%
  ungroup()%>%
  distinct(Date, type, Deployment_number, Recorder_dur_day)%>%
  group_by(Date)%>%
  mutate(`Number of recorders` = n())%>%
  dplyr::select(Date, type, `Number of recorders`)%>%
  ungroup()%>%
  tidyr::pivot_wider(names_from = type, values_from = `Number of recorders`)

dagg_summ[dagg_summ == 2] <- 1
dagg_summ[is.na(dagg_summ)] <- 0

dagg_summ_perc<-dagg_summ%>%
  mutate(type = "samp",
         cum = 1: n(),
         FPOD_cum = cumsum(FPOD),
         ST_cum = cumsum(ST))%>%
  mutate(days_listening = as.numeric(Date - Date[1])+1)%>%
  mutate(FPOD_perc = FPOD_cum/days_listening,
         ST_perc = ST_cum/days_listening,
         all = cum/days_listening)%>%
  mutate(perc_diff=FPOD_perc-ST_perc)
  # mutate(type = case_when(
  #   Date >= ymd("2023-02-20") & Date <= ymd("2023-04-30") ~ "samp + browse",
  #   TRUE ~ type))

summary(dagg_summ_perc)

dagg_plot<-ggplot(dagg_summ_perc)+
  geom_line(aes(x = Date, y = all, color = "Both (FPOD+ST)"), size = 1)+
  geom_line(aes(x = Date, y = FPOD_perc, color = "FPOD"), size = 1)+
  geom_errorbar(aes(ymin=FPOD_perc-0.04, ymax=FPOD_perc, x = Date))+
  geom_line(aes(x = Date, y = ST_perc, color = "ST"), size = 1)+
  geom_vline(xintercept = ymd("2022-03-01"))+ # march 1st, autumn
  geom_vline(xintercept = ymd("2022-06-01"))+ # june 1st, winter
  geom_vline(xintercept = ymd("2022-09-01"))+ #sep 1st, spring
  geom_vline(xintercept = ymd("2022-12-01"))+ #dec 1st, summer
  geom_vline(xintercept = ymd("2023-03-01"))+ #mar 1st
  geom_vline(xintercept = ymd("2023-06-01"))+ #june 1
  geom_vline(xintercept = ymd("2023-09-01"))+
  geom_hline(yintercept = 0.21, linetype = "dashed")+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-15"), xmax = ymd("2022-11-27"), ymin = 0.13, ymax = 0.2), fill="white", alpha = 1, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-02-16"), xmax = ymd("2022-07-13"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE)+ 
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-05-24"), ymin = 0, ymax = 1), fill="red", alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-04-30"), ymin = 0, ymax = 1), fill="blue", alpha = 0.2, inherit.aes = FALSE)+
  theme_bw()+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", date_minor_breaks="1 month")+
  ylab("Running proportion of days with acoustic detections")+
  annotate("text",label = c("autumn","winter","spring","summer","autumn"), 
           x = c(ymd("2022-04-15"),ymd("2022-07-15"),ymd("2022-10-15"),ymd("2023-01-15"),ymd("2023-04-15")), y = 1.02)+
  theme(legend.position = c(0.88,0.82),
        legend.title = element_text( size=8), 
        legend.text=element_text(size=8))+
  scale_color_brewer("Recorder type", palette = "Dark2")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
dagg_plot
hist(dagg_summ_perc$all)

ggsave('./figures/dagg_plot.png',dagg_plot, dpi = 300, width = 220, height = 150, units = "mm")

treatment<-dagg_summ_perc%>%
  dplyr::select(Date, perc_diff)%>%
  mutate(ST_approach = case_when(
    Date >= ymd("2023-02-20") & Date <= ymd("2023-04-30") ~ "samp_browse",
    Date <= ymd("2022-07-13") | Date > ymd("2022-11-27") ~ "samp",
    TRUE ~ "cont"
  ))%>%
  mutate(season = case_when(
    month(Date) <= 2 | month(Date) == 12 ~ "summer",
    month(Date) >= 3 & month(Date) <= 5 ~ "autumn",
    month(Date) >= 6 & month(Date) <= 8 ~ "winter",
    month(Date) >= 9 & month(Date) <= 11 ~ "spring"
    
  ))

ggplot(treatment)+
  geom_point(aes(x = Date, y = perc_diff))+
  geom_smooth(aes(x = Date, y = perc_diff), method = "lm")+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-02-16"), xmax = ymd("2022-07-13"), ymin = 0, ymax = 0.1), fill="red", alpha = 0.2, inherit.aes = FALSE)+ 
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2022-11-27"), xmax = ymd("2023-05-24"), ymin = 0, ymax = 0.1), fill="red", alpha = 0.2, inherit.aes = FALSE)+
  geom_rect(data = data.frame(Fiord_recorder = "DAGG_ST"), aes(xmin = ymd("2023-02-20"), xmax = ymd("2023-04-30"), ymin = 0, ymax = 0.1), fill="blue", alpha = 0.2, inherit.aes = FALSE)

#library(rstanarm)
#rstanarm::stan_lmer(perc_diff ~ ST_approach * season + (1|ST_approach), data = treatment)

# continuous ST sampling

sampling_cont<-DAGG%>%
  filter(Deployment_number == "Dagg01_03" & Datetime <= ymd_hms("2022-11-14 22:30:24"))%>%
  distinct(Date, type, Deployment_number, Recorder_dur_day)%>%
  group_by(Date)%>%
  mutate(`Number of recorders` = n())%>%
  group_by(`Number of recorders`)

cont<-sampling_cont%>%
  mutate(`Detection days` = n(),
         `Listening days` = round(min(Recorder_dur_day), 0))%>%
  distinct(`Number of recorders`, `Detection days`, `Listening days`)%>%
  ungroup()%>%
  mutate(`Percent of detection days` = round(`Detection days`/sum(`Detection days`) * 100, 1))%>%
  arrange(-`Number of recorders`)%>%
  dplyr::select(`Listening days`, everything())

cont$`Listening days`[2]<-""

cont%>%
  kableExtra::kable(booktabs = T, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria")%>%
  add_header_above(c("continuous ST & FPOD" = 4))

capture_cont<-sampling_cont%>%
  dplyr::select(Date, type, `Number of recorders`)%>%
  ungroup()%>%
  tidyr::pivot_wider(names_from = type, values_from = `Number of recorders`)


capture_cont[capture_cont == 2] <- 1
capture_cont[is.na(capture_cont)] <- 0

capture_cont<-capture_cont%>%
  mutate(type = "cont",
         cum = 1:n(),
         FPOD_cum = cumsum(FPOD),
         ST_cum = cumsum(ST))%>%
  mutate(days_listening = as.numeric(Date - Date[1])+1)%>%
  mutate(FPOD_perc = FPOD_cum/days_listening,
         ST_perc = ST_cum/days_listening)

cap<-capture_15ST%>%
  bind_rows(capture_cont)%>%
  arrange(Date)%>%
  mutate(FPOD_totcum = cumsum(FPOD),
         ST_totcum = cumsum(ST),
         tot_totcum = 1:n())

cap%>%
  dplyr::select(Date, FPOD, ST)%>%
  tidyr::pivot_wider(Date)

#### capture histories ----
sampling_dates<-data.frame(date = as.Date(c(ymd("2022-02-16"):ymd("2022-07-12"),ymd("2022-11-28"):ymd("2023-02-19"))))
cont_dates<-data.frame(date = as.Date(c(ymd("2022-07-14"):ymd("2022-11-12"))))

samp_ch<-sampling_dates%>%left_join(cap, by = c("date" = "Date"))%>%
  dplyr::select(date, FPOD, ST)%>%
  mutate(samp = 1)

samp_ch[is.na(samp_ch)] <- 0
saveRDS(samp_ch, file = paste0("./data/Dagg_samp_ch.rds"))

cont_ch<-cont_dates%>%left_join(capture_cont, by = c("date" = "Date"))%>%
  dplyr::select(date, FPOD, ST)%>%
  mutate(samp = 0)

cont_ch[is.na(cont_ch)] <- 0
saveRDS(cont_ch, file = paste0("./data/Dagg_cont_ch.rds"))

ch<-samp_ch%>%
  bind_rows(cont_ch)%>%
  arrange(date)
saveRDS(ch, file = paste0("./data/ch.rds"))

#####

ggplot(cap)+
  geom_point(mapping = aes(x = cum, y = FPOD_cum, color = "FPOD"))+
  geom_smooth(mapping = aes(x = cum, y = FPOD_cum, color = "FPOD"), method="lm")+
  geom_point(mapping = aes(x = cum, y = ST_cum, color = "ST"))+
  geom_smooth(mapping = aes(x = cum, y = ST_cum, color = "ST"), method="lm")+
  facet_wrap(~type)+
  theme_bw()

summary(lm(FPOD_cum ~ cum + FPOD + ST + cum:FPOD:ST, data = cap%>%filter(type == "samp")))

#  geom_path(cap%>%filter(type == "cont"), mapping = aes(x = Date, y = ST_cum), color = "black", linetype = "dashed")+
#  geom_path(cap%>%filter(type == "cont"), mapping = aes(x = Date, y = FPOD_cum), color = "black", linetype = "dashed")+

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
  filter(Datetime <= ymd_hms("2023-05-24 09:10:00"))%>%
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
