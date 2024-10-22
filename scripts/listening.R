#charles ----
char<-deploy%>%filter(Fiord == "CHARLES")%>%arrange(Date)
#total period
char_f<-as.Date(char$Recorder_stop_local[5]) - #2023-10-21 10:25:00, not full day and no detections
  as.Date(char$Datetime_deployment_local[1]) # 2022-02-15, yes detections
  
#nancy ----
nan<-deploy%>%filter(Fiord == "NANCY")%>%arrange(Date)
#total period
nan_st<-as.Date(nan$Recorder_stop_local[6]) - #2023-11-20 18:10:00, not full day and no detections
  as.Date(nan$Datetime_deployment_local[1]) - 1 - #"2022-02-15 16:00:00, not full day and no detections  
  #when st not active, add one to include count of days
  (as.Date(nan$Datetime_deployment_local[4]) - #2022-11-27 20:48:29, not full day and no detections
     as.Date(nan$Recorder_stop_local[3]) + 1) - #2022-10-07 20:48:29, not full day and no detections
  #when st not active, add one to include count of days
  (as.Date(nan$Datetime_deployment_local[5]) - #2023-03-14 00:00:01, deployment day, no detections, exclude this day
     as.Date(nan$Recorder_stop_local[4]) + 1) #2023-01-02 19:37:47, not full day and no detections

# dagg ----
dagg<-deploy%>%filter(Fiord == "DAGG")%>%arrange(Date)
fpod<-dagg%>%filter(Recorder_type == 'F-POD')
st<-dagg%>%filter(Recorder_type == 'ST')

#total period for both going
dagg_both<-as.Date(fpod$Recorder_stop_local[6])- #2023-05-24 09:10:00, not full day and no detections
  as.Date(dagg$Datetime_deployment_local[1]) - # 2022-02-16 13:39:00, yes st and fpod detections
  #when st not active, add one to include count of days
(as.Date(st$Datetime_deployment_local[4]) - #2022-11-27 16:06:00, not full day and no detections
   as.Date(st$Recorder_stop_local[3]) + 1) # 2022-11-14 22:30:24, not full day and no detections

dagg_f<-as.Date(fpod$Recorder_stop_local[6]) - # 2023-05-24 09:10:00, not full day and no detections
  as.Date(dagg$Datetime_deployment_local[1]) # 2022-02-16 13:39:00, yes st and fpod detections

dagg_st<-as.Date(st$Recorder_stop_local[6]) - # 2023-09-02 00:41:11, not full day and no detections
  as.Date(dagg$Datetime_deployment_local[1]) - # 2022-02-16 13:39:00, yes st and fpod detections
  #when st not active, add one to include count of days
  (as.Date(st$Recorder_start_local[4]) - # 2022-11-27 15:45:10, not full day and no detections
     as.Date(st$Recorder_stop_local[3]) + 1) #2022-11-14 22:30:24,  not full day and no detections

#mr1 ----
mr1_d<-deploy%>%filter(Fiord == "MARINE-RESERVE-1")%>%arrange(Date)
#total period
mr1_st<-as.Date(mr1_d$Recorder_stop_local[3])- #2023-06-22 19:06:00, not full day and no detections
  as.Date(mr1_d$Datetime_deployment_local[1] + 1)- #2022-02-19, yes detection
  #when st not active, add one to include count of days
  (as.Date(mr1_d$Datetime_deployment_local[3])- #2023-02-23 16:27:00, not full day and no detections
     as.Date(mr1_d$Recorder_stop_local[2]) + 1) #2022-12-29 00:55:02, not full day and no detections

#mr2 ----
mr2_d<-deploy%>%filter(Fiord == "MARINE-RESERVE-2")%>%arrange(Date)
#total period
mr2_st<-as.Date(mr2_d$Recorder_stop_local[3])- #2023-11-29 04:02:00, not full day and no detections
  as.Date(mr2_d$Datetime_deployment_local[1] + 1)- #2022-02-20, yes detection  
  #when st not active
  (as.Date(mr2_d$Datetime_deployment_local[3])- #2023-06-26 09:52:00, not full day and no detections
     as.Date(mr2_d$Recorder_stop_local[2])) #2022-12-31 20:15:00, yes detection, don't add 1

#dusky ----
du<-deploy%>%filter(Fiord == "DUSKY")%>%arrange(Date)
#total period
du_st<-as.Date(du$Recorder_stop_local[4])- #2023-11-25, not full day and no detections
  as.Date(du$Datetime_deployment_local[1]) - 1 -  #2022-02-20, not full day and no detections
  #when st not active, add one to include count of days
  (as.Date(du$Datetime_deployment_local[3])- #2023-02-23 17:51:00, not full day and no detections
     as.Date(du$Recorder_stop_local[2]) + 1)- #2022-11-23, not full day and no detections
  (as.Date(du$Datetime_deployment_local[4])- #2023-06-27 14:08:00, yes detections, don't add 1
     as.Date(du$Recorder_stop_local[3])) #2023-06-02 05:47:42, not full day and no detections
#chalky ----
chalk<-deploy%>%filter(Fiord == "CHALKY")%>%arrange(Date)
#total period
chalk_st<-as.Date(chalk$Recorder_stop_local[5])- #2023-10-19 22:00:00, not full day and no detections
  as.Date(chalk$Datetime_deployment_local[1]) - 1 - #2022-02-21 14:22:00, not full day and no detections
  #when st not active
  (as.Date(chalk$Datetime_deployment_local[4])- #2023-04-28 16:35:00, yes detections, don't add 1
     as.Date(chalk$Recorder_stop_local[3]) - 1) #2022-11-16 13:10:53, yes detection again, so minus 1

# pres ----
pres<-deploy%>%filter(Fiord == "PRESERVATION")%>%arrange(Date)
#total period
pres_f<-as.Date(pres$Datetime_retrieval_local[5])- # 2023-11-02, not full day and no detections
  as.Date(pres$Datetime_deployment_local[1]) - 1- # 2022-02-21 16:05:00, not full day and no detections
  #not active
  (as.Date(pres$Datetime_deployment_local[4])- #2023-04-28 12:13:35, no detections
     as.Date(pres$Recorder_stop_local[3])) #2023-03-15 02:33:00, yes detections, don't add 1

# total ----
listening<-data.frame(Fiord_recorder = c("CHARLES_F-POD","NANCY_ST","DAGG_F-POD","DAGG_ST","DAGG_BOTH", "MARINE-RESERVE-1_ST","MARINE-RESERVE-2_ST","DUSKY_ST","CHALKY_ST","PRESERVATION_F-POD"),
           active = c(char_f, #charles
                         nan_st, #nancy,
                         dagg_f, #df,
                         dagg_st, #dst,
                         dagg_both,
                         mr1_st, #mr1
                         mr2_st, #mr2,
                         du_st, #du
                         chalk_st, #ch
                         pres_f))
