
dagg_dp<-read.delim("./data/performance/Dagg01_04_performance_detections_04Jan2023.txt", header = T)%>%
  mutate(Begin.Date.Time = dmy_hm(Begin.Date.Time))%>%
  mutate(hr = hour(Begin.Date.Time))%>%
  filter(!is.na(hr))

#detection positive hours w/ clicks from Jenni's browsing for FPOD comparison
dagg_dp%>%
  filter(Clicks...y.n. == 'y')%>%
  distinct(hr)

## detection positive hours from Jenni's browsing
dagg_dp%>%
  filter(Whistles...y.n. == 'y' | grepl("buzz", Notes))%>%
  distinct(hr)

dagg_dp%>%distinct(hr)

#number of detection hours to validate
all_ST%>%
  filter(Fiord == "DAGG" & Date == "2023-01-04")%>%
  mutate(hr = hour(Datetime))%>%
  distinct(hr)

## number of detection hours with positive detections from the detector!
all_ST%>%
  filter(Fiord == "DAGG" & Date == "2023-01-04")%>%
  mutate(hr = hour(Datetime))%>%
  filter(Dolphin...y.n. == "y")%>%
  distinct(hr)
