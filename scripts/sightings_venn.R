library(odbc);library(dplyr);library(DBI);library(lubridate);library(ggplot2);library(viridis);library(eulerr)

source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_data<-dbReadTable(con, "photo_analysis")%>%filter(ID_NAME != "CULL" & ID_NAME != "UNMA")
head(photo_data)
head(lifehist)

photo_data%>%
  group_by(SURVEY_AREA)%>%
  filter(DATE == min(DATE))%>%
  distinct(SURVEY_AREA, DATE)

ind_area<-photo_data%>%
  distinct(SURVEY_AREA, DATE, ID_NAME)%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  filter(LAST_YEAR >= 2010) # still includes spring 2009

# DOUBTFUL
doubtfulpod<-ind_area%>%
  filter(POD == "DOUBTFUL")%>%
  distinct(SURVEY_AREA, ID_NAME)%>%
  group_by(SURVEY_AREA, ID_NAME)%>%
  mutate(count = n())%>%
  tidyr::spread(SURVEY_AREA, value = count)%>%
  filter(!grepl("D-", ID_NAME)) #exclude animals not seen dead at first sighting

# 
doubtfulpod%>%
  filter(is.na(DAGG))%>%
  filter(is.na(DUSKY))%>%
  as.data.frame()

doubtfulpod_venn<-doubtfulpod%>%
  ungroup()%>%
  dplyr::select(-ID_NAME)
  
doubtfulpod_venn[is.na(doubtfulpod_venn)] <-FALSE

plot(euler(doubtfulpod_venn, shape = "ellipse"), quantities = TRUE)


ind_area%>%
  filter(POD == "DOUBTFUL" & SURVEY_AREA == "DUSKY")%>%
  group_by(DATE)%>%
  tally()

# DUSKY
duskypod<-ind_area%>%
  filter(POD == "DUSKY")%>%
  distinct(SURVEY_AREA, ID_NAME)%>%
  group_by(SURVEY_AREA, ID_NAME)%>%
  mutate(count = n())%>%
  tidyr::spread(SURVEY_AREA, value = count)%>%
  filter(!grepl("D-", ID_NAME)) #exclude animals not seen dead at first sighting

# 
duskypod%>%
  filter(is.na(BLUFF))%>%
  filter(is.na(DOUBTFUL))%>%
  filter(is.na(PRESERVATION))%>%
  as.data.frame()

duskypod_venn<-duskypod%>%
  ungroup()%>%
  dplyr::select(-ID_NAME)

duskypod_venn[is.na(duskypod_venn)] <-FALSE

plot(euler(duskypod_venn, shape = "ellipse"), quantities = TRUE)

## sightings table

sigs_nf<-read_excel("./data/SIGHTINGS_NF.xlsx")%>%
  arrange(Year, Date)
head(sigs_nf)

color_fiord<-paletteer::paletteer_d("vapoRwave::hotlineBling")[1:length(unique(sigs_nf$Fiord))]

sigs_nf$Fiord<-cell_spec(sigs_nf$Fiord, bold = T, 
             color = factor(sigs_nf$Fiord, c(unique(sigs_nf$Fiord)), c(rep("white",6),"black")),
             background = factor(sigs_nf$Fiord, c(unique(sigs_nf$Fiord)), c(color_fiord)))

kbl(sigs_nf%>%dplyr::select(-Notes), escape = F, align = "c") %>%
  kable_classic("striped", full_width = F)%>%
  as_image()

png