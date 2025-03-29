library(odbc)
library(dplyr)
library(DBI)
library(lubridate)
library(ggplot2)
library(viridis)
library(eulerr)

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
  filter(DATE <= "2023-11-30")%>%
  distinct(SURVEY_AREA, DATE, ID_NAME)%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  filter(LAST_YEAR >= 2010) # still includes spring 2009

#ind per area
ID_areas<-ind_area%>%
  dplyr::select(SURVEY_AREA, DATE, POD, ID_NAME)

# DOUBTFUL

doubtfuldataquery<-ind_area%>%
  filter(POD == "DOUBTFUL")%>%
  distinct(SURVEY_AREA, ID_NAME, POD)%>%
  group_by(SURVEY_AREA, ID_NAME, POD)%>%
  mutate(count = n())

doubtfulpod<-doubtfuldataquery%>%
  tidyr::spread(SURVEY_AREA, value = count)%>%
  filter(!grepl("D-", ID_NAME)) #exclude animals not seen dead at first sighting

# 
doubtfulpod%>%
  filter(is.na(DAGG))%>%
  filter(is.na(DUSKY))%>%
  as.data.frame()

doubtfulpod_venn<-doubtfulpod%>%
  ungroup()%>%
  dplyr::select(-ID_NAME,-POD)
  
doubtfulpod_venn[is.na(doubtfulpod_venn)] <-FALSE
doubtfulpod_venn

library(wesanderson)
wesanderson::wes_palettes$Zissou1
doubtful_venn<-plot(euler(doubtfulpod_venn, shape = "ellipse"), quantities = list(col = "black"), label = list(col = c("black")), edge = list(col = "black"), fill = c("#3B9AB2", "lightgrey", "#E1AF00", "#F21A00"), alpha = 0.5)
doubtful_venn

ind_area%>%
  filter(POD == "DOUBTFUL" & SURVEY_AREA == "DUSKY")%>%
  group_by(DATE)%>%
  tally()

# DUSKY

duskydataquery<-ind_area%>%
  filter(POD == "DUSKY")%>%
  distinct(SURVEY_AREA, ID_NAME, POD)%>%
  group_by(SURVEY_AREA, ID_NAME, POD)%>%
  mutate(count = n())

ID_areas%>%
  filter(!grepl("D-", ID_NAME))%>%
  distinct(POD,ID_NAME)%>%
  filter(POD == "DUSKY")%>%
  group_by(POD)%>%
  tally()

duskypod<-duskydataquery%>%
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
  dplyr::select(-ID_NAME,-POD)

duskypod_venn[is.na(duskypod_venn)] <-FALSE

dusky_venn<-plot(euler(duskypod_venn, shape = "ellipse"), quantities = list(col = "black"), label = list(col = c("black")), edge = list(col = "black"), fill = c("purple", "lightgrey", "#E1AF00", "#5B1A18"), alpha = 0.5, cex = 0.25)

venn<-ggpubr::ggarrange(doubtful_venn, dusky_venn, labels = "auto")

ggsave('./figures/venn.png', venn, dpi = 700, height = 100, width = 250, units = "mm", bg="white")

