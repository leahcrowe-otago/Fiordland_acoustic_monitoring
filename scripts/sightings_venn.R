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
  dplyr::select(-ID_NAME)
  
doubtfulpod_venn[is.na(doubtfulpod_venn)] <-FALSE

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
  dplyr::select(-ID_NAME)

duskypod_venn[is.na(duskypod_venn)] <-FALSE

dusky_venn<-plot(euler(duskypod_venn, shape = "ellipse"), quantities = list(col = "black"), label = list(col = c("black")), edge = list(col = "black"), fill = c("purple", "lightgrey", "#E1AF00", "#5B1A18"), alpha = 0.5, cex = 0.25)

venn<-ggpubr::ggarrange(doubtful_venn, dusky_venn, labels = "auto")

ggsave('./figures/venn.png', venn, dpi = 700, height = 100, width = 250, units = "mm", bg="white")

## map

fiord_group<-doubtfuldataquery%>%
  bind_rows(duskydataquery)%>%
  tidyr::spread(SURVEY_AREA, value = SURVEY_AREA)%>%
  mutate(fiord_group = paste(NANCY, DOUBTFUL, DAGG, DUSKY, PRESERVATION, BLUFF))

lag_fiord_group<-doubtfuldataquery%>%
  bind_rows(duskydataquery)%>%
  arrange(ID_NAME, factor(SURVEY_AREA, levels = c("NANCY","DOUBTFUL","DAGG","DUSKY","PRESERVATION","BLUFF")))%>%
  group_by(ID_NAME)%>%
  mutate(lag_end = lead(SURVEY_AREA))%>%
  mutate(total_a = sum(count))%>%
  mutate(lag_end = case_when(
    is.na(lag_end) & total_a > 2 ~ dplyr::first(SURVEY_AREA),
    TRUE ~ lag_end
  ))

fiord_pos<-doubtfuldataquery%>%
  bind_rows(duskydataquery)%>%
  mutate(Lat = case_when(
    SURVEY_AREA == "NANCY" ~ -45.150,
    SURVEY_AREA == "DOUBTFUL" ~ -45.300,
    SURVEY_AREA == "DAGG" ~ -45.414,
    SURVEY_AREA == "DUSKY" ~ -45.674,
    SURVEY_AREA == "PRESERVATION" ~ -46.081,
    SURVEY_AREA == "BLUFF" ~ -46.597
  ))%>%
  mutate(Lon = case_when(
    SURVEY_AREA == "NANCY" ~ 167.078,
    SURVEY_AREA == "DOUBTFUL" ~ 167.010,
    SURVEY_AREA == "DAGG" ~ 166.863,
    SURVEY_AREA == "DUSKY" ~ 166.729,
    SURVEY_AREA == "PRESERVATION" ~ 166.683,
    SURVEY_AREA == "BLUFF" ~ 168.330
  ))%>%
  left_join(fiord_group, by = c("ID_NAME","POD"))%>%
  filter(fiord_group != "NA DOUBTFUL NA NA NA NA")%>%
  filter(fiord_group != "NA NA NA DUSKY NA NA")%>%
  left_join(lag_fiord_group, by = c("ID_NAME","POD","SURVEY_AREA"))%>%
  mutate(lag_Lat = case_when(
    lag_end == "NANCY" ~ -45.150,
    lag_end == "DOUBTFUL" ~ -45.300,
    lag_end == "DAGG" ~ -45.414,
    lag_end == "DUSKY" ~ -45.674,
    lag_end == "PRESERVATION" ~ -46.081,
    lag_end == "BLUFF" ~ -46.597
  ))%>%
  mutate(lag_Lon = case_when(
    lag_end == "NANCY" ~ 167.078,
    lag_end == "DOUBTFUL" ~ 167.010,
    lag_end == "DAGG" ~ 166.863,
    lag_end == "DUSKY" ~ 166.729,
    lag_end == "PRESERVATION" ~ 166.683,
    lag_end == "BLUFF" ~ 168.330
  ))%>%
  mutate(POD = case_when(
    POD == "DOUBTFUL" ~ "Doubtful pod",
    TRUE ~ "Dusky pod"
  ))


library(ggalt);library(ggforce)

fiord_pos$fiord_group<-as.factor(fiord_pos$fiord_group)

ggplot()+
  geom_sf(data = NZ_coast, alpha = 0.9, fill = "white", lwd = 0.1)+
  coord_sf(xlim = c(166.3,168.4), ylim = c(-45,-46.6), crs = 4269)+
  #coord_sf(xlim = c(166.6,167.2), ylim = c(-45.1,-45.7), crs = 4269)+
  geom_mark_circle(fiord_pos, mapping = aes(x = Lon, y = Lat, fill = fiord_group, color = fiord_group), position = "jitter", expand=0.02, linewidth = 0.5)+
  #geom_curve(fiord_pos%>%filter(!is.na(lag_end)), mapping = aes(x = Lon, y = Lat, xend = lag_Lon, yend = lag_Lat, color = fiord_group, group = ID_NAME), position=position_dodge(width=0.2))+
  #geom_line(fiord_pos, mapping = aes(x = Lon, y = Lat, color = fiord_group, group = ID_NAME), position=position_dodge(width=0.2))+
  geom_point(fiord_pos, mapping = aes(x = Lon, y = Lat, shape = SURVEY_AREA), fill = "red", color = "red", size = 5)+
  scale_fill_viridis(discrete = T)+
  scale_color_viridis(discrete = T)+
  scale_shape_manual(values = c(7,21:25))+
  facet_wrap(~POD)

ggsave('./figures/venn_map.png', dpi = 700, height = 150, width = 300, units = "mm", bg="white")
