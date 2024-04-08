library(odbc);library(dplyr);library(DBI);library(lubridate)

source('C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/scripts/connect to MYSQL.R', local = TRUE, verbose = F)$value

survey_data<-dbReadTable(con, "survey_data")

head(survey_data)
str(survey_data)

survey_data$DATE<-ymd(survey_data$DATE)

noTt<-survey_data%>%
  filter(DATE > ymd('2022-01-31'))%>%
  distinct(DATE,SPECIES)%>%
  mutate(SPECIES = if_else(SPECIES =='', 'NOSPECIES', SPECIES))%>%
  mutate(value = 1)%>%
  tidyr::pivot_wider(names_from = SPECIES, values_from = value)%>%
  filter(is.na(Bottlenose))%>%
  arrange(DATE)%>%
  dplyr::select(DATE)


noTt%>%
  left_join(all_ST_Cet, by = c("DATE" = "Date"))%>%
  as.data.frame()
