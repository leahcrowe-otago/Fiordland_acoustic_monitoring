library(leaflet);library(leaflet.esri);library(leaflet.extras);library(dplyr);library(sf);library(readxl)

protected_areas<-sf::read_sf("C:/Users/leahm/OneDrive - University of Otago/Documents/git-otago/Fiordland_reporting/shapefiles", layer = "protected-areas") #https://data.linz.govt.nz/layer/53564-protected-areas/
natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))

deployments<-read_excel('./data/Fiordland deployment locations.xlsx', sheet = "Sheet1")
deployments$Latitude<-round(deployments$Latitude,5)
deployments$Longitude<-round(deployments$Longitude,5)

deployments<-deployments%>%
  filter(!is.na(Datetime_deployment_local))%>%
  filter(is.na(Datetime_retrieval_local))%>%
  group_by(substr(Deployment_number,1, stringr::str_length(Deployment_number)-3))%>%
  as.data.frame()

factpal <- colorFactor(palette = "Dark2", domain = deployments$PROJECT)

deploy_leaf<-leaflet(data = deployments) %>% 
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE)%>%
  addPolygons(data = mpa, color = "orange", weight = 1) %>%
  addCircleMarkers(lng = ~deployments$Longitude, lat = ~deployments$Latitude, color = ~factpal(deployments$PROJECT), stroke = FALSE, fillOpacity = 2, radius = 5,
                   popup = ~paste(sep = "<br/>",Deployment_number,as.Date(Datetime_deployment_local),Recorder_type,Latitude, Longitude)) %>%
  addLegend(pal = factpal, values = deployments$PROJECT, opacity = 1, title = "Project")%>%
  addLegend(colors = "orange", labels = "Marine Reserves")%>%
  fitBounds(min(deployments$Longitude), min(deployments$Latitude), max(deployments$Longitude), max(deployments$Latitude))

library(htmlwidgets)
saveWidget(deploy_leaf, file="index.html")

