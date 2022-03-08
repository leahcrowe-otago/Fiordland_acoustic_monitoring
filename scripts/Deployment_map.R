library(leaflet);library(leaflet.esri);library(leaflet.extras);library(dplyr);library(rgdal)

protected_areas<-readOGR("./shapefiles", layer = "protected-areas")
CRS.latlon<-CRS("+proj=longlat +datum=WGS84 +no_defs")
protected_areas<-sp::spTransform(protected_areas, CRS.latlon)
natpark<-subset(protected_areas, (section == "s.4 - National Park"))
mpa<-subset(protected_areas, (section == "s.3 - Marine Reserve"))

deployments<-read.csv('./data/Deployment locations.csv', header = T, stringsAsFactors = T)

factpal <- colorFactor(palette = "Dark2", domain = deployments$PROJECT)
leaflet(data = deployments) %>% 
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels=TRUE)%>%
  addPolygons(data = mpa, color = "orange", weight = 1) %>%
  addCircleMarkers(lng = ~deployments$Longitude, lat = ~deployments$Latitude, color = ~factpal(deployments$PROJECT), stroke = FALSE, fillOpacity = 2, radius = 5) %>%
  addLegend(pal = factpal, values = deployments$PROJECT, opacity = 1, title = "Project")%>%
  addLegend(colors = "orange", labels = "Marine Reserves")%>%
  fitBounds(min(deployments$Longitude), min(deployments$Latitude), max(deployments$Longitude), max(deployments$Latitude))
