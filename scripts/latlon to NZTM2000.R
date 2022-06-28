library(dplyr);library(rgdal);library(mapview)

deployments<-read.csv('./data/Deployment locations.csv', header = T, stringsAsFactors = T)
deployments$Latitude<-round(deployments$Latitude,5)
deployments$Longitude<-round(deployments$Longitude,5)

CRS.latlon<-CRS("+proj=longlat +datum=WGS84 +no_defs")
CRS.nztm2000<-CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m")

coordinates(deployments)<- ~Longitude+Latitude
proj4string(deployments)<-CRS.latlon

deployments.nztm2000<-sp::spTransform(deployments,CRS.nztm2000)
mapview(deployments.nztm2000)
deployments_nztm_df<-deployments.nztm2000%>%as.data.frame()%>%dplyr::select(Deployment.,Longitude,Latitude)%>%arrange(-Latitude)%>%distinct()
