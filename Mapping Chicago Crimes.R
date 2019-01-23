#Plotting crime in Chicago 2012
install.packages("ggmap")
install.packages("geojsonio")
install.packages("rgdal")
install.packages("broom")
library(broom)
library(rgdal)
library(geojsonio)
library(ggplot2)
library(ggmap)
library(readxl)
library(sp)
library(maptools)
library(plotly)
library(dplyr)


#Reading in the crime data
crime_all<-read.csv("/Users/lukeofthehill/Desktop/R/Data/Chicago Crime Edu/crimes_2011_now.csv", header=TRUE) 
crime_2012<-subset(crime_all, Year=="2012")

#Reading in boundary data
boundaries<-geojson_read("/Users/lukeofthehill/Desktop/R/Data/Chicago Crime Edu/boundaries.geojson", what="sp")

plot(boundaries)
?geojson_read()
#Plotting basic boundaries of Chicago
plot(boundaries)

#Downloading map from google maps
mapImage <- 
print(mapImage)

#plotting boundaries on maps
#Transforming the JSON data to get the coordinates to plot
boundary_data<-tidy(boundaries)

#transforming the json data to get the community names
boundary_data_names <-as.data.frame(boundaries)
table(boundary_data_names$area_num_1)
table(boundary_data$id)

is.character(boundary_data$id)
boundary_data_names$id<-c(0:76) %>% as.character()
#Joining the community name to the coordinate data
boundary_data_full<-left_join(boundary_data, select(boundary_data_names, c("community", "id")), by="id")



#Testing
print(mapImage + geom_polygon(aes(long, lat, group=id, color=id), fill=NA, data = boundary_data))

#Turning the map into a ggplot object



p<-ggmap(get_googlemap(c(lon = -87.643433, lat = 41.853196), scale = 1, 
                       zoom = 10, key="AIzaSyBb050eQTzkN-MoHNiG6Fs8RvD36WFt_6A"), extent = "normal") +
                      geom_polygon(aes(long, lat, color=community), fill=NA, data = boundary_data_full)
ggplotly(p)


#Subsetting the crime data to violent crimes
names(crime_2012)
table(crime_2012$Primary.Type)
homicides<-subset(crime_2012, Primary.Type=="HOMICIDE")

#adding homicides to the map
Killings<-ggmap(get_googlemap(c(lon = -87.643433, lat = 41.853196), scale = 1, 
                       zoom = 10, key="AIzaSyBb050eQTzkN-MoHNiG6Fs8RvD36WFt_6A"), extent = "normal") +
  geom_polygon(aes(long, lat, color=community), fill=NA, data = boundary_data_full)+
    geom_point(aes(Longitude, Latitude), data=homicides)
ggplotly(Killings)

#Testing upload to GITHUB
