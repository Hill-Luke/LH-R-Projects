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
library(sqldf)

#Reading in the crime data
crime_all<-read.csv("/Users/lukeofthehill/Desktop/R/Data/Chicago Crime Edu/Crimes_-_2001_to_present.csv", header=TRUE) 


#Reading in boundary data
boundaries<-geojson_read("/Users/lukeofthehill/Desktop/R/Data/Chicago Crime Edu/boundaries.geojson", what="sp")

plot(boundaries)
?geojson_read()
#Plotting basic boundaries of Chicago
plot(boundaries)

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

head(crime_all)

crime_sub<-select(crime_all, c(ID, Date, Primary.Type, Description, Arrest, District, Ward, Community.Area, Year, Longitude, Latitude))
head(crime_sub)

names(boundary_data_names)
is.integer(boundary_data_names$id)
boundary_data_names$Community.Area<-as.integer(boundary_data_names$id)

crime_full<-left_join(crime_sub, select(boundary_data_names, c("community", "Community.Area")), by="Community.Area")
head(crime_full)


setwd("/Users/lukeofthehill/Desktop/R/Data/Chicago Crime Edu/")

write.csv(file="crime_full.csv", crime_full)

table(crime_full$Primary.Type)
crime_major<-subset(crime_full, Primary.Type=="ASSAULT" |Primary.Type=="WEAPONS VIOLATION" |Primary.Type=="OTHER NARCOTIC VIOLATION" |Primary.Type=="BATTERY" | Primary.Type=="HOMICIDE" |Primary.Type=="NARCOTICS"|Primary.Type=="ROBBERY"|Primary.Type=="THEFT")


write.csv(file="crime_major.csv", crime_major)


?save()


crimes_plot<-ggplot(data=crime_major, aes(x=count(Primary.Type), y=Year, color=Primary.Type))+geom_line(stat=identity)

ggplotly(crimes_plot)
names(crime_major)

crime_major$crime_type<-crime_major$Primary.Type
crime_major$community_area<-crime_major$Community.Area
?sqldf()
crime_summary<-sqldf("SELECT COUNT(crime_type) as num_crimes, community_area, crime_type, Year
                    FROM crime_major
                    GROUP BY community_area, crime_type, Year;")
names(boundary_data_names)





crime_summary_2016<-subset(crime_summary, Year==2016)

boundary_table<-select(boundary_data_names, c("community", "area_numbe"))



crime_summary_full2<-sqldf("SELECT  A.*, b.crime_type, B.num_crimes, b.Year
                          FROM
                          boundary_table AS A
                          LEFT JOIN crime_summary AS B
                          ON A.area_numbe=B.community_area;")

crime_summary_full<-sqldf("SELECT  A.*, b.crime_type, B.num_crimes as num_crimes_2013,c.num_crimes as num_crimes_2014,
                        d.num_crimes as num_crimes_2015, e.num_crimes as num_crimes_2016
                          FROM
                          boundary_table AS A
                          LEFT JOIN crime_summary_2013 AS B
                          ON A.area_numbe=B.community_area
                          LEFT JOIN crime_summary_2014 AS C
                          ON A.area_numbe=C.community_area and b.crime_type=c.crime_type
                          LEFT JOIN crime_summary_2015 AS D
                          ON A.area_numbe=D.community_area and b.crime_type=d.crime_type
                          LEFT JOIN crime_summary_2016 AS e 
                          ON A.area_numbe=e.community_area and b.crime_type=e.crime_type;")
crime_summary_full<-NULL 


write.csv(file="crime_summary_final_project.csv", crime_summary_full2)
