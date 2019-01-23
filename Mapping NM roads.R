# Author WF835334 
install.packages('maptools')
install.packages('ggmap')
install.packages('rgdal')
library(maptools)
library(ggmap)
library(rgdal)
setwd("/Users/lukeofthehill/Desktop/R/Data/Shape")


# using the following tutorial: https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf

data<-"/Users/lukeofthehill/Desktop/R/Data/Shape"

NM_road <- readOGR(dsn = data, layer = "Trans_RoadSegment3")
NM_trail <- readOGR(dsn = data, layer = "Trans_TrailSegment")

head(NM)

#Plotting the data
plot(NM_trail)
plot(NM_road)


##Reading in the elevation data from https://viewer.nationalmap.gov/basic/#productSearch

print(gdalDrivers())  
# double check the path, it may be different on your computer
dem<-readGDAL("topography/dem/w001001.adf")
print(summary(dem))
# plot does not work well for SpatialGridDataFrame
# this step is slow, be patient...
p<-spplot(dem)  
print(p)

