library(sp)
library(terra)
rm(list = ls())
setwd('C:/Dropbox/Applied-Economics/task10')

#Load Swedish municipal borders
data=vect("jl_riks/jl_riks.shp")

#What type of object is it? 
str(data)
data
#CRS: SWEREF99 TM (EPSG:3006)

#Plot
plot(data)

#Load Swedish railway lines
data2=vect("KommunRT90/Kommun_RT90_region.shp")

#What type of object is it? 
str(data2)
data2
#CRS: RT90 2.5 gon V (EPSG:3021)


#Plot the railways together with the municipal borders
new.crs<-crs(data2)
data.newproj<-project(data,new.crs)
plot(data.newproj)
plot(data2,add=TRUE)
