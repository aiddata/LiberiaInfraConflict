setwd("C:/Users/Harsh/Desktop/AidData/libgie")
library(rgdal)
library(maptools)
library(rgeos)
library(sp)
library(devtools)
library(reshape2)
library(doBy)
library(MatchIt)
#define the projection
proj.def<-CRS("+proj=longlat +datum=WGS84")
distInMetersU <- 2000
distInMetersR <- 5000
#read in DHS spatial data
dhs1986.shpfile.location <-"C:/Users/Harsh/Desktop/AidData/libgie/1986 DHS Liberia/lbge03fl Geocoding/LBGE03FL.shp"
dhs.1986 <- readShapePoints(dhs1986.shpfile.location,proj4string = proj.def)
dhs2007.shpfile.location <-"C:/Users/Harsh/Desktop/AidData/libgie/2007 DHS Liberia/lbge52fl Geocoding/LBGE52FL.shp"
dhs.2007 <- readShapePoints(dhs2007.shpfile.location,proj4string = proj.def)
dhs2009.shpfile.location <-"C:/Users/Harsh/Desktop/AidData/libgie/2009 DHS Liberia/lbge5cfl Geocoding/LBGE5CFL.shp"
dhs.2009 <- readShapePoints(dhs2009.shpfile.location,proj4string = proj.def)
dhs2011.shpfile.location <-"C:/Users/Harsh/Desktop/AidData/libgie/2011 DHS Liberia/lbge61fl Geocoding/LBGE61FL.shp"
dhs.2011 <- readShapePoints(dhs2011.shpfile.location,proj4string = proj.def)
dhs2013.shpfile.location <-"C:/Users/Harsh/Desktop/AidData/libgie/2013 DHS LIberia/lbge6afl Geocoding/LBGE6AFL.shp"
dhs.2013 <- readShapePoints(dhs2013.shpfile.location,proj4string = proj.def)
#subset data into urban and rural for each wave of DHS - key to creating 5km buffers around R and 2km buffers around U
dhs.1986.u <-dhs.1986[dhs.1986@data$URBAN_RURA=="U",]
dhs.1986.r <-dhs.1986[dhs.1986@data$URBAN_RURA=="R",]
dhs.2007.u <-dhs.2007[dhs.2007@data$URBAN_RURA=="U",]
dhs.2007.r <-dhs.2007[dhs.2007@data$URBAN_RURA=="R",]
dhs.2009.u <-dhs.2009[dhs.2009@data$URBAN_RURA=="U",]
dhs.2009.r <-dhs.2009[dhs.2009@data$URBAN_RURA=="R",]
dhs.2011.u <-dhs.2011[dhs.2011@data$URBAN_RURA=="U",]
dhs.2011.r <-dhs.2011[dhs.2011@data$URBAN_RURA=="R",]
dhs.2013.u <-dhs.2013[dhs.2013@data$URBAN_RURA=="U",]
dhs.2013.r <-dhs.2013[dhs.2013@data$URBAN_RURA=="R",]
dhs.1986.u <-spTransform(dhs.1986.u,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.1986.u.buffer <-gBuffer(dhs.1986.u,byid = TRUE,width=distInMetersU,capStyle = "ROUND",joinStyle = "ROUND")
dhs.1986.r <-spTransform(dhs.1986.r,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.1986.r.buffer <-gBuffer(dhs.1986.r,byid = TRUE,width=distInMetersR,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2007.u <- spTransform(dhs.2007.u,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2007.u.buffer <-gBuffer(dhs.2007.u,byid = TRUE,width=distInMetersU,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2007.r <- spTransform(dhs.2007.r,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2007.r.buffer <-gBuffer(dhs.2007.r,byid = TRUE,width=distInMetersR,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2009.u <-spTransform(dhs.2009.u,CRS("+proj=merc +zone=29 +datum=WGS84")) 
dhs.2009.u.buffer <-gBuffer(dhs.2009.u,byid = TRUE,width=distInMetersU,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2009.r <-spTransform(dhs.2009.r,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2009.r.buffer <-gBuffer(dhs.2009.r,byid = TRUE,width=distInMetersR,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2011.u <- spTransform(dhs.2011.u,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2011.u.buffer <-gBuffer(dhs.2011.u,byid = TRUE,width=distInMetersU,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2011.r <- spTransform(dhs.2011.r,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2011.r.buffer <-gBuffer(dhs.2011.r,byid = TRUE,width=distInMetersR,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2013.u <- spTransform(dhs.2013.u,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2013.u.buffer <-gBuffer(dhs.2013.u,byid = TRUE,width=distInMetersU,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2013.r <- spTransform(dhs.2013.r,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2013.r.buffer <-gBuffer(dhs.2013.r,byid = TRUE,width=distInMetersR,capStyle = "ROUND",joinStyle = "ROUND")
proj.def2<-CRS("+proj=merc +zone=29 +datum=WGS84")
concessions.shp.location <-"C:/Users/Harsh/Desktop/AidData/libgie/confinal.shp"
concessions <-readShapePoly(concessions.shp.location,proj4string = proj.def)
dhs.1986.buffers <-spRbind(dhs.1986.r.buffer,dhs.1986.u.buffer)
dhs.2007.buffers <-spRbind(dhs.2007.r.buffer,dhs.2007.u.buffer)
dhs.2009.buffers <-spRbind(dhs.2009.r.buffer,dhs.2009.u.buffer)
dhs.2011.buffers <-spRbind(dhs.2011.r.buffer,dhs.2011.u.buffer)
dhs.2013.buffers <-spRbind(dhs.2013.r.buffer,dhs.2013.u.buffer)
dhs.1986 <-spTransform(dhs.1986,CRS("+proj=longlat +datum=WGS84"))
dhs.2007 <-spTransform(dhs.2007,CRS("+proj=longlat +datum=WGS84"))
dhs.2009 <-spTransform(dhs.2009,CRS("+proj=longlat +datum=WGS84"))
dhs.2011 <-spTransform(dhs.2011,CRS("+proj=longlat +datum=WGS84"))
dhs.2013 <-spTransform(dhs.2013,CRS("+proj=longlat +datum=WGS84"))
concessions <-spTransform(concessions,CRS("+proj=longlat +datum=WGS84"))
dhs.1986$within <- !is.na(over(dhs.1986,concessions)) #Dan - this line returns all values as "FALSE" - should I be more explict about defining the projections as equal to each other? 
