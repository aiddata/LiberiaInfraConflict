library(sp)
library(maptools)
library(rgdal)
library(rgeos)

P4S.latlon <- CRS("+proj=longlat +datum=WGS84")

rural_dhs <- readShapePoints("/home/dan/Desktop/GitRepo/Liberia/DHS_Buffers/rural_DHS_2013.shp", proj4string=P4S.latlon)
urban_dhs <- readShapePoints("/home/dan/Desktop/GitRepo/Liberia/DHS_Buffers/urban_DHS_2013.shp",proj4string=P4S.latlon)

urban_dhs_2 = urban_dhs[urban_dhs$URBAN_RURA == "U",]

#Transform both into the UTM for Liberia
rural_DHS_reproject <- spTransform(rural_dhs, CRS("+init=epsg:32229"))
urban_DHS_reproject <- spTransform(urban_dhs_2, CRS("+init=epsg:32229"))

plot(gBuffer(rural_DHS_reproject, width=10000, byid=TRUE), lwd=2)
plot(gBuffer(urban_DHS_reproject, width=2000, byid=TRUE), lwd=2)

rural_buffers <- gBuffer(rural_DHS_reproject, width=10000, byid=TRUE)
urban_buffers <- gBuffer(urban_DHS_reproject, width=2000, byid=TRUE)

rural_WGS <- spTransform(rural_buffers, CRS("+init=epsg:4326"))
urban_WGS <- spTransform(urban_buffers, CRS("+init=epsg:4326"))



writePolyShape(rural_WGS, "/home/dan/Desktop/GitRepo/Liberia/DHS_Buffers/rural_buffer.shp")
writePolyShape(urban_WGS, "/home/dan/Desktop/GitRepo/Liberia/DHS_Buffers/urban_buffer.shp")
