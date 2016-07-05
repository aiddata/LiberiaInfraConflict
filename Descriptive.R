#Liberia Descriptive Stats Examples for Harsh

#Library that lets you load shapefiles
library(maptools)

#Library that lets you manipulate Shapefiles
library(sp)

#Library that lets you handle projections
library(rgdal)

#Define the projection ("Coordinate Reference System")
proj.def <- CRS("+proj=longlat +datum=WGS84")

#The location of the DHS Shapefile Boundary with the Buffers
DHS.shpfile.location <- "/home/aiddata/Desktop/Github/LiberiaInfraConflict/Analysis_Data/all_buffers_merged.shp" 

#Load in the buffers, and project them.  You always have to manually project in R.
DHS.locs <- readShapePoly(DHS.shpfile.location,proj4string=proj.def)

#The path to the boundary of liberia for visualizaitons.
Lib.bound.loc <- "/home/aiddata/Desktop/Github/LiberiaInfraConflict/LiberiaBoundaries/LBR_adm2.shp"

#Load & project                
Lib.bound <- readShapePoly(Lib.bound.loc,proj4string=proj.def)

#Where the CSV with ancillary data is located (Seth's exports)
anc.data.loc <- "/home/aiddata/Desktop/Github/LiberiaInfraConflict/Analysis_Data/liberia_data.csv"

#Load a CSV.  You can type View(anc.dta) to see what loaded (note capital V)
anc.dta <- read.csv(anc.data.loc)

#You can also type names(anc.dta) to see the column names, or head(anc.dta)
#to see a quick summary

#I'm merging the ancillary data into the shapefile.
#Note the @data - that tells R I'm looking at the Data of the shapefile.
#You can also type names(DHS.locs@data), head, etc. to see the data.
DHS.locs@data <- merge(DHS.locs@data, anc.dta, by="DHSID")

#A quick example of how to make a helpful visualization

#First, subset the data so it only contains one year:
DHS_2013 <- DHS.locs[DHS.locs@data$DHSYEAR.x == 2013,]

#Now, create a quick map.
saved_DHS_2013_map <- spplot(DHS_2013["sslp_e"], main="2013", sp.layout=list(Lib.bound))

#Plot it by simply calling the name you saved it as:
saved_DHS_2013_map

#Note there is a point WAY outside of Liberia.  
#You can quickly remove all points
#That don't fall inside Liberia by subsetting the data
#Spatially:
DHS_2013.subset <- DHS_2013[Lib.bound,]

#Now, plot the map just like before, but with the subset:
DHS_2013_map.subset <- spplot(DHS_2013.subset["sslp_e"], main="2013", sp.layout=list(Lib.bound))

#And, remember to call your new map:
DHS_2013_map.subset 

#A few quick formatting examples so you can actually see something - type ?spplot for more
formatted.DHS.2013 <- spplot(DHS_2013.subset["sslp_e"], main="2013", sp.layout=list(Lib.bound), col="transparent", cex=0.5)
formatted.DHS.2013

DHS_2013.subset@data$dist_nearest <- NA

for(dhs_location in 1:length(DHS_2013.subset))
{
  within_var <- over(DHS_2013.subset[dhs_location,], Lib.bound)
  
  if(is.na(within_var))
  {
    min_dist <- min(spDists(DHS_2013.subset[dhs_location,], Lib.bound))
    DHS_2013.subset@data$dist_nearest[dhs_location]
  }
  else
  {
    DHS_2013.subset@data$dist_nearest[dhs_location] <- 0
  }
}
  
  
  


