library(foreigh)
library(maptools)

setwd("/home/dan/Desktop/GitRepo/Liberia/")

AllLoc <- read.csv("/home/dan/Desktop/GitRepo/Liberia/Afrobarometer/AB_Locations_ALL.csv")

AlllocDF <- as.data.frame(AllLoc)
ADF <- subset(AlllocDF, country=="LIBERIA")

shp_YY <- readShapePoints("/home/dan/Desktop/GitRepo/Liberia/Afrobarometer/Yang-Yang's shapefiles/Liberia.shp")
plot(shp_YY)

shp_YY$join <- as.character(shp_YY$RESPNO)
shp_YY$join <- enc2utf8(shp_YY$join)

R4 <- read.spss("/home/dan/Desktop/GitRepo/Liberia/Afrobarometer/merged_r4_data.sav")
R4_Inc <- read.dta("/home/dan/Desktop/GitRepo/Liberia/Afrobarometer/merged_r4_data INCOMPLETE.dta")


R4_Lib <- as.data.frame(R4_Inc)
R4_Lib_sub <- subset(R4_Lib, COUNTRY=="Liberia")
R4_Lib_sub$join <- as.character(R4_Lib_sub$RESPNO)

Encoding(R4_Lib_sub$join)
Encoding(shp_YY$join)

Encoding(R4_Lib_sub$join) <- "UTF-8"
Encoding(shp_YY$join) <- "UTF-8"

iconv(R4_Lib_sub$join, "unknown", "UTF-8")

Test <- merge(shp_YY, R4_Lib_sub,by.x="join", by.y="join")
