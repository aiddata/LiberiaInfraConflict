setwd("C:/Users/Harsh/Desktop/AidData/libgie")
library(rgdal)
library(ltm)
library(maptools)
library(rgeos)
library(foreign)
library(stargazer)
require(rgeos)
require(maptools)
library(polycor)
library(sp)
library(plyr)
library(devtools)
library(reshape2)
library(doBy)
library(MatchIt)
library(PBSmapping)
library(multiwayvcov)
library(lmtest)
#define distances for buffers: 2km urban, 5km rural
distInMetersU <- 2000
distInMetersR <- 5000
#import dhs 2007 shapefile
dhs2007.shpfile.location <-"C:/Users/Harsh/Desktop/AidData/libgie/2007 DHS Liberia/lbge52fl Geocoding/LBGE52FL.shp"
dhs.2007 <- readOGR(dhs2007.shpfile.location,"LBGE52FL")
#subset data into urban and rural for each wave of DHS, so that we can subset data for creating buffers
dhs.2007.u <-dhs.2007[dhs.2007@data$URBAN_RURA=="U",]
dhs.2007.r <-dhs.2007[dhs.2007@data$URBAN_RURA=="R",]
#define projections for subsets and generate buffers 
dhs.2007.u <- spTransform(dhs.2007.u,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2007.u.buffer <-gBuffer(dhs.2007.u,byid = TRUE,width=distInMetersU,capStyle = "ROUND",joinStyle = "ROUND")
dhs.2007.r <- spTransform(dhs.2007.r,CRS("+proj=merc +zone=29 +datum=WGS84"))
dhs.2007.r.buffer <-gBuffer(dhs.2007.r,byid = TRUE,width=distInMetersR,capStyle = "ROUND",joinStyle = "ROUND")
#import concessions shapefile
concessions.shp.location <-"C:/Users/Harsh/Desktop/AidData/libgie/confinal/confinal.shp"
confinal <- readOGR(concessions.shp.location, "confinal")
#rename year field
names(confinal)[names(confinal)=="year_1"] <- "year"
names(confinal)[names(confinal)=="phys_inf_1"] <- "phys_infra" 
confinal@data$year <- as.numeric(as.character(confinal@data$year)) #convert factor to character, then to numeric
#subset data to only include >=2003 and =<2013 licenses, and to only include extraction licenses
concessions <- confinal[which(confinal@data$year >= 2007 & confinal@data$year <= 2013),]
concessions.extractonly <- concessions[concessions@data$yes_extrac == 1,]
#import Liberia outcome and ancillary data
liberia_data.location <-"C:/Users/Harsh/Desktop/AidData/libgie/liberia_data.csv"
liberia_data <- read.csv(file=liberia_data.location,header=TRUE,sep = ",")
ntl.2007.location <-"C:/Users/Harsh/Desktop/AidData/libgie/merge_liberia_dhs_buffers.csv"
ntl_2007 <- read.csv(file=ntl.2007.location,header=TRUE,sep = ",")
#merge urban and rural buffers for each year
dhs.2007.buffers <-spRbind(dhs.2007.r.buffer,dhs.2007.u.buffer)
#redefine projects for concessions shapefile and each dhs wave 
concessions <- spTransform(concessions,CRS("+proj=merc +zone=29 +datum=WGS84"))
concessions.extractonly <- spTransform(concessions.extractonly,CRS("+proj=merc +zone=29 +datum=WGS84")) 
dhs.2007 <-spTransform(dhs.2007,CRS("+proj=merc +zone=29 +datum=WGS84"))
#for 2007 cross-section, calculate nearest distance from each DHS point to 
#a concession for both all and extract only files
dhs.2007@data$dist_nearest <- NA
for(i in 1:length(dhs.2007))
{
  within_2007 <- over(dhs.2007[i,],concessions,fn=NULL)
  if(is.na(within_2007$title))
  {
    min_dist <-min(spDists(dhs.2007[i,],concessions,longlat = FALSE))
    dhs.2007@data$dist_nearest[i] <- min_dist
  } 
  else
  {
    dhs.2007@data$dist_nearest[i] <-0
  }
}
for (i in 1:length(dhs.2007))
{
  within_2007_extractonly <- over(dhs.2007[i,],concessions.extractonly,fn=NULL)
  if(is.na(within_2007_extractonly$title))
  {
    min_dist_extractonly <- min(spDists(dhs.2007[i,],concessions.extractonly,longlat = FALSE))
    dhs.2007@data$dist_nearest_extractonly[i] <- min_dist_extractonly
  }
  else
    dhs.2007@data$dist_nearest_extractonly[i] <-0
}
#for 2007 cross-section, generate count of concessions that intersect
#with each DHS buffer, write that into the original dhs.2007 shapefile
dhs.2007@data$overlap_counts <-NA
gO_2007 <- gOverlaps(dhs.2007.buffers,concessions,byid=c(TRUE,TRUE))
dim(gO_2007)
overlaps_count_2007 <- apply(gO_2007,2,sum)
dhs.2007@data$overlap_counts <- overlaps_count_2007
dhs.2007@data$overlap_counts_extractonly <-NA
#do this for extraction only concessions
gO_2007_extractonly <- gOverlaps(dhs.2007.buffers,concessions.extractonly,byid=c(TRUE,TRUE))
dim(gO_2007_extractonly)
overlaps_count_2007_extractonly <- apply(gO_2007_extractonly,2,sum)
dhs.2007@data$overlap_counts_extractonly <- overlaps_count_2007_extractonly
#require sp to allow for sp merge between 2013 shapefile and outcome data
#outcome data file is liberia_data
require(sp)
dhs.2007@data <- merge(dhs.2007@data,liberia_data,by="DHSID") 
#read in dhs covariates --> only need the individual covariates file for variables of interest
dhs.2007.individual.location <- "C:/Users/Harsh/Desktop/AidData/libgie/2007 DHS Liberia/lbir51dt Individual/LBIR51FL.DTA"
dhs2007_individual <- read.dta(dhs.2007.individual.location)
#calculate aggregates for each relevant variable of interest
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
v106 <- aggregate(v106 ~ v001,dhs2007_individual,Mode)
v130 <- aggregate(v130 ~ v001,dhs2007_individual,Mode)  
v133 <- aggregate(v133 ~ v001,dhs2007_individual,Mode)
v136 <- aggregate(v136 ~ v001,dhs2007_individual,Mode)
v149 <- aggregate(v149 ~ v001,dhs2007_individual,Mode) 
v151 <- aggregate(v151 ~ v001,dhs2007_individual,Mode)  
v152 <- aggregate(v152 ~ v001,dhs2007_individual,Mode)
v190 <- aggregate(v190 ~ v001,dhs2007_individual,Mode)
v191 <- aggregate(v191 ~ v001,dhs2007_individual,Mode)
v501 <- aggregate(v501 ~ v001,dhs2007_individual,Mode)  
v155 <- aggregate(v155 ~ v001,dhs2007_individual,Mode)
v504 <- aggregate(v504 ~ v001,dhs2007_individual,Mode)
v714 <- aggregate(v714 ~ v001,dhs2007_individual,Mode)
v716 <- aggregate(v716 ~ v001,dhs2007_individual,Mode)
v119 <- aggregate(v119 ~ v001,dhs2007_individual,Mode)
v120 <- aggregate(v120 ~ v001,dhs2007_individual,Mode)
v121 <- aggregate(v121 ~ v001,dhs2007_individual,Mode)
v122 <- aggregate(v122 ~ v001,dhs2007_individual,Mode)
v123 <- aggregate(v123 ~ v001,dhs2007_individual,Mode)
v124 <- aggregate(v124 ~ v001,dhs2007_individual,Mode)
v125 <- aggregate(v125 ~ v001,dhs2007_individual,Mode)
#merge all covariates into one file
agg.compiled <-Reduce(function(x, y) merge(x, y, all=TRUE), list(v106,v130,v133,v136,v149,v151,
                                                                 v152,v190,v191,v501,v155,v504,
                                                                 v714,v716,v119,v120,v121,
                                                                 v122,v123,v124,v125))
#rename the variable in agg.compiled to allow merge with dhs.2007 data
#v001 corresponds with DHS clusters, which allows the merging of covariates into shapefile
agg.compiled <- rename(agg.compiled, c("v001"="DHSCLUST"))
source("SciClone_functions.R")
dhs.2007@data <- merge(dhs.2007@data,agg.compiled, by="DHSCLUST")
#set categorical variables to factors
dhs.2007@data$v106 <- factor(dhs.2007@data$v106)
dhs.2007@data$v130 <- factor(dhs.2007@data$v130)
dhs.2007@data$v133 <- factor(dhs.2007@data$v133)
dhs.2007@data$v149 <- factor(dhs.2007@data$v149)
dhs.2007@data$v501 <- factor(dhs.2007@data$v501)
dhs.2007@data$v504 <- factor(dhs.2007@data$v504)
dhs.2007@data$v151 <- factor(dhs.2007@data$v151)
dhs.2007@data$v155 <- factor(dhs.2007@data$v155)
dhs.2007@data$v714 <- factor(dhs.2007@data$v714)
dhs.2007@data$v716 <- factor(dhs.2007@data$v716)
#rename all DHS covariates
names(dhs.2007)[names(dhs.2007)=="v106"] <- "Edu_Lvl"
names(dhs.2007)[names(dhs.2007)=="v130"] <- "Religion"
names(dhs.2007)[names(dhs.2007)=="v133"] <- "Edu_Yrs"
names(dhs.2007)[names(dhs.2007)=="v136"] <- "Household"
names(dhs.2007)[names(dhs.2007)=="v149"] <- "Edu_Achievement"
names(dhs.2007)[names(dhs.2007)=="v151"] <- "Gender"
names(dhs.2007)[names(dhs.2007)=="v152"] <- "Age"
names(dhs.2007)[names(dhs.2007)=="v190"] <- "Wealth"
names(dhs.2007)[names(dhs.2007)=="v191"] <- "Wealth_Factor"
names(dhs.2007)[names(dhs.2007)=="v501"] <- "Marital_Status"
names(dhs.2007)[names(dhs.2007)=="v155"] <- "Literacy"
names(dhs.2007)[names(dhs.2007)=="v504"] <- "Residence"
names(dhs.2007)[names(dhs.2007)=="v714"] <- "Working"
names(dhs.2007)[names(dhs.2007)=="v716"] <- "Occupation"
#calculate pre and post averages and pre trends
dhs.2007@data$preAvgNTL <- timeRangeAvg(dhs.2007,"ncc4_","e",1992,2007,"y")
dhs.2007@data$postAvgNTL <- timeRangeAvg(dhs.2007,"ncc4_","e",2013,2013,"y")
dhs.2007@data$preTrendNTL <- timeRangeTrend(dhs.2007,"ncc4_[0-9][0-9][0-9][0-9]e",1992,2007,"DHSID", "y")

#first linear model: count of overlaps
#create a new data frame and set all units in treatment to 0:
DTA_overlaps <- dhs.2007
DTA_overlaps@data$Treatment <- 0
#Calculate the Median Overlap
med_overlap <- median(DTA_overlaps@data$overlap_counts)
#Set units with overlap > med_overlap to 1:
DTA_overlaps@data$Treatment <- (as.numeric((DTA_overlaps@data$overlap_counts) > med_overlap))
#Double check:
summary(DTA_overlaps@data$Treatment)
#Check how many units of observation this leaves us with (our initial n for the
#analysis).
length(DTA_overlaps@data)
#first stage of psm model for overlap counts, set caliper to .5 
aVars <- c("Treatment","preTrendNTL","preAvgNTL","postAvgNTL","Edu_Lvl","Religion",
           "Household","Gender","Age","Literacy","Wealth","Marital_Status",
           "Residence","Working","Occupation","DHSREGNA")
matchit.overlaps <- matchit(Treatment ~ preTrendNTL + preAvgNTL + postAvgNTL + Edu_Lvl + Religion +
                            Household + Gender + Age + Literacy + Wealth +
                            Marital_Status + Residence + Working + Occupation + DHSREGNA, 
                            data=DTA_overlaps@data[aVars],
                            method="nearest",distance="logit", 
                            caliper=0.5)
summary(matchit.overlaps)
modelOverlaps <- match.data(matchit.overlaps)
modelOverlaps$outcome <- modelOverlaps$postAvgNTL - modelOverlaps$preAvgNTL
summary(LM1 <- lm(outcome ~ Treatment + preTrendNTL + preAvgNTL + Edu_Lvl + 
                    Religion + Household + Gender + Age + Literacy + Wealth + 
                    Marital_Status + Residence + Working + Occupation + 
                    factor(DHSREGNA),
                  data=modelOverlaps))

#second linear model: nearest distance to a concession
#create a new data frame and set all units in the treatment to 0:
DTA_dist <- dhs.2007
DTA_dist@data$Treatment <- 0
#Calculate the Median Nearest Distance
med_dist <- median(DTA_dist$dist_nearest)
#Set units with nearest distance > med_dist to 1:
DTA_dist@data$Treatment <- (as.numeric((DTA_dist@data$dist_nearest) < med_dist))
#Double check:
summary(DTA_dist@data$Treatment)
#Check how many units of observation this leaves us with (our initial n for the
#analysis).
length(DTA_dist@data)
#first stage of psm model for nearest distance
aVars <- c("Treatment","preTrendNTL","preAvgNTL","postAvgNTL","Edu_Lvl","Religion",
           "Household","Gender","Age","Literacy","Wealth","Marital_Status",
           "Residence","Working","Occupation","DHSREGNA")
matchit.dist <- matchit(Treatment ~ preTrendNTL + preAvgNTL + postAvgNTL + Edu_Lvl + Religion +
                              Household + Gender + Age + Literacy + Wealth +
                              Marital_Status + Residence + Working + Occupation + DHSREGNA, 
                            data=DTA_dist@data[aVars],
                            method="nearest",distance="logit", 
                            caliper=0.5)
summary(matchit.dist)
modelDistance <- match.data(matchit.dist)
modelDistance$outcome <- modelDistance$postAvgNTL - modelDistance$preAvgNTL
summary(LM2 <- lm(outcome ~ Treatment + preTrendNTL + preAvgNTL + Edu_Lvl + 
                    Religion + Household + Gender + Age + Literacy + Wealth + 
                    Marital_Status + Working + Occupation + 
                    factor(DHSREGNA),
                  data=modelDistance))

#third linear model: nearest distance to a concession related to extraction
DTA_dist_ext <- dhs.2007
DTA_dist_ext@data$Treatment <- 0
#Calculate the Median Nearest Distance
med_dist_extractonly <- median(DTA_dist_ext$dist_nearest_extractonly)
#Set units with nearest distance > med_dist to 1:
DTA_dist_ext@data$Treatment <- (as.numeric((DTA_dist_ext@data$dist_nearest_extractonly) < med_dist_extractonly))
#Double check:
summary(DTA_dist_ext@data$Treatment)
#Check how many units of observation this leaves us with (our initial n for the
#analysis).
length(DTA_dist_ext@data)
#first stage of psm model for nearest distance (extraction only)
aVars <- c("Treatment","preTrendNTL","preAvgNTL","postAvgNTL","Edu_Lvl","Religion",
           "Household","Gender","Age","Literacy","Wealth","Marital_Status",
           "Residence","Working","Occupation","DHSREGNA")
matchit.dist.ext <- matchit(Treatment ~ preTrendNTL + preAvgNTL + postAvgNTL + Edu_Lvl + Religion +
                              Household + Gender + Age + Literacy + Wealth +
                              Marital_Status + Residence + Working + Occupation + DHSREGNA, 
                            data=DTA_dist_ext@data[aVars],
                            method="nearest",distance="logit", 
                            caliper=0.5)
summary(matchit.dist.ext)
modelDistance_ext <- match.data(matchit.dist.ext)
modelDistance_ext$outcome <- modelDistance_ext$postAvgNTL - modelDistance_ext$preAvgNTL
summary(LM3 <- lm(outcome ~ Treatment + preTrendNTL + preAvgNTL + Edu_Lvl + 
                    Religion + Household + Gender + Age + Literacy + Wealth + 
                    Marital_Status + Working + Occupation + 
                    factor(DHSREGNA),
                  data=modelDistance_ext))

#fourth model, overlaps with concessions only related to extraction
DTA_overlaps_ext <- dhs.2007
DTA_overlaps_ext@data$Treatment <- 0
#Calculate the Median Overlap
med_overlap_extractonly <- median(DTA_overlaps_ext@data$overlap_counts_extractonly)
#Set units with overlap >med_overlap to 1:
DTA_overlaps_ext@data$Treatment <- (as.numeric(DTA_overlaps_ext@data$overlap_counts_extractonly) > med_overlap_extractonly)
#Double check:
summary(DTA_overlaps_ext@data$Treatment)
#Check how many units of observation this leaves us with (our initial n for the
#analysis).
length(DTA_overlaps_ext@data)
#first stage of psm model for overlap counts, set caliper to .5 
aVars <- c("Treatment","preTrendNTL","preAvgNTL","postAvgNTL","Edu_Lvl","Religion",
           "Household","Gender","Age","Literacy","Wealth","Marital_Status",
           "Residence","Working","Occupation","DHSREGNA")
matchit.overlaps.ext <- matchit(Treatment ~ preTrendNTL + preAvgNTL + postAvgNTL + Edu_Lvl + Religion +
                              Household + Gender + Age + Literacy + Wealth +
                              Marital_Status + Residence + Working + Occupation + DHSREGNA, 
                            data=DTA_overlaps_ext@data[aVars],
                            method="nearest",distance="logit", 
                            caliper=0.5)
summary(matchit.overlaps.ext)
modelOverlaps_ext <- match.data(matchit.overlaps.ext)
modelOverlaps_ext$outcome <- modelOverlaps_ext$postAvgNTL - modelOverlaps_ext$preAvgNTL
summary(LM4 <- lm(outcome ~ Treatment + preTrendNTL + preAvgNTL + Edu_Lvl + 
                    Religion + Household + Gender + Age + Literacy + Wealth + 
                    Marital_Status + Working + Occupation + 
                    factor(DHSREGNA),
                  data=modelOverlaps_ext))
stargazer(LM1,LM4,
          type = "text",
          align = TRUE,
          dep.var.labels = "Night-time Lights",
          covariate.labels = c("Treatment,","Night-time Lights, Pre-Trend (1992-2006)",
                               "Night-Time Lights, Pre-Average (1992-2006)","Education Level (Primary)",
                               "Education Level (Secondary)","Religion","Years of Education (3)",
                               "Years of Education (4)","Years of Education (6)","Years of Education (8)",
                               "Years of Education (12)","Living in Household",
                               "Education Achievement (Incomplete Primary)","Education Achievement (Incomplete Secondary)",
                               "Education Achievement (Complete Secondary)","Gender of Head of Household (Female)",
                               "Age of Head of Household","Literacy (High)",
                               "Wealth (Poorer)","Wealth (Middle)","Wealth (Richer)","Wealth (Richest)",
                               "Wealth (Factor Score)","Marital Status (Married)","Marital Status (Living Together)","Marital Status (Not Living Together)",
                               "Residing with Partner","Working (Yes)","Occupation (44)","Occupation (62)","Occupation (65)",
                               "DHS Region (North Central)","DHS Region (North Western)","DHS Region (South Central)","DHS Region (South Eastern A)","DHS Region (South Eastern B)"),
          title = "Overlap Counts (2007-2013)",
          column.labels = c("All Concessions","Extraction Concessions Only"),
          out = "overlaps_all.txt")
stargazer(LM2,LM3,
          type = "text",
          align = TRUE,
          dep.var.labels = "Night-time Lights",
          title = "Distance to Concession (2007-2013)",
          column.labels = c("All Concessions","Extraction Concessions Only"),
          covariate.labels = c("Treatment,","Night-time Lights, Pre-Trend (1992-2006)",
                               "Night-Time Lights, Pre-Average (1992-2006)","Education Level (Primary)",
                               "Education Level (Secondary)","Religion","Years of Education (3)",
                               "Years of Education (4)","Years of Education (6)","Years of Education (8)",
                               "Years of Education (12)","Living in Household",
                               "Education Achievement (Incomplete Primary)","Education Achievement (Incomplete Secondary)",
                               "Education Achievement (Complete Secondary)","Gender of Head of Household (Female)",
                               "Age of Head of Household","Literacy (High)",
                               "Wealth (Poorer)","Wealth (Middle)","Wealth (Richer)","Wealth (Richest)",
                               "Wealth (Factor Score)","Marital Status (Married)","Marital Status (Living Together)","Marital Status (Not Living Together)",
                               "Residing with Partner","Working (Yes)","Occupation (44)","Occupation (62)","Occupation (65)",
                               "DHS Region (North Central)","DHS Region (North Western)","DHS Region (South Central)","DHS Region (South Eastern A)","DHS Region (South Eastern B)"),
          out = "distance_all.txt")
#generate histograms for each of the outcome variables (post - pre) 
hist1 <- hist(modelOverlaps$outcome,xlab = "NTL Outcome",main = "Outcome - Overlaps")
hist2 <- hist(modelOverlaps_ext$outcome,xlab = "NTL Outcome",main = "Outcome - Overlaps (Extraction)")
hist3 <- hist(modelDistance$outcome,xlab = "NTL Outcome",main = "Outcome - Distance")
hist4 <- hist(modelDistance_ext$outcome,xlab = "NTL Outcome",main = "Outcome - Distance (Extraction)")
#scatterplots of post on Y and pre on X
plot1 <- plot(DTA_dist$preAvgNTL,DTA_dist$postAvgNTL,xlab = "preNTL",ylab = "postNTL",
              main = "preTreatment Distance")
plot2 <- plot(modelDistance$preAvgNTL,modelDistance$postAvgNTL,xlab = "preNTL",
              ylab = "postNTL",main = "postTreatment Distance")
plot3 <- plot(DTA_overlaps$preAvgNTL,DTA_overlaps$postAvgNTL,xlab = "preNTL",
              ylab = "postNTL",main = "preTreatment Overlaps")
plot4 <- plot(modelOverlaps$preAvgNTL,modelOverlaps$postAvgNTL,xlab = "preNTL",
              ylab = "postNTL",main = "postTreatment Overlaps")
#generate cor for outcome and all of the covariates for Overlaps models
library(plotly)
preNTL_over <- cor(modelOverlaps$outcome,modelOverlaps$preAvgNTL)
edulvl_over <- polyserial(modelOverlaps$outcome,modelOverlaps$Edu_Lvl)
household_over <- cor(modelOverlaps$outcome,modelOverlaps$Household)
gender_over <- biserial.cor(modelOverlaps$outcome,modelOverlaps$Gender)
age_over <- cor(modelOverlaps$outcome,modelOverlaps$Age)
wealth_over <- polyserial(modelOverlaps$outcome,modelOverlaps$Wealth)
literacy_over <- polyserial(modelOverlaps$outcome,modelOverlaps$Literacy)
residence_over <- biserial.cor(modelOverlaps$outcome,modelOverlaps$Residence)
employed_over <- biserial.cor(modelOverlaps$outcome,modelOverlaps$Working)
#generate cor for outcome and all of the covariates for Distance models
preNTL_dist <- cor(modelDistance$outcome,modelDistance$preAvgNTL)
edulvl_dist <- polyserial(modelDistance$outcome,modelDistance$Edu_Lvl)
household_dist <- cor(modelDistance$outcome,modelDistance$Household)
gender_dist <- biserial.cor(modelDistance$outcome,modelDistance$Gender)
age_dist <- cor(modelDistance$outcome,modelDistance$Age)
wealth_dist <- polyserial(modelDistance$outcome,modelDistance$Wealth)
literacy_dist <- polyserial(modelDistance$outcome,modelDistance$Literacy)
residence_dist <- biserial.cor(modelDistance$outcome,modelDistance$Residence)
employed_dist <- biserial.cor(modelDistance$outcome,modelDistance$Working)
#import liberia boundary file to do overlaps between concessions and boundaries
lib.bound.loc <- "C:/Users/Harsh/Desktop/AidData/libgie/district-2007.shp"
lib.bound <- readOGR(lib.bound.loc, "district-2007")
lib.bound <- spTransform(lib.bound,CRS("+proj=merc +zone=29 +datum=WGS84"))
lib.bound.Sinoe <- lib.bound[which(lib.bound$FIRST_CCNA=="Sinoe"),]

