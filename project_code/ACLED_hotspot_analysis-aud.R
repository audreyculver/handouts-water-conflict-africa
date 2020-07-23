##################################Conflict Hotspot Analysis########################

##Set working directory##
setwd("C:/Users/audre/Desktop/H2O-Conflict/sesync_summer_2020")

getwd()

##Packages##
install.packages("shiny")
install.packages("rgdal")
install.packages("maptools")
install.packages("raster")
install.packages("ggplot2")
install.packages("spatstat")
install.packages("shapefiles")
install.packages("reshape2")
install.packages("sp")
install.packages("GISTools")

library(shiny)
library(rgdal)
library(maptools)
library(raster)
library(ggplot2)
library(spatstat)
library(shapefiles)
library(reshape2)
library(sp)
library(readr)
library(dplyr)
library(sf)
library(rworldmap)
library(RColorBrewer)
library(rgeos)
library(GISTools)

getwd()

##Read in the shapefiles, like African country boundaries##
Africa<-readOGR("/home/audrey/water-conflict-africa-handouts/project_data/Africa")
plot(Africa) 
Africa 

#Full ACLED
ACLED <- read.csv("project_data/Africa_1997-2020.csv")

##Read in the Conflict Data##
#ACLED#
#ACLED_Northern<-read.csv("ACLED9719_Northern_Africa.csv")
#ACLED_Eastern<-read.csv("ACLED9719_Eastern_Africa.csv")
#ACLED_Central<-read.csv("ACLED9719_Middle_Africa.csv")
#ACLED_Western<-read.csv("ACLED9719_Western_Africa.csv")
#ACLED_Southern<-read.csv("ACLED9719_Southern_Africa.csv")


#ACLED_merge<-rbind(ACLED_Northern, ACLED_Eastern, ACLED_Central, ACLED_Western, ACLED_Southern)

#UPSAL#
setwd("C:/Users/audre/Desktop/H2O-Conflict/sesync_summer_2020")

UPSALA<-read.csv("project_data/UPSALA_AfricaConflict.csv")

##Mapping points##
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-40, 60), ylim = c(0, 10), asp = 1)
points(ACLED$LONGITUDE, ACLED$LATITUDE, col = "red", cex = .4, pch=19)
points(UPSALA$longitude, UPSALA$latitude, col = "red", cex = .4, pch=19)

##Display X,Y Data
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

coords <- ACLED[c("LONGITUDE","LATITUDE")] 
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED, proj4string = projection_info) 

coords2<-UPSALA[c("longitude","latitude")] 
UPSAL_pt <- SpatialPointsDataFrame(coords = coords2, UPSALA, proj4string = projection_info) 

plot(ACLED_pt, col = "red", cex = .4, pch=19)
plot(UPSAL_pt, col="green", cex=.4, pch=19, add=TRUE)
plot(Africa, add=TRUE)

##Summary Statistics and mapping it back to the Africa shapefile##
CountALLConflictCountry<-as.data.frame(ACLED %>% count(COUNTRY))

CountConflictCountry<-as.data.frame(ACLED %>% group_by(EVENT_TYPE) %>% count(COUNTRY))

CountConflictRegion<-as.data.frame(ACLED %>% count(EVENT_TYPE, YEAR, REGION))

CountConflictCountryYear<-as.data.frame(ACLED %>% count(EVENT_TYPE, YEAR, COUNTRY))

sp <- ggplot(CountConflictCountryYear, aes(x=YEAR, y=n, colour=COUNTRY)) + geom_line()+ facet_grid(EVENT_TYPE ~ .)
sp

CountALLConflictCountry_joined <- merge(Africa, CountALLConflictCountry, by.x="COUNTRY", by.y="COUNTRY")

CountConflictCountry$COUNTRY[CountConflictCountry$COUNTRY == "eSwatini"] <- "Swaziland"

Battles <- CountConflictCountry[ which(CountConflictCountry$EVENT_TYPE=='Battles'),]
RemoteViolence <- CountConflictCountry[ which(CountConflictCountry$EVENT_TYPE=='Explosions/Remote violence'),]
Protests <- CountConflictCountry[ which(CountConflictCountry$EVENT_TYPE=='Protests'),]
Riots <- CountConflictCountry[ which(CountConflictCountry$EVENT_TYPE=='Riots'),]
StrategicDev <- CountConflictCountry[ which(CountConflictCountry$EVENT_TYPE=='Strategic developments'),]
ViolenceCitizens <- CountConflictCountry[ which(CountConflictCountry$EVENT_TYPE=='Violence against civilians'),]

#Fixing Names in the Africa attribute table#
Africa$COUNTRY <- as.character(Africa$COUNTRY)
Africa@data[Africa@data$COUNTRY == "Cote d`Ivoire", "COUNTRY"] <- "Ivory Coast"

##Density / Hotspot Analysis: All ACLED##
ACLED_ptBACKUP<-ACLED_pt  ##ACLED_pt<-ACLED_ptBACKUP
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED, proj4string = projection_info) 

###Only run if not on ALL ACLED points#### ACLED_pt <- ACLED_pt[ which(ACLED_pt$event_type=="Battle"),]

##Year chunks:TBD##
ACLED_pt$yr3Cluster<-""
ACLED_pt$yr5Cluster<-""

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_pt$PRESENT <- 1

#subset for each year

setwd("project_data") ##change this for the different types
getwd()

heatVector <- list() #create an empty list
years<-c(1997:2018)

for (i in 1:length(years)) { 
  y <- years[i] 
  ACLED<-ACLED_pt[ which(ACLED_pt$YEAR==y),]
  rasterACLED <- rasterize(ACLED, template, field = 'PRESENT', fun = sum)
  rasterACLED2<-log(rasterACLED)
  kernel = focalWeight(rasterACLED2, d = .01, type = 'Gauss')
  heat = focal(rasterACLED2, kernel, fun = sum, na.rm=T)
  
  f<-paste0('heat', y, '.tif')
  writeRaster(heat,filename=f,options=c('TFW=YES'), overwrite=TRUE)
  
  Allevent_polygons <- rasterToPolygons(x =heat, n=8, dissolve=TRUE)
  
  Allevent_polygons2<-Allevent_polygons
  Allevent_polygons_class2 <- Allevent_polygons[Allevent_polygons$layer > 0 & Allevent_polygons$layer < 2,]
  Allevent_polygons_class4 <- Allevent_polygons[Allevent_polygons$layer >= 2 & Allevent_polygons$layer < 4,]
  Allevent_polygons_class6 <- Allevent_polygons[Allevent_polygons$layer >= 4 & Allevent_polygons$layer < 6,]
  Allevent_polygons_class8 <- Allevent_polygons[Allevent_polygons$layer >= 6 & Allevent_polygons$layer < 8,]
  Allevent_polygons_classgrt8 <- Allevent_polygons[Allevent_polygons$layer >= 8,]
  
  Allevent_polygons2@data$layer <-ifelse (Allevent_polygons@data$layer >= 0 & Allevent_polygons@data$layer < 2, "2",
                                          ifelse (Allevent_polygons@data$layer >= 2 & Allevent_polygons@data$layer < 4, "4",
                                                  ifelse(Allevent_polygons@data$layer >= 4 & Allevent_polygons@data$layer < 6, "6", 
                                                         ifelse (Allevent_polygons@data$layer >= 6 & Allevent_polygons@data$layer < 8, "8", "9"))))
  
  heatVector[[i]]<-Allevent_polygons2
  
} 



#Reading in the hotspot data just created#
raster.files<-list.files(pattern=".tif") ##why use the .tif in the pattern recognition rather than part of the name?
raster.files

raster.filesALL<-unique(raster.files) ##what does the unique function do?

heat.raster<-lapply(raster.filesALL, raster)
#plot(heat.raster)
heat.raster.stack<-stack(heat.raster)

plot(heat.raster.stack[[1]])
plot(Africa, add=TRUE)

#Pixel counts per layer#
plot(heatVector[[1]])
plot(Africa, add=TRUE)

projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
##AfricaCount<-Africa

#Put this in loop#
trial<-heatVector[[1]]
trial2<-as.data.frame(sapply(over(Africa, geometry(trial), returnList = TRUE), length))
colnames(trial2)<- c("count")
AfricaCount$count1997<-trial2$count

trial3<-data.frame(AfricaCount)

trial4<-as.data.frame(tapply(trial3$count, trial3$COUNTRY, sum))
colnames(trial4)<- c( "counts")


