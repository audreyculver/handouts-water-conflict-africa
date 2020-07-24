#Packages##
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
install.packages("dplyr")
install.packages("rworldmap")
install.packages("sf")

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

##Read in the shapefiles
Africa<-readOGR(".", "Africa")
plot(Africa) 
Africa 

##Read in the Conflict Data##
#ACLED#
#ACLED_Northern<-read.csv("ACLED9719_Northern_Africa.csv")
#ACLED_Eastern<-read.csv("ACLED9719_Eastern_Africa.csv")
#ACLED_Central<-read.csv("ACLED9719_Middle_Africa.csv")
#ACLED_Western<-read.csv("ACLED9719_Western_Africa.csv")
#ACLED_Southern<-read.csv("ACLED9719_Southern_Africa.csv")
#ACLED_merge<-rbind(ACLED_Northern, ACLED_Eastern, ACLED_Central, ACLED_Western, ACLED_Southern)
#Transaction data
Tran <- read.csv("AllLM.csv")


##Mapping points##
newmap <- getMap(resolution = "low") #function to get maps stored in a rworldmap
plot(newmap, xlim = c(-40, 60), ylim = c(0, 10), asp = 1) #plot new map with x and y limits
points(Tran$Longitude, Tran$Latitude, col = "red", cex = .4, pch=19) #draw a seq of points at the specified lat/lon

##Display X,Y Data 
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
#crs will get or set the Coord. ref. system of a raster or spatial object
coords <- Tran[c("Longitude","Latitude")] 
#create coords obkecy by concatenating long and lat

Tran_pt <- SpatialPointsDataFrame(coords = coords, Tran, proj4string = projection_info) 
#in {sp} - SpatialPointsDataFrame function, inputs (coordinates, data, projection string of CRS-class)
#SpatialPointsDataFrame create objects of class spatialpoints (class for irregularly spaced points) 
#or spatialpointsdataframe (class for spatial attr that have spatial pnt location)
plot(Tran_pt, col = "red", cex = .4, pch=19)
plot(Africa, add=TRUE)


###########First I want to clean the intention.of.investment column
##Summary Statistics and mapping it back to the Africa shapefile##

#CountALLConflictCountry<-as.data.frame(ACLED_merge %>% count(country))
#create df based on ACLED_merge for a count of observations by group {dplyr}

#CountConflictCountry<-as.data.frame(ACLED_merge %>% group_by(event_type) %>% count(country))
#create df from ACLED_merge that is grouped by event type and a count of each event type for each country

#CountConflictRegion<-as.data.frame(ACLED_merge %>% count(event_type, year, region))
#create a df from ACLED_merge to count event_type in each year and each region

#CountConflictCountryYear<-as.data.frame(ACLED_merge %>% count(event_type, year, country))
#create a df from ACLED_merge for the count of event_type in each year in every country
#sp <- ggplot(CountConflictCountryYear, aes(x=year, y=n, colour=country)) + geom_line()+ facet_grid(event_type ~ .)

CountALLtran<-as.data.frame(Tran %>% count(Target.country))
#create df from Tran for a count of observations in each country
CountConflictCountry<-as.data.frame(Tran %>% sum(Deal.size) %>% count(Target.country))
#create a df grouped by intention.of.investment with a count of each type of intended use by country
#CountConflictRegion<-as.data.frame(ACLED_merge %>% count(event_type, year, region))
#create a df to count intended use in each (year?) in every country
CountConflictCountryYear<-as.data.frame(ACLED_merge %>% count(event_type, year, country))
#create a df of intended use, for each year in every country
sp <- ggplot(CountConflictCountryYear, aes(x=year, y=n, colour=country)) + geom_line()+ facet_grid(event_type ~ .)
#If we can get a good column for year we could do it this way. If not I would do a histogram where x axis is country
#each country should have two bars (one for the total number of transactions and one for the sum of deal size) This would require 2 y axez
#or x axis is country and two bars, one for each type of intended use and the total n

CountALLConflictCountry_joined <- merge(Africa, CountALLConflictCountry, by.x="COUNTRY", by.y="Target country")
#Merge may need to use same col names???
#in package sp, the merge function merges a spatial object having a df (i.e. merging non-spatial attr)

# Change country names when they do not match Africa.shp, for example: Swaziland
#CountConflictCountry$country[CountConflictCountry$country == "eSwatini"] <- "Swaziland"

#Battles <- CountConflictCountry[ which(CountConflictCountry$event_type=='Battles'),]
#RemoteViolence <- CountConflictCountry[ which(CountConflictCountry$event_type=='Explosions/Remote violence'),]
#Protests <- CountConflictCountry[ which(CountConflictCountry$event_type=='Protests'),]
#Riots <- CountConflictCountry[ which(CountConflictCountry$event_type=='Riots'),]
#StrategicDev <- CountConflictCountry[ which(CountConflictCountry$event_type=='Strategic developments'),]
#ViolenceCitizens <- CountConflictCountry[ which(CountConflictCountry$event_type=='Violence against civilians'),]
#pull from the dfs created above to create objects with specific attr values
#which is a base function that gives the TRUE indices of a logical object

#Fixing Names in the Africa attribute table#
Africa$COUNTRY <- as.character(Africa$COUNTRY) #create a character vector out of country col in Africa file
Africa@data[Africa@data$COUNTRY == "Cote d`Ivoire", "COUNTRY"] <- "Ivory Coast" 
# @ will extract or replace contents of a slot in an object with a formal (S4) class structure
#take the data of the Africa.shp. in the data's COUNTRY attr, fine Cote d'Ivoire in the country col and replace it with Ivory Coast


##Density / Hotspot Analysis: All ACLED##
Tran_ptBACKUP<-Tran_pt
Tran_pt <- SpatialPointsDataFrame(coords = coords, Tran, proj4string = projection_info) 
#create a spatial points df from coords with df Tran

###Only run if not on ALL ACLED points#### ACLED_pt <- ACLED_pt[ which(ACLED_pt$event_type=="Battle"),]
##Year chunks:TBD##
##Do we need to cluster????????????????????
#ACLED_pt$yr3Cluster<-""
#ACLED_pt$yr5Cluster<-""

#pixel size, just setting a value to an object names pixelsize
pixelsize = .5 


#Africa bounding box / raster template
#creates a raster for our map of Africa
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
Tran_pt$PRESENT <- 1


heatVector <- list() #create an empty list
years<-c(2000:2001)

#rasterize {raster} transfers values assciated with the object type spatial data
#to raster cells
?focalWeight
for (i in 1:length(years)) { 
  y <- years[i] 
  ACLED<-Tran_pt[ which(Tran_pt$Year==y),]
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

heat.raster.stack<-stack(heat.raster)

#How do we change the color ramp?
#plot(heat.raster.stack[[1]])
#plot(Africa, border = 'azure3', lwd = .25, add=TRUE)

pdf("finalmap.pdf", width = 4, height = 5)
plot(heat.raster.stack[[1]])
plot(Tran_pt)
plot(Africa, border = 'azure3', lwd = .25, add=TRUE)
dev.off()