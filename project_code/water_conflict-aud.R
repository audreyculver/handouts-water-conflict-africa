##################################Conflict Hotspot Analysis########################

##Set working directory##
#setwd("C:/Users/audre/Desktop/H2O-Conflict/sesync_summer_2020")
##setwd("~/")
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


##Read in the shapefiles, like African country boundaries##
#Africa<-readOGR(".", "project_data/Africa_shp.shp")
Africa<-readOGR("project_data/Africa_SHP")
Africa <- st_read("/home/audrey/water-conflict-africa-handouts/project_data/Africa_shp")
Africa2 <- st_read("/home/audrey/water-conflict-africa-handouts/project_data/Africa")
plot(Africa2) 
Africa3<-readOGR("/home/audrey/water-conflict-africa-handouts/project_data/Africa")
plot(Africa3) 
#dev.off()

##Read in the Conflict Data##
#ACLED#

ACLED_all<-read.csv("project_data/Africa_1997-2020.csv")
#ACLED_Northern<-read.csv("ACLED9719_Northern_Africa.csv")
#ACLED_Eastern<-read.csv("ACLED9719_Eastern_Africa.csv")
#ACLED_Central<-read.csv("ACLED9719_Middle_Africa.csv")
#ACLED_Western<-read.csv("ACLED9719_Western_Africa.csv")
#ACLED_Southern<-read.csv("ACLED9719_Southern_Africa.csv")

#ACLED_merge<-rbind(ACLED_Northern, ACLED_Eastern, ACLED_Central, ACLED_Western, ACLED_Southern)

#UPSAL and Riots/Protests#
#setwd("/nfs/waterconflictafrica-data/UPSAL Conflict Data/")
UPSALA<-read.csv("project_data/UPSALA_AfricaConflict.csv")


#file.symlink('/nfs/public-data/training', 'data')


#setwd("/nfs/waterconflictafrica-data/geolocated protests/")
MassMob<-read.csv("/home/audrey/water-conflict-africa-handouts/project_data/Locations_with_CCodes.csv")


##Mapping points##
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-40, 60), ylim = c(0, 10), asp = 1)
points(ACLED_all$LONGITUDE, ACLED_all$LATITUDE, col = "red", cex = .4, pch=19)
points(UPSALA$longitude, UPSALA$latitude, col = "green", cex = .4, pch=19)
points(MassMob$LON, MassMob$LAT, col = "blue", cex = .4, pch=19)

##Mapping points##
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-40, 60), ylim = c(0, 10), asp = 1)
points(ACLED_all$LONGITUDE, ACLED_all$LATITUDE, col = "red", cex = .4, pch=19)
points(UPSALA$longitude, UPSALA$latitude, col = "green", cex = .4, pch=19)
points(MassMob$LON, MassMob$LAT, col = "blue", cex = .4, pch=19)

##Display X,Y Data
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa3)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

coords <- ACLED_all[c("LONGITUDE","LATITUDE")] 
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED_all, proj4string = projection_info) 

coords2<-UPSALA[c("longitude","latitude")] 
UPSAL_pt <- SpatialPointsDataFrame(coords = coords2, UPSALA, proj4string = projection_info) 

plot(ACLED_pt, col = "red", cex = .4, pch=19)
plot(UPSAL_pt, col="green", cex=.4, pch=19, add=TRUE)
plot(Africa3, add=TRUE)

##Summary Statistics and mapping it back to the Africa shapefile##
CountALLConflictCountry<-as.data.frame(ACLED_all %>% count(COUNTRY))

CountConflictCountry<-as.data.frame(ACLED_all %>% group_by(EVENT_TYPE) %>% count(COUNTRY))
##CountConflictCountry_UPSAL<-as.data.frame(UPSAL_pt %>% count(COUNTRY))

CountConflictRegion<-as.data.frame(ACLED_all %>% count(EVENT_TYPE, YEAR, REGION))

CountConflictCountryYear<-as.data.frame(ACLED_all %>% count(EVENT_TYPE, YEAR, COUNTRY))

CountALLConflictCountry_joined <- merge(Africa3, CountALLConflictCountry, by.x="COUNTRY", by.y="COUNTRY")

##CountConflictCountry$country[CountConflictCountry$country == "eSwatini"] <- "Swaziland"

Battles <- CountConflictCountry[ which(CountConflictCountry$event_type=='Battles'),]
RemoteViolence <- CountConflictCountry[ which(CountConflictCountry$event_type=='Explosions/Remote violence'),]
Protests <- CountConflictCountry[ which(CountConflictCountry$event_type=='Protests'),]
Riots <- CountConflictCountry[ which(CountConflictCountry$event_type=='Riots'),]
StrategicDev <- CountConflictCountry[ which(CountConflictCountry$event_type=='Strategic developments'),]
ViolenceCitizens <- CountConflictCountry[ which(CountConflictCountry$event_type=='Violence against civilians'),]
ArmedConflict<-
  
  #Fixing Names in the Africa attribute table#
  Africa3$COUNTRY <- as.character(Africa3$COUNTRY)
Africa3@data[Africa3@data$COUNTRY == "Cote d`Ivoire", "COUNTRY"] <- "Ivory Coast"

##############################
##Density / Hotspot Analysis##
ACLED_ptBACKUP<-ACLED_pt  ##ACLED_pt<-ACLED_ptBACKUP
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED_all, proj4string = projection_info) 


ACLED_Event_types <- c("Protests",
                       "Explosions/Remote violence", 
                       "Strategic developments",
                       "Battles",
                       "Violence against civilians",
                       "Riots")

File_names <- c("Protests",
                "Explosions Remote violence",
                "Strategic developments",
                "Battles",
                "Violence against civilians",
                "Riots")

for(j in length(unique(ACLED_pt$event_type))){
  j <-6
  ACLED_pt_loop <- subset(ACLED_pt, event_type==ACLED_Event_types[j]) #we just comment this out for the "All Conflict"
  
  ACLED_pt_loop
  summary(ACLED_pt_loop$event_type)
  
  #pixel size
  pixelsize = .5 
  
  #Africa bounding box / raster template
  box = round(extent(Africa3) / pixelsize) * pixelsize 
  template = raster(box, crs = projection_info,
                    nrows = (box@ymax - box@ymin) / pixelsize, 
                    ncols = (box@xmax - box@xmin) / pixelsize)
  
  ACLED_pt_loop$PRESENT <- 1
  #field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
  
  #subset for each year
  #setwd("/nfs/waterconflictafrica-data/HotspotRasters/AllConflict/") ##leave this in for "all"
  setwd(paste0('/nfs/waterconflictafrica-data/HotspotRasters/',File_names[j],'/'))
  
  heatVector <- list() #create an empty list
  countVector <- list() #create an empty list
  heatVectorSP <- list() #create an empty list
  countVectorSP <- list() #create an empty list
  
  years<-c(1997:2018)
  
  for (i in 1:length(years)) { 
    
    y <- years[i] 
    ACLED<-ACLED_pt_loop[ which(ACLED_pt_loop$year==y),]
    rasterACLED <- rasterize(ACLED, template, field = 'PRESENT', fun = sum) #allocates events to raster cell
    #rasterACLED2<-log(rasterACLED)
    kernel = focalWeight(rasterACLED, d = .01, type = 'Gauss') #hotpot analysis
    heat = focal(rasterACLED, kernel, fun = sum, na.rm=T) #hotspot analysis #note: also changed ACLED2 here
    
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
    
    heatVector[[i]]<-st_as_sf(Allevent_polygons2)
    countVector[[i]]<-st_as_sf(Allevent_polygons)
    
    heatVectorSP[[i]]<-Allevent_polygons2
    countVectorSP[[i]]<-Allevent_polygons
    
    st_write(heatVector[[i]], paste0('heat_event',j, '_', y, '.shp'))
    st_write(countVector[[i]], paste0('count_event',j, '_', y, '.shp'))
    
  }
}


counts_df <- as.data.frame(matrix(0,nrow = 52,ncol=length(years)))

for(i in 1:length(years)){
  names(counts_df)[i] <- paste0('count_',years[i])
}

#column names
for(i in 1:length(years)){
  names(counts_df)[i] <- paste0('count_',years[i])
  AfricaCount <- Africa
  trial3<-countVectorSP[[i]]
  trial4<-as.data.frame(sapply(over(Africa, geometry(trial3), returnList = TRUE), length))
  colnames(trial4)<- c("count")
  AfricaCount@data$countVectorSP<-trial4$count
  
  AfriDF<-data.frame(AfricaCount)
  
  trial6<-data.frame(tapply(AfriDF$countVectorSP, AfriDF$COUNTRY, sum))
  
  colnames(trial6)<- c( "counts")
  
  counts_df[,i] <- trial6$counts
}

#row names
rownames(counts_df) <- unique(AfriDF$COUNTRY)
means<-rowMeans(counts_df)
std_dev <- apply(counts_df,1,sd)
counts_df$CountryCountMean<-means
counts_df$CountryCountSD<-std_dev
counts_df$CountryCount2SD<-std_dev*2

counts_df$CountryCount_plus2SD<-(counts_df$CountryCountMean+counts_df$CountryCount2SD)
counts_df$CountryCount_minus2SD<-(counts_df$CountryCountMean-counts_df$CountryCount2SD)

counts_df$CountExceed <- rowSums(counts_df[,grep("^count_", names(counts_df))] > counts_df$CountryCount_plus2SD, na.rm=T)
counts_df$CountBelow <- rowSums(counts_df[,grep("^count_", names(counts_df))] < counts_df$CountryCount_minus2SD, na.rm=T)

write.csv(counts_df,file = paste0(File_names[j],'_counts_df.csv')) #for event types

counts_df$Country <- rownames(counts_df)

counts_dfLONG<- counts_df %>% gather(CountYear, Count, count_1997:count_2018)
counts_dfLONG$Class<-ifelse (counts_dfLONG$Count > counts_dfLONG$CountryCount_plus2SD, "1",
                             ifelse (counts_dfLONG$Count < counts_dfLONG$CountryCount_minus2SD, "-1", "0"))

write.csv(counts_dfLONG,file = paste0(File_names[j],'_countsLONG_df.csv')) #for event types

summary(ACLED_pt_loop$event_type)

###################################################################################
###########################################OLD#########Backup of original##########
##Density / Hotspot Analysis: All ACLED##
ACLED_ptBACKUP<-ACLED_pt  ##ACLED_pt<-ACLED_ptBACKUP
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED_merge, proj4string = projection_info) 

###Only run if not on ALL ACLED points#### ACLED_pt <- ACLED_pt[ which(ACLED_pt$event_type=="Riots"),]

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
setwd("/nfs/waterconflictafrica-data/HotspotRasters/AllConflict/") ##change this for the different types

heatVector <- list() #create an empty list
years<-c(1997:2018)

for (i in 1:length(years)) { 
  ##y <- years[i] 
  y<-1997
  ACLED<-ACLED_pt[ which(ACLED_pt$year==y),]
  rasterACLED <- rasterize(ACLED, template, field = 'PRESENT', fun = sum)
  ##rasterACLED2<-log(rasterACLED)
  kernel = focalWeight(rasterACLED, d = .01, type = 'Gauss')
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
#######Reading in the rasters#############
raster.files<-list.files(pattern=".tif") ##why use the .tif in the pattern recognition rather than part of the name?
raster.files

raster.filesALL<-unique(raster.files) ##what does the unique function do?

heat.raster<-lapply(raster.filesALL, raster)

heat.raster.stack<-stack(heat.raster)

plot(heat.raster.stack[[1]])
plot(Africa, add=TRUE)

#######Reading in the vectors###########
#Pixel counts per layer#
plot(heatVector[[4]])
plot(Africa, add=TRUE)

projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


#Put this in loop#
#####To be Fixed#####
AfricaCount<-Africa

trial<-heatVector[[1]]
trial2<-as.data.frame(sapply(over(Africa, geometry(trial), returnList = TRUE), length))
colnames(trial2)<- c("count")
AfricaCount$count1997<-trial2$count

trial3<-data.frame(AfricaCount)

trial4<-as.data.frame(tapply(trial3$count, trial3$COUNTRY, sum))
colnames(trial4)<- c( "counts")





