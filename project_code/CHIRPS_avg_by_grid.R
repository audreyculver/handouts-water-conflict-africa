####################################################
####15 AUG Work for #################################
#####################################################

remove(list=ls())

library(raster)

setwd("/nfs/waterconflictafrica-data/CHIRPS monthly (new)/tif")

years <- 1996:2017 #these are the years that match the temp data
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

for(j in 1:length(years)){
  
  f<- list()
  for(i in 1:length(months)){
    f[i] <- paste0("chirps-v2.0.",years[j],".",months[i],".tif" )
  }
  
  ras <- lapply(f,raster) 
  STACK1 <- stack(ras) 
  
  mean_rainfall_raster <- calc(STACK1, fun = mean)
  f <- paste0('mean_rain_', years[j], '.tif')
  writeRaster(mean_rainfall_raster, filename=f,overwrite=TRUE)
  
  var_rainfall_raster <- calc(STACK1,fun=var)
  ff <-  paste0('var_rain_', years[j], '.tif')
  writeRaster(var_rainfall_raster, filename=ff,overwrite=TRUE)
  
}


f<- list()
for(i in 1:length(years)){#length(years)
  f[i] <- paste0('mean_rain_', years[i], '.tif')
}

ras <- lapply(f,raster) 
STACK_rain_mean <- stack(ras) 

save(STACK_rain_mean,file="mean_rainfall_1981_2017.RData")

f<- list()
for(i in 1:length(years)){#length(years)
  f[i] <- paste0('var_rain_', years[i], '.tif')
}