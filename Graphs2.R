library(dplyr)
library(tidyr)
library(stringr)
alldata <- read.csv( 'AllLM2.csv')

###################################################################3
removepounds <- function(dat, neworexist, dplyrfun, col) {
  result <- mutate(alldata,
         neworexist = dplyrfun(col, '#+'))
  return(result)
}
test2<- removepounds(alldata, alldata$Intention.of.investment, str_remove_all, alldata$Intention.of.investment)

############################################################################
#clean data

clean <-mutate(alldata,
                Intention.of.investment = str_remove_all(Intention.of.investment, '#+')) %>%
  mutate(Intention.of.investment = str_remove_all(Intention.of.investment, "\\d+")) %>%
  mutate(Intention.of.investment = str_remove_all(Intention.of.investment, "current")) %>%
  mutate(Negotiation.year =str_extract(Negotiation.status, "[\\d+]{1,4}")) %>%
  mutate(Negotiation.status =str_remove_all(Negotiation.status, "\\d+")) %>%
  mutate(Negotiation.status = str_remove_all(Negotiation.status, '#+')) %>%
  mutate(Intention.of.investment = str_remove_all(Intention.of.investment, "\\d+")) %>%
  mutate(Food.Crops = str_detect(Intention.of.investment, "food crops|Food crops") & !str_detect(Intention.of.investment, "fuels"))%>%
  mutate(Biofuels = str_detect(Intention.of.investment, "Biofuels|biofuels") & !str_detect(Intention.of.investment, "food|Food")) %>%
  mutate(Both = str_detect(Intention.of.investment, "Biofuels|biofuels") & str_detect(Intention.of.investment, "food|Food")) %>%
  mutate(Other = !str_detect(Intention.of.investment, "Biofuels|biofuels") & !str_detect(Intention.of.investment, "food|Food")) %>%
  mutate(Intention = str_c(Food.Crops,Biofuels, Both, Other))%>%
  mutate(Intention = str_replace(Intention, "TRUEFALSEFALSEFALSE","Food.crops"))%>%
  mutate(Intention = str_replace(Intention, "FALSETRUEFALSEFALSE","Biofuels"))%>%
  mutate(Intention = str_replace(Intention, "FALSEFALSETRUEFALSE","Food&Biofuels"))%>%
  mutate(Intention = str_replace(Intention, "FALSEFALSEFALSETRUE|FALSEFALSEFALSEFALSE","Other"))
CleanedData = subset(clean, select = -c(Intention.of.investment, Both, Biofuels,Food.Crops,Other))

selvar <- CleanedData[c(3,4,8,10:13, 21,22),]

#######################################################################################################
#Summary
library(ggplot2)
DealsPerCountry <- (clean %>% group_by(Target.country) %>% count(Target.country, Intention))
CountCountryType<-as.data.frame(CleanedData %>% count(Intention, Target.country))
#Dealsizes <- as.data.frame(tapply(CleanedData$Deal.size, CleanedData$Target.country, FUN=sum))
DealsPerCountry[is.na(DealsPerCountry)] <-0
Tidy_Deals <- pivot_wider(DealsPerCountry, 
                          names_from = Intention,
                          values_from = n)
Tidy_Deals[is.na(Tidy_Deals)] <- 0
Biofuelspc <- filter(DealsPerCountry, Intention == "Biofuels")
FoodCropspc <- filter(DealsPerCountry, Intention == "Food.crops")
Otherpc <- subset(DealsPerCountry, Intention == "Other")
Bothpc <- subset(DealsPerCountry, Intention == "Both")
Totalpc <- (clean %>% count(Target.country))



#Bioplot <- ggplot(Biofuelspc,
#       aes(x = Target.country, y = n)) + 
#  geom_bar(stat = 'identity') +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
#    face = 'bold', hjust = 0.5)) +
#  labs(title = "Biofuel Transactions", x = "Country", y = "Number of Deals")

#cropsplot <- ggplot(FoodCropspc,
#                  aes(x = Target.country, y = n)) + 
#  geom_bar(stat = 'identity') +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
#    face = 'bold', hjust = 0.5)) +
#  labs(title = "Food Crop Transactions", x = "Country", y = "Number of Deals")
#cropsplot

#otherplot <- ggplot(Otherpc,
#                   aes(x = Target.country, y = n)) + 
#  geom_bar(stat = 'identity') +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
#    face = 'bold', hjust = 0.5)) +
#  labs(title = "Other Transactions", x = "Country", y = "Number of Deals")

#bothplot <- ggplot(Bothpc,
#                    aes(x = Target.country, y = n)) + 
 # geom_bar(stat = 'identity') +
  #theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
   # face = 'bold', hjust = 0.5)) +
  #labs(title = "Both Transactions", x = "Country", y = "Number of Deals")
#bothplot

#alltran <- ggplot(Totalpc, aes(x = Target.country, y = n)) + 
#  geom_bar(stat = 'identity') +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
#  face = 'bold', hjust = 0.5)) +
#  labs(title = "All Transactions", x = "Country", y = "Number of Deals")
#alltran
dev.off

pdf("stacked_transactionbyintention.pdf", ggplot(DealsPerCountry, aes(fill=Intention, y= n, x=Target.country)) + 
  geom_bar(position="stack", stat="identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
    face = 'bold', hjust = 0.5)) +
  labs(title = "Transaction Intentions", x = "Country", y = "Number of Deals"))
dev.off()


pdf("Facetplot2.pdf", ggplot(DealsPerCountry,
       aes(x = Target.country, y = n)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(vars(Intention))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
    face = 'bold', hjust = 0.5)) +
  labs(title = "Frequency of Transactions", x = "Country", y = "Number of Deals"))
dev.off



############################################################################
Size <- 
  
Water <- filter(CleanedData, Water.extraction.envisaged == "Yes")
waterpts <- points(Water$Longitude, Water$Latitude, col = "red", cex = .4, pch=19) #draw a seq of points at the specified lat/lon
##Display X,Y Data 
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
water_pt <- SpatialPointsDataFrame(coords = coords, Water, proj4string = projection_info) 
plot(water_pt)
########################################################################
basins <- readOGR(".", "Afr_basins1")
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(basins)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(basins)
plot(water_pt, col = "blue", cex = .4, pch=19, add = TRUE)
extobasins <- extract(heat.raster.stack, basins[3,])
totaltran <- extract(heat.raster.stack, basins, fun = modal)

#what is the lc at the point?
#first argument is nldc - raster we are using
#second argument is the point, but we need to use a second argument, st_coordinates
#this coerices to a numeric matrix, for a single pt it needs to be numeric matrix
ex<- extract(nlcd, st_coordinates(sesync))
heat.raster<-lapply(raster.filesALL, raster)
heat.raster.stack<-stack(heat.raster)
writeRaster(heat.raster.stack,"stackedcon.tif", )
#this gives us 23, to find LC class of 23 use indexing
#use lc descriptive names we created earlier, but remember to add 1 bc indexing starts at 1
lc_types[sesync_lc + 1]

#summary of raster values from each polygon, what if we wanted modal value at each polygon
modal_lc <- extract(nlcd_agg,huc_md, fun = modal)

