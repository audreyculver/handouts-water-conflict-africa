
mydata <- read.csv('TransactionLatLon.csv', na.strings =  c('NA', ""))
alldata <- read.csv( 'AllLM.csv', na.strings = c('NA', ""))

foodcrops <-filter(alldata,
                   str_detect(Intention.of.investment, 'Food crops')) %>%
  count(Target.country)%>% 
  sum(Deal.size) %>%
  group_by(Target.country)
library(dplyr)
data1 <- as.data.frame(alldata %>% group_by (Target.country) %>% count(Target.country, Deal.size))
data1 <- group_by (Target.country) %>% count(Target.country, Deal.size)
#foodcrops2 <- as.data.frame(alldata %>% count(Target.country))
#foodcrops3 <- as.data.frame (alldata %>% group_by(Target.country) %>% count(Target.country))
#foodcrops4 <- as.data.frame (alldata %>% sum(Deal.size))



