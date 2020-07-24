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
  mutate(Intention = str_replace(Intention, "FALSEFALSETRUEFALSE","Both"))%>%
  mutate(Intention = str_replace(Intention, "FALSEFALSEFALSETRUE|FALSEFALSEFALSEFALSE","Other"))
CleanedData = subset(clean, select = -c(Intention.of.investment, Both, Biofuels,Food.Crops,Other))

#######################################################################################################
#Summary
library(ggplot2)
DealsPerCountry <- (clean %>% group_by(Target.country) %>% count(Target.country, Intention))
CountCountryType<-as.data.frame(CleanedData %>% count(Intention, Target.country))
Dealsizes <- as.data.frame(tapply(CleanedData$Deal.size, CleanedData$Target.country, FUN=sum))
Dealsizes <- Dealsizes/1000
?tapply
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

pdf("LSLTbyIntetion.pdf", ggplot(DealsPerCountry, aes(fill=Intention, y= n, x=Target.country)) + 
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

ggplot(DealsPerCountry,
       aes(x = Target.country, y = n)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(vars(Intention))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(
    face = 'bold', hjust = 0.5)) +
  labs(title = "Frequency of Transactions", x = "Country", y = "Number of Deals")