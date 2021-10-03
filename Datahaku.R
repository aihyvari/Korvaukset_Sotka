
# Dataa eka.... 
library(sotkanet)
sotkanet.indicators <- SotkanetIndicators()
#Yhteens‰ on yli 3000 indikaattoria!

org<-unique(sotkanet.indicators$indicator.organization)
ongelmalliset <- c(1575, 1743, 1826, 1861, 1882, 1924, 1952, 2000, 
                   2001, 2033, 2050, 3386, 3443)
#VAIN KELA
kelaind<-sotkanet.indicators[sotkanet.indicators$indicator.organization.title=="Kansanel‰kelaitos (Kela)",1]	
kelaind<-kelaind[!(kelaind %in% ongelmalliset)]

#HUOM: t‰‰ kest‰‰ kauan - kannattaa kirjoittaa data talteen, kun kerran hakenut!!
datlist <- list()
for (ind in kelaind) {
  datlist[[as.character(ind)]] <- GetDataSotkanet(indicators = ind, 
                                                  years = 2015:2018, genders = c('total')) #,
  #                                                 region.category = "KUNTA")
}
keladata <- do.call("rbind", datlist)
#write.csv(keladata, file="JonnekinJoku.csv", row.names=FALSE
############################################################################
setwd("D:/data/Sotka/T‰rke‰")
list.files()

keladata<-read.csv("Keladata.csv", header=TRUE)
kuntadata_wide<- keladata %>%
  filter(region.category=="KUNTA") %>%
  select(region.title.fi, indicator.title.fi, year, primary.value) %>% # , absolute.value)
  spread(indicator.title.fi,primary.value)

#Osassa indikaattoreita tiedot p‰ivittyy vasta ajan kuluessa. Voi olla puuttuvia, jos vuosi 2020, 2019, 2018,..
kuntadata_wide <- kuntadata_wide[,colSums(is.na(kuntadata_wide))< 0.1*nrow(kuntadata_wide)]

#L‰‰kekorvaus-muuttujat luonnollisesti t‰ysin korreloitunut tutkittavan kanssa - otetaan pois
kuntadata_wide<-kuntadata_wide %>%
  select(-starts_with("L‰‰kekorvaukset, "))

#Samoin n‰m‰ - erona asIAKAS vs asUKAS
kuntadata_wide<-kuntadata_wide %>%
  select(-"Korvattujen l‰‰kkeiden kustannukset, 1 000 euroa",
         -"Korvattujen l‰‰kkeiden kustannukset, euroa / asiakas")

dtest<-kuntadata_wide[kuntadata_wide$year==2018,]
dtrain<-kuntadata_wide[kuntadata_wide$year<2018,]
