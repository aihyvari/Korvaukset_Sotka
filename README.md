# Lääkekorvaukset kunnittain - Sotkanet

Tässä on tarkoitus tutkia lääkekorvauksia julkisella kuntakohtaisella aineistolla. <br>
[Sotkanet data portal]: https://sotkanet.fi/sotkanet/fi/index <br>
sotkanet R paketti:  Leo Lahti, Einari Happonen, Juuso Parkkinen, Joona Lehtomaki, Vesa Saaristo and Pyry
  Kantanen 2013-2021. sotkanet: Sotkanet Open Data Access and Analysis
  
## Esimerkki

**Datan valintaa ja hakeminen** <br>
Valitaan vuodet 2015-2017. Muistaakseni uudemmissa ei ole kaikkia tietoja.


```{r}
# Dataa eka.... 
library(sotkanet)
sotkanet.indicators <- SotkanetIndicators()
#Yhteensä on yli 3000 indikaattoria!

org<-unique(sotkanet.indicators$indicator.organization)
ongelmalliset <- c(1575, 1743, 1826, 1861, 1882, 1924, 1952, 2000, 
                   2001, 2033, 2050, 3386, 3443)
#VAIN KELA
kelaind<-sotkanet.indicators[sotkanet.indicators$indicator.organization.title=="Kansaneläkelaitos (Kela)",1]	
kelaind<-kelaind[!(kelaind %in% ongelmalliset)]

#HUOM: tää kestää kauan - kannattaa kirjoittaa data talteen, kun kerran hakenut!!
datlist <- list()
for (ind in kelaind) {
  datlist[[as.character(ind)]] <- GetDataSotkanet(indicators = ind, 
                                                  years = 2015:2017, genders = c('total')) #,
  #                                                 region.category = "KUNTA")
}
keladata <- do.call("rbind", datlist)
#write.csv(keladata, file="JonnekinJoku.csv", row.names=FALSE
```

**Datan esikäsittely** <br>
Tiputetaan ne, joissa paljon NA:ta. Luonteva imputointi olisi toki korvata puuttuvat hyvinvointialueen ka:lla <br>
Käytetään uusinta vuotta testiaineistona ja vanhempia opetusaineistona. <br>
Toki saman kunnan havainnot eri vuosilta ovat korreloituneet, mutta niputetaan ne silti aluksi opetusaineistoon

```{r}
#keladata<-read.csv("JokuJostakin.csv", header=TRUE)
kuntadata_wide<- keladata %>%
  filter(region.category=="KUNTA") %>%
  select(region.title.fi, indicator.title.fi, year, primary.value) %>% # , absolute.value)
  spread(indicator.title.fi,primary.value)
  
#Kelan osalta tosin ei tainnut olla...
  kuntadata_wide <- kuntadata_wide[,colSums(is.na(kuntadata_wide))< 0.1*nrow(kuntadata_wide)]
  
dtest<-kuntadata_wide[year==2017,]
dtrain<-kuntadata_wide[year!=2017,]
```

## XGBoost menetelmä
```{r}
library(xgboost)
library("SHAPforxgboost")
y_var <-  as.matrix(dtrain %>%
  select("Korvattujen lääkkeiden kustannukset, euroa / asukas"))
summary(y_var)

dtrain<-dtrain[,-1]
dtrain<-as.matrix(dtrain %>%
                    select(-colnames(y_var)))
dim(dtrain)

# Hyperparametrit
hparam <- list(objective = "reg:squarederror",  # Jatkuvan muuttujan regressio
                   eta = 0.02,                  #eta control the learning rate
                   max_depth = 10,
                   gamma = 0.01,                #minimum loss reduction required to make a further partition on a leaf node
                   subsample = 0.98,
                   colsample_bytree = 0.86)

XGmalli1 <- xgboost(data = dtrain, #SELITTÄJÄT
                 label =  y_var, #SELITETTÄVÄ
                 params = param_list, 
                 nrounds = 200, #boosting iterations
                 verbose = TRUE, 
                 nthread = parallel::detectCores() - 2,
                 early_stopping_rounds = 5)
```

## Support Vector Machine menetelmä
```{r}
```
## Tensorflow neuroverkko
```{r}
```
