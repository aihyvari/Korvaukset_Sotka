# Lääkekustannukset kunnittain 

Tässä on tarkoitus tutkia lääkekorvauksia julkisella kuntakohtaisella aineistolla. <br>
[Sotkanet data portal]: https://sotkanet.fi/sotkanet/fi/index <br>
sotkanet R paketti:  Leo Lahti, Einari Happonen, Juuso Parkkinen, Joona Lehtomaki, Vesa Saaristo and Pyry
  Kantanen 2013-2021. sotkanet: Sotkanet Open Data Access and Analysis
  
## Esimerkki: korvattujen lääkkeiden kustannus/ asukas <br>
Tämä on Kansaeläkelaitoksen tuottama indikaattori, numero 3225. Kustannus sisältää sekä potilaan maksaman osan että korvauksen. Lisäksi luku sisältää arvonlisäveron, joka on lääkkeillä 10 %.

```{r}
LK<-GetDataSotkanet(indicators = 3225, 
                    years = 2010:2020, genders = c('total'),
                    region.category = "ERVA")
LK<- LK %>%
  select(region.title.fi, indicator.title.fi, year, primary.value) 
```
Yksinkertainen kuva
```{r}
library(ggplot2)
ggplot(data=LK, aes(x=year, y=primary.value, group=region.title.fi)) +
  geom_line(aes(color=region.title.fi))+
  geom_point(aes(color=region.title.fi))+
 # theme(legend.position="bottom")+
  ggtitle("Kustannukset korvatuista (avo)lääkkeistä euroa/ asukas, 2010-2020")+
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1))
```
![alt text](https://github.com/aihyvari/Korvaukset_Sotka/blob/main/Kust_2010_2020.png?raw=true)

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
Muuta esivalintaa.....
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

#Lääkekorvaus-muuttujat luonnollisesti täysin korreloitunut tutkittavan kanssa - otetaan pois
kuntadata_wide<-kuntadata_wide %>%
                    select(-starts_with("Lääkekorvaukset, "))
#Samoin tämä - erona asIAKAS vs asUKAS
kuntadata_wide<-kuntadata_wide %>%
          select(-"Korvattujen lääkkeiden kustannukset, euroa / asiakas")
  
dtest<-kuntadata_wide[kuntadata_wide$year==2017,]
dtrain<-kuntadata_wide[kuntadata_wide$year!=2017,]
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
                   eta = 0.02,                  # learning rate
                   max_depth = 10,
                   gamma = 0.01,                #minimum loss reduction required to make a further partition on a leaf node
                   subsample = 0.98,
                   colsample_bytree = 0.86)

XGmalli1 <- xgboost(data = dtrain, #SELITTÄJÄT
                 label =  y_var, #SELITETTÄVÄ
                 params = param_list, 
                 nrounds = 200, #iteraatiot
                 verbose = TRUE, 
                 nthread = parallel::detectCores() - 2,
                 early_stopping_rounds = 5)
```
XGtuloksien tutkimista
```{r}
#yleinen merkitys
importance_matrix <- xgb.importance(model = malli1)
xgb.plot.importance(importance_matrix[1:10,], xlab = "Selittäjien merkitys mallissa")
test<-as.matrix(dtest[,colnames(dtrain)])
pred_xgb <- predict(malli1, test)
plot(dtest[,colnames(y_var)],
     pred_xgb,
     xlab = "Toteuma",
     ylab = "Ennuste",
     main = "XGboost malli")
abline(lm(pred_xgb ~ dtest[,colnames(y_var)]), col="red")
#####################################################
```
SHAP
```{r}
shap_values <- shap.values(xgb_model = malli1, X_train = dtrain)
shap_long <- shap.prep(xgb_model = malli1, X_train = dtrain)
shap.plot.summary(shap_long, x_bound  = 1.2, dilute = 10)
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)
shap.plot.force_plot_bygroup(plot_data)
```
## Support Vector Machine menetelmä
```{r}
```
## Tensorflow neuroverkko
```{r}
```
