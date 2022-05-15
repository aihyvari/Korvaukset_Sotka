# Lääkekustannukset kuntadatassa

Tässä on tarkoitus tutkia lääkekorvauksia julkisella kuntakohtaisella indikaattoriaineistolla. <br>
Data on saatavilla R paketin Sotkanet avulla:
[Sotkanet data portal]: https://sotkanet.fi/sotkanet/fi/index <br>
sotkanet R paketti:  Leo Lahti, Einari Happonen, Juuso Parkkinen, Joona Lehtomaki, Vesa Saaristo and Pyry
  Kantanen 2013-2021. sotkanet: Sotkanet Open Data Access and Analysis
  
## Esimerkki: korvattujen lääkkeiden kustannus/ asukas <br>
Kyseessä Kansaeläkelaitoksen tuottama indikaattori, jonka numero Sotkanetissa on 3225. <br>
Kustannus sisältää sekä potilaan maksaman osan että korvauksen. Lisäksi luku sisältää arvonlisäveron, joka on lääkkeillä 10 %.<br>

Ladataan paketit ja haetaan kyseinen indikaattridata:
```{r}
library(tidyverse)
library(sotkanet)

LK<-GetDataSotkanet(indicators = 3225, 
                    years = 2010:2021, genders = c('total'),
                    region.category = "ERVA")
LK<- LK %>%
  select(region.title.fi, indicator.title.fi, year, primary.value) 
LK$year<-as.character(LK$year)

```
Yksinkertainen kuva, josta nähdään kasvua kustannuksissa erityisesti vuoden 2017 jälkeen.<br>
Kuvassa ERVA-alueet omina käyrinään.
```{r}
#Kuva 2010-2021
library(ggplot2)
ggplot(data=LK, aes(x=year, y=primary.value, group=region.title.fi)) +
  geom_line(aes(color=region.title.fi))+
  geom_point(aes(color=region.title.fi))+
  theme(axis.title.y=element_blank()) +
  theme_bw()+
  labs(color='ERVA-alue')+
  ggtitle("Kustannukset korvatuista (avo)lääkkeistä euroa/ asukas, 2010-2021")+
  xlab("Vuosi") + ylab("euroa/ asukas")
```
![alt text](https://github.com/aihyvari/Korvaukset_Sotka/blob/main/Avo_Korv_2010_2021.png)

**Datan valintaa ja hakeminen** <br>
Valitaan vuodet 2015-2021. Muistaakseni uudemmissa puuttuu vielä useita tietoja mm. sairastavuusindeksit.<br>
Tässä poimitaan kaikki Kelan, Tilastokeskuksen, Eläketurvakeskuksen ja Työ- ja elinkeinoministeriön (TEM) tuottamat indikaattorit. <br>
THL indikaattoreissa on epäilemättä kiinnostavia, mutta vaatisi hiukan syventymistä poimia halutut.


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
                                                  years = 2015:2018, genders = c('total')) #,
  #                                                 region.category = "KUNTA")
}
keladata <- do.call("rbind", datlist)
#write.csv(keladata, file="JonnekinJoku.csv", row.names=FALSE
```

**Datan esikäsittely** <br>
Tiputetaan ne, joissa paljon NA:ta. Luonteva imputointi olisi toki korvata puuttuvat hyvinvointialueen ka:lla <br>
Kun kiinnostuksen kohteena on korvattujen lääkkeiden kustannukset/ asukas, on osa indikaattoreista ilmiselvästi korreloituneita tutkittavan kanssa. <br>
Tällaisia ovat mm. lääkekorvaukset ja korvattujen lääkkeiden kustannukset/ asiakas. Näiden käyttö selittäjinä ei kuitenkaan ole kiinnostavaa ja tiputetaan ne pois. <br>
<br>
Tässä tehdään train-test jako hiukan eri tavalla kuin yleensä:  <br>
käytetään uusinta vuotta 2021 testiaineistona ja vanhempia 2015-2019 opetusaineistona. <br>
Vanhempien vuosien (-2017) aineistoilla opetetun mallin ennustekyvylle on haasteena aiemmin kuvattu kustannusten kasvu 2017 jälkeen, mikä ei osu opetusaineistoon. <br>
Saman kunnan havainnot eri vuosilta ovat toki korreloituneet, mutta niputetaan ne silti aluksi opetusaineistoon ikään kuin olisivat itsenäisiä riippumattomia havaintoja.

```{r}
#keladata<-read.csv("JokuJostakin.csv", header=TRUE)
kuntadata_wide<- keladata %>%
  filter(region.category=="KUNTA") %>%
  select(region.title.fi, indicator.title.fi, year, primary.value) %>% # , absolute.value)
  spread(indicator.title.fi,primary.value)
  
#Osassa indikaattoreita tiedot päivittyy vasta ajan kuluessa. Voi olla puuttuvia, jos vuosi 2020, 2019, 2018,..
kuntadata_wide <- kuntadata_wide[,colSums(is.na(kuntadata_wide))< 0.1*nrow(kuntadata_wide)]

#Lääkekorvaus-muuttujat luonnollisesti täysin korreloitunut tutkittavan kanssa - otetaan pois
kuntadata_wide<-kuntadata_wide %>%
                    select(-starts_with("Lääkekorvaukset, "))
                    
#Samoin nämä - erona asIAKAS vs asUKAS
kuntadata_wide<-kuntadata_wide %>%
          select(-"Korvattujen lääkkeiden kustannukset, 1 000 euroa",
                 -"Korvattujen lääkkeiden kustannukset, euroa / asiakas")
  
dtest<-kuntadata_wide[kuntadata_wide$year==2018,]
dtrain<-kuntadata_wide[kuntadata_wide$year<2018,]
```

## XGBoost menetelmä
Tianqi Chen ym. Extreme Gradient Boosting https://github.com/dmlc/xgboost <br>
XGBoost = eXtreme Gradient Boosting on ollut suosittu algoritmi koneoppimiskilpailuissa. <br>
Sen avulla saadut tulokset ovat olleet kilpailukykyisiä vertailussa syviin neuroverkkoihin.
<br>
HUOM: mallia ei ole viimeisen päälle tuunattu. <br>
Kuitenkin havaitaan, että test RMSE säilyy noin ~50 €/ asukas, vaikka hyperparametrien arvoja vaihdellaan. <br>
Sen sijaan opetusaineiston RMSE:n saa hyvinkin pieneksi ylisovittamalla (enemmän?). <br>


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
                   max_depth = 10,              #Pienennä??
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
                 
xtrain<-xgb.DMatrix(dtrain, label= y_var)
col<-grep("Korvattujen lääkkeiden kustannukset, euroa / asukas", colnames(dtest))
xtest<-xgb.DMatrix(as.matrix(dtest[,-c(1,col)]), 
                   label= as.matrix(dtest[,col]))
watchlist = list(train=xtrain, test=xtest)
tmp = xgb.train(data = xgb.DMatrix(data = dtrain, label= y_var), 
                params = param_list, watchlist=watchlist, nrounds = 200)

###############################################################
```
Viimeiset iteraatiot: <br>
[197]	train-rmse:12.144533	test-rmse:38.810844 <br>
[198]	train-rmse:11.964718	test-rmse:38.719948 <br>
[199]	train-rmse:11.783748	test-rmse:38.604141 <br>
[200]	train-rmse:11.614498	test-rmse:38.497894 <br>


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
![alt text](https://github.com/aihyvari/Korvaukset_Sotka/blob/main/EnnVStot.png?raw=true)

SHAP
SHAP arvot on uusi mittari kuvaamaan eri selittäjien merkitystä mallissa. SHAP arvo saadaan erikseen kullekin havainnolle - tässä kunnalle. <br>
Tietty selittäjä voi keskimäärin olla keskimäärin vähämerkityksinen, mutta silti tärkeä joillekin havainnoille. <br>
S. Lundberg ym. Nature Machine Intelligence volume 2, pages56–67 (2020) https://www.nature.com/articles/s42256-019-0138-9

```{r}
shap_values <- shap.values(xgb_model = malli1, X_train = dtrain)
shap_long <- shap.prep(xgb_model = malli1, X_train = dtrain)
shap.plot.summary(shap_long, x_bound  = 1.2, dilute = 10)
plot_data <- shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)
shap.plot.force_plot_bygroup(plot_data)
```
![alt text](https://github.com/aihyvari/Korvaukset_Sotka/blob/main/SHAPdep.png?raw=true) <br>
SHAP summary kuvasta ilmenee tärkeä seikka, kun tulkitaan esimerkin lukuisia selittäjiä sisältäviä malleja: <br>
jos >100 selittäjää simuloitaisiin satunnaislukuina, vaikuttaisivat jotkin selittäjistä silti hyviltä. <br>
Esimerkkimalliin nousee myös mukaan sellaisia, joilla on vaikea ajatella yhteyttä lääkekustannuksiin. <br>
Nämä voidaan toki poistaa tutkijan harkinnan perusteella - sen sijaan, ei voida varmistaa, johtuuko ajateltavissa oleva "järkevä" yhteys sattumasta vai onko se kausaalinen.
```{r}
shap.plot.summary.wrap2(shap_values$shap_score, dtrain, top_n=20)
```
![alt text](https://github.com/aihyvari/Korvaukset_Sotka/blob/main/SHAP.png?raw=true)
## Support Vector Machine menetelmä
Jos muillakin menetelmillä saisi samansuuntaisia tuloksia, voisi olla luottavaisempi niiden suhteen.
```{r}
```

