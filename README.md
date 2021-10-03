# Korvaukset Sotkanet
Lääkekorvaukset kunnittain

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/SHAPforxgboost)](https://CRAN.R-project.org/package=SHAPforxgboost) [![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![](https://cranlogs.r-pkg.org/badges/SHAPforxgboost)](https://cran.r-project.org/package=SHAPforxgboost) [![](https://cranlogs.r-pkg.org/badges/grand-total/SHAPforxgboost?color=orange)](https://cran.r-project.org/package=SHAPforxgboost)

<!-- badges: end -->

Tässä on tarkoitus tutkia lääkekorvauksia julkisella kuntakohtaisella aineistolla.


## Esimerkki

**Datan valintaa ja hakeminen**

```{r}
# Dataa eka.... 
library(sotkanet)
sotkanet.indicators <- SotkanetIndicators()
org<-unique(sotkanet.indicators$indicator.organization)
```
## XGBoost menetelmä
```{r}
library(xgboost)
library("SHAPforxgboost")
y_var <-  as.matrix(kuntadata_wide %>%
  select("Korvattujen lääkkeiden kustannukset, euroa / asukas"))
summary(y_var)

dtrain<-kuntadata_wide[,-1]
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

##Support Vector Machine menetelmä
```{r}
```
## Tensorflow neuroverkko
