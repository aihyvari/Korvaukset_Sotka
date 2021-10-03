# Korvaukset_Sotka
Lääkekorvaukset kunnittain

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/SHAPforxgboost)](https://CRAN.R-project.org/package=SHAPforxgboost) [![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![](https://cranlogs.r-pkg.org/badges/SHAPforxgboost)](https://cran.r-project.org/package=SHAPforxgboost) [![](https://cranlogs.r-pkg.org/badges/grand-total/SHAPforxgboost?color=orange)](https://cran.r-project.org/package=SHAPforxgboost)

<!-- badges: end -->

Tässä on tarkoitus tutkia lääkekorvauksia.


## Esimerkki

**Summary plot**

```{r}
# run the model with built-in data, these codes can run directly if package installed  
library("SHAPforxgboost")
y_var <-  as.matrix(kuntadata_wide %>%
  select("Korvattujen lääkkeiden kustannukset, euroa / asukas"))
summary(y_var)

dtrain<-kuntadata_wide[,-1]
dtrain<-as.matrix(dtrain %>%
                    select(-colnames(y_var)))
dim(dtrain)

# hyperparameter tuning results
hparam <- list(objective = "reg:squarederror",  # For regression
                   eta = 0.02,
                   max_depth = 10,
                   gamma = 0.01,
                   subsample = 0.98,
                   colsample_bytree = 0.86)

XGmalli1 <- xgboost::xgboost(data = dtrain, label = as.matrix(dataXY_df[[y_var]]), 
                       params = hparam, nrounds = 200,
                       verbose = FALSE, 
                       early_stopping_rounds = 8)
                       
