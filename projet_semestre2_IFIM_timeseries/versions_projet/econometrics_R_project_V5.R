##Définition de l'environnement de travail

rm(list = ls())
setwd("C:/Users/PC/Documents/econometrics/")

##Installation des packages recquis

#install.packages("tseries")
#install.packages("readxl")
#install.packages("xts")
#install.packages("forecast", dependencies = TRUE) #pour la fonction auto.arima
library(forecast)
library("readxl")
library(tseries)
library(zoo)
library(ggplot2)
## Accès aux données

gov_reciept <- read_excel("reciept.xlsx")
summary(gov_reciept)
View(gov_reciept)
dim(gov_reciept)

## Statistiques descriptives
gross_ts <- ts(data=gross_gov, start=c(1947), end=(2022), frequency=4)
autoplot(gross_ts, title="gross government investement in billion($) from 1947 to 2022")

##Différence première pour stationnariser la série
diff_premiere = diff(log(gross_ts), differences=1)
plot(diff_premiere, type="l", col="red", main="first difference of gross government investement in billion($)", sub="figure 3.",xlab="years", ylab="first difference of gross investment")

##années 50 -> après guerre investissement état pour l'effort de reconstruction
## choc pétrolier 73-80
##biden président 2020-> monter car grosse dépense publics car politique keynésienne -> relancer économie par demande -> faire politique budgétaire expotienniste-> augmenetr dépense public


##La série semble être stationnaire d'un point de vue conjectural. Nous allons donc faire un test pour vérifier qu'elle soit bien
##stationnaire.

##test de Dickey-Füler

adf.test(diff_premiere) #test de statinnarité rejeté car p-value=0.01< 5%, donc l'hypothèse de stationnarité

## En réalisant un test de Dickey-Füler, on a deux hypothèses: h0: la série est stationnaire et he: la série est explosive donc pas stationnaire
## on constate qu'en réalisant un test avec l'hypothèse h0. le teste de stationnarité est 

## ACF et PACF
acf(diff_premiere) 
##on constate une décroissance forte et régulière, avec possiblement un effet de saisonalité. On peut donc penser qu'il
#s'agit d'un AR, il faudra quand même tester si'l y a présence ou non d'aspect saisonier.

pacf(diff_premiere, main="PACF of the first difference of gross government investment", sub="2 bares out of the blue lines(q=2)")
## on peut voir qu'il y a un effet sinusoïdale, donc on peut penser qu'il s'agit d'un modèle à moyenne mobile MA. En regardant les pics
## on peut observer qu'ils sont significatifs à k=2. On aurait donc un AR(2). Pour le modèle à moyenne mobile, on regardera dans l'ACF.
##On voit que sur l'ACF, la significativité de nos auto-corrélations s'expriment pour k=3 ou k=4. on a donc un MA(3) ou un MA(4).
## C'est ce dont nous testerons car il sera possible de construire un modèle ARIMA où notre partie auto-régressive nous fournira des renseignements
## sur les évenements passées. et la partie à moyenne mobile expliquera les chocs innatendues de notre modèle.

## Test de saisonalité

##src: https://rdrr.io/cran/seastests/man/isSeasonal.html
#install.packages("seastests")
library(seastests)
help(isSeasonal)
View(diff_premiere)
serie_seas <- isSeasonal(diff_premiere, test = "combined", freq = 12)
serie_seas
##Le test retourne un booléen FALSE, ce qui signifie que notre série ne présente pas d'effet saisonier. On peut donc estimer un modèle
##ARIMA de notre différence première maximisant les critères d'informations AIC et BIC 

##Approximation du meilleur modèle ARIMA

arima(diff_premiere, order = c(2,1,3)) ##on test un modèle ARIMA tel que AR=2 et MA=3
arima(diff_premiere, order = c(2,1,4)) ##on test un modèle ARIMA tel que AR=2 et MA=4
arima(diff_premiere, order = c(1,1,3)) ##on test un modèle ARIMA tel que AR=1 et MA=3

fit <- arima(diff_premiere, order = c(1,1,4)) ##on test un modèle ARIMA tel que AR=1 et MA=4
dim(fit)
forecast_model_arima <- forecast(fit)

## Diagnostic des résidus
checkresiduals(fit)

##affichage des résidus
plot(forecast_model_arima$residuals)
print(forecast_model_arima$residuals)
##qqnorm des résidus
qqnorm(forecast_model_arima$residuals)

##acf
acf(forecast_model_arima$residuals)
pacf(forecast_model_arima$residuals)

##retourne les prévisions réalisé par les différentes méthodes
accuracy(fit)
##augmentation des taux interets -> investissement diminue car taux trop cher les entreprises font moins de crédit
##quand taux interets baisse, entreprises ivnestissent car pas cher -> dynamiser offre
##l'inflation elevé-> banque centrale augmente taux pour baisser les prix-> moins ivnestissement des entreprises-> moins offre-> ménage consomment moins et plus épargner car c cher l'inflation et taux plus avantageux
## -> baisse de la demande --> baisse offre/demande pression sur les prix ->> entreprises revoient leur offre en baissant leur prix pour refouler -> diminuer l'inflation
##affichage des prévisions
##mandas président démocrate --> relancer par l'investissement(biden)

autoplot(forecast(fit))
autoplot(fit)
print(forecast(fit))
plot(forecast(fit))
