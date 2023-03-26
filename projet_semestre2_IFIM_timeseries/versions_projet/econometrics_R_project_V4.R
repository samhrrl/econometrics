##Définition de l'environnement de travail

rm(list = ls())
setwd("C:/Users/PC/Documents/econometrics/")

##Installation des packages recquis

#install.packages("tseries")
#install.packages("readxl")
#install.packages("xts")
#install.packages("forecast", dependencies = TRUE)#pour la fonction auto.arima
library(forecast)
library("readxl")
library(tseries)
library(zoo)

## Accès aux données

gov_reciept <- read_excel("reciept.xlsx")
summary(gov_reciept)
View(gov_reciept)
dim(gov_reciept)

serie <- ts(data=gov_reciept[,2])
summary(serie)

## Graphique représentant l'investissement du gouvernement depuis 1947 jusqu'à 2022
gross_gov <- gov_reciept$`Gross government investment`
plot(serie, gross_gov, col="red", main="gross government investement since 1947 to 2022 in billion($)", ylab="gross government investment", xlab="year")

##Différence première pour stationnariser la série
diff_premiere = diff(log(gross_gov), differences=1)
plot(diff_premiere, type="l", col="red", main="first difference of gross government investement in billion($)", sub="figure 3.",xlab="years", ylab="first difference of gross investment")

##test de Dickey-Füler


##test fuuler
adf.test(diff_premiere, alternative="stationary") #test de statinnarité rejeté car p-value=0.01< 5%, donc l'hypothèse de stationnarité

##Définition de l'environnement de travail

rm(list = ls())
setwd("C:/Users/PC/Documents/econometrics/")

##Installation des packages recquis

#install.packages("tseries")
#install.packages("readxl")
#install.packages("xts")
#install.packages("forecast", dependencies = TRUE)#pour la fonction auto.arima
library(forecast)
library("readxl")
library(tseries)
library(zoo)

## Accès aux données

gov_reciept <- read_excel("reciept.xlsx")
summary(gov_reciept)
View(gov_reciept)
dim(gov_reciept)

serie <- ts(data=gov_reciept[,2])
summary(serie)

## Graphique représentant l'investissement du gouvernement depuis 1947 jusqu'à 2022
gross_gov <- gov_reciept$`Gross government investment`
plot(serie, gross_gov, col="red", main="gross government investement since 1947 to 2022 in billion($)", ylab="gross government investment", xlab="year")

##on constate que la série est explosive, la stationnarité ne sera pas validé. inutile de faire un test de Dickey Füler.
##on va donc stationnariser la série


##Différence première pour stationnariser la série
diff_premiere = diff(log(gross_gov), differences=1)
plot(diff_premiere, type="l", col="red", main="first difference of gross government investement in billion($)", sub="figure 3.",xlab="years", ylab="first difference of gross investment")

##La série semble être stationnaire d'un point de vue conjectural. Nous allons donc faire un test pour vérifier qu'elle soit bien
##stationnaire.


##test de Dickey-Füler

adf.test(diff_premiere, alternative="stationary") #test de statinnarité rejeté car p-value=0.01< 5%, donc l'hypothèse de stationnarité

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
arima(diff_premiere, order = c(1,1,4)) ##on test un modèle ARIMA tel que AR=1 et MA=4

##on voit que le modèle maximisant notre critère d'information AIC est le modèle ARIMA(1,1,4)

##on peut faire une estimation automatique du modèle ARIMA pour vérifier notre résultat
auto.arima(diff_premiere)

