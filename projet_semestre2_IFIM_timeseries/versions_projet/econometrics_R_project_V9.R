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

## Récupération des données

gov_reciept <- read_excel("reciept.xlsx")
summary(gov_reciept)
View(gov_reciept)
dim(gov_reciept)

## Statistiques descriptives
gross_ts <- ts(data=gov_reciept$`Gross government investment`, start=c(1947), end=(2022), frequency=4)
autoplot(gross_ts, main="gross government investement in billion($) from 1947 to 2022")

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
acf(diff_premiere, main="ACF of the first difference of gross government investment") 
##on constate une décroissance forte et régulière, avec possiblement un effet de saisonalité. On peut donc penser qu'il
#s'agit d'un AR, il faudra quand même tester si'l y a présence ou non d'aspect saisonier.

#MA3 ou MA4

pacf(diff_premiere, main="PACF of the first difference of gross government investment", sub="2 bares out of the blue lines(q=2)")

#arma(2,4)
estimate(diff_premiere, p=2,d=0,q=4)
estimate(diff_premiere, p=4,d=0,q=4) #AIC -1239.034 modèle validé, meilleur car -1239.034<-1235, dernier MA non significatif
estimate(diff_premiere, p=5,d=0,q=4)
estimate(diff_premiere, p=4,d=0,q=3) #AIC -1235 plus grand
estimate(diff_premiere, p=4,d=0,q=5) #AIC -1235 plus grand


fit <- estimate(diff_premiere, p=5,d=0,q=3) ##le AR(4) n'est pas significatif à 5%, 

#AIC -1239.748 meilleure modèle
### 5,0,3 car AR(5) plus ou moins accepté à 10% et MA(3) 4.39e-01>0.05 donc rejetée
##résidus inde car dans l'inetravelle de confiance


##
## ligne rouge suit loi normale, les points sont collés à la droite dc àa suit plus ou moins à la loi normale
#AR 2

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
serie_seas <- isSeasonal(diff_premiere, test = "combined", freq = 4)
serie_seas
##Le test retourne un booléen FALSE, ce qui signifie que notre série ne présente pas d'effet saisonier. On peut donc estimer un modèle
##ARIMA de notre différence première maximisant les critères d'informations AIC et BIC 

##Approximation du meilleur modèle ARIMA

##on cherche un modèle qui cherche à minimiser le critère AIC. Pour cela, on va tester plusieurs modèles arima
## et regarder lequel de ces modèles est le meilleur
library(aTSA)


auto.arima(gross_ts)

#-0.3391/0.7440 = -0.45
arima(diff_premiere, order = c(2,1,9)) ##on test un modèle ARIMA tel que AR=2 et MA=3 #-1228.73
arima(diff_premiere, order = c(1,1,8)) ##on test un modèle ARIMA tel que AR=2 et MA=3 #-1228.73


auto.arima(diff_premiere) ##le modèle auto.arima généré par R propose un arima(2,2,1). Cependant, je ne prefère pas refaire une
##différence première car ma série se stationnarise déja en 1 différence première. En refaire une ferait que je perdrai de l'information
##je choisis donc le modèle ARIMA(2,1,8)

forecast_model_arima <- forecast(fit)


auto.arima(diff_premiere)
## Diagnostic des résidus
checkresiduals(fit)
min(fit$residuals)

##affichage des résidus
plot(forecast_model_arima$residuals)
print(forecast_model_arima$residuals)
##qqnorm des résidus
qqnorm(forecast_model_arima$residuals)

## // test d'indépendance des résidus \\

##acf
acf(forecast_model_arima$residuals)
pacf(forecast_model_arima$residuals)

Box.test(fit$residuals, lag=10, type=c("Ljung-Box")) ##p-value au dessus des 5%

## // test de l'homoscédasticité des résidus \\

library(lmtest)

m <- lm(residuals(fit) ~ 1)
bgtest(m) ## test de breusch godfrey

coeftest(fit) ##test de white
## // test de normalité des résidus \\

shapiro.test(fit$residuals) ##p-value = 9.8x10-10 < 5%, donc la p-value est très petite
##je peux donc en conclure que les résidus du modèle ne suivent pas une distribution normale

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

