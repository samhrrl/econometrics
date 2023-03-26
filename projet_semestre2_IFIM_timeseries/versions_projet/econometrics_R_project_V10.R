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

pacf(diff_premiere, main="PACF of the first difference of gross government investment", sub="2 bares out of the blue lines(q=2)")

## on peut voir qu'il y a un effet sinusoïdale, donc on peut penser qu'il s'agit d'un modèle à moyenne mobile MA. En regardant les pics
## on peut observer qu'ils sont significatifs à k=2. On aurait donc un AR(2). Pour le modèle à moyenne mobile, on regardera dans l'ACF.
##On voit que sur l'ACF, la significativité de nos auto-corrélations s'expriment pour k=3 ou k=4. on a donc un MA(3) ou un MA(4).
## C'est ce dont nous testerons car il sera possible de construire un modèle ARIMA où notre partie auto-régressive nous fournira des renseignements
## sur les évenements passées. et la partie à moyenne mobile expliquera les chocs innatendues de notre modèle.


## Test de saisonalité

##src: https://rdrr.io/cran/seastests/man/isSeasonal.html
#install.packages("seastests")


##on fait rapidement un test de saisonnalité pour voir si y a une effet saisonier dans notre modèle

library(seastests)
help(isSeasonal)
View(diff_premiere)
serie_seas <- isSeasonal(diff_premiere, test = "combined", freq = 4)
serie_seas

##le test retourne FALSe, il y a donc à priori aucun effet de saisonnalité.


##Estimation du meilleur modèle ARIMA
library(aTSA)
##test du premier candidat-> ARMA(2,4)
estimate(diff_premiere, p=2,d=0,q=4)
##les t-values ne sont pas significatives, de même pour les p-value, on rejete ce modèle, on va augmenter p et q

##test du second candidat-> ARMA(3,4)
estimate(diff_premiere, p=3,d=0,q=4) ##problème quand j'ai p=3, je vais donc passer passer à p=4

##test du troisième candidat-> ARMA(4,4)
estimate(diff_premiere, p=4,d=0,q=4)
#les t-values sont toutes significatives > 1.96 en valeur absolue, sauf pour le AR(4), sa p-value est au dessus du seuil des 5%
##mais peut être accepté si l'on se place au seuil des 10%, de même pour le Ma(4), sa p-value dépasse le seuil des 5%, 
##le critère AIC=-1239.034, essayons de trouver un modèle qui minimise ce critère AIC.

##test du quatrième candidat-> ARMA(5,4)
estimate(diff_premiere, p=5,d=0,q=4)
##toutes les t-values sont significatives sauf pour le Ma(4) et le AR(4)<1.96, si l'on s'interesse aux p-values
##le AR(3) est acceptable au seuil des 10%, le AR(4) est totalement rejeté, le AR(5) est acceptable au seuil des 10%, et le MA(4) est totalement
##rejeté. son critère AIC=-1239.922, ce modèle est moins bon que le second candidat car il admet plusieurs valeurs non significatives et son critère d'information
##AIC ne minimise pas.


##test du cinquième candidat-> ARMA(4,3)
estimate(diff_premiere, p=4,d=0,q=3)
##Les t-value pour ce modèle ne sont pas significatives pour le Ar(1), AR(3), MA(1) et MA(3)
## si l'on regarde les p-value du modèle, le AR(2) est totalement rejeté car non significative, de même pour le AR(3), le AR(4) aussi,
##le MA(1) et ke MA(3). Le modèle est donc rejeté, le critère d'information ne minimise absolument pas le modèle si l'on se refère au modèle précedent

##test du sixième candidat-> ARMA(4,5)
estimate(diff_premiere, p=4,d=0,q=5)
##en testant de modèle on constate que les t-value sont toutes significatives sauf pour le AR(4) et le MA(4).
## En regardant les p-value, elles sont plus ou moins toutes significatives sauf pour le AR(4), le MA(4) et le MA(5)
## Enfin, si l'on regarde le critère d'information AIC du modèle il vaut -1238.078. il minimise pas celui des candidats précdent, on rejete ce modèle


##test du septième candidat -> ARMA(5,3)
fit <- estimate(diff_premiere, p=5,d=0,q=3) ##le AR(4) n'est pas significatif à 5%, 
##en regardant le modèle, on voit que les t-values sont toutes siginificatives sauf pour le AR(4) qui ne l'est pas au seuil des 5%
## pour les p-value, le AR(4) n'est pas accepté au seuil des 5%, le AR(5) de même, sonc crit-re AIC=-1239.748. Il est nettement supérieur aux candidats précdennt
##je retiens donc ce modèle car  c'est celui qui minimise au mieux le critère d'information AIC et ses valeurs pour les t-value et p-value sont plus ou moins toutes significatives au seuil de 5%

## si l'on fait rapidement un diagonistic de nos résidus, ils sont dans l'intervalle de confiance, la ligne rouge suit à priori une loi normale
## et on voit que nos résidus sont proche de la droite donc on peut penser qu'ils suivent eux aussi une distribution normale(il faudra tester)

auto.arima(diff_premiere) ##le modèle auto.arima généré par R propose un arima(2,2,1). Cependant, je ne prefère pas refaire une
##différence première car ma série se stationnarise déja en 1 différence première. En refaire une ferait que je perdrai de l'information
##je choisis donc le modèle ARIMA(2,1,8)

## Diagnostic des résidus
checkresiduals(fit)
min(fit$residuals)


## Test d'indépendance des résidus

acf(fit$residuals)
pacf(fit$residuals)
Box.test(fit$residuals, lag=10, type=c("Ljung-Box")) 

##Nous avons deux hypothèses en réalisant le test de Ljung-Box:

##H0: la corrélation entre résidus est très faible(donc il y a indépendance)
##He: la corrélation entre les résidus est significativement différente de 0(donc il y a pas d'indépendance)

##on constate que la p-value = 0.9625 est supérieur au seuil des 5%, j'en conclus que je peux rejeté l'hypothèse alter que la corrélation entre les résidus
##est signficativement différente de 0, donc nos résidus sont indépendants.


## Test de l'homoscédasticité des résidus

library(lmtest)

m <- lm(residuals(fit) ~ 1)
bgtest(m) ## test de breusch godfrey

##Nous avons deux hypothèses pour le test de White

##h0:  Nous sommes dans un cas d'homoscédasticité, c'est à dire que la variance est constante
##he:  Nous sommes dans un cas d'héteroscédasticité, c'est à dire que la variance varie!

##en réalisant le test, on constate que nous avons une p-value = 0.9018 supérieur au 5%, c'est à dire qu'on ne rejete pas l'hypothèse h0,
##mais qu'on rejete l'hypothèse alternative. Donc j'en conclus, que notre modèle admet un cas d'homoscédasticité, donc que la variance est constante


##Test de normalité des résidus

shapiro.test(fit$residuals) 

##Le test de shapiro permet de vérifier si nos résidus suivent une distribution normale

##Nous pouvons voir que la pèvalue = 6.157x10E(-11), qui est inférieur au seuil des 5%, j'en conclus que mes résidus ne suivent pas une
##distribution normale
##p-value = 9.8x10-10 < 5%, donc la p-value est très petite, ceci n'est pas très grave.

##Prévision du modèle

##Nous avons estimé un modèle ARIMA(5,0,3), et réalisé un diagnostic de nos résidus, les tests statistiques se sont averés cohérents,
## car l'hypothèse de normalité, homoscédasticité, indépendance des rédidus sont acceptés. Ainsi, nos prévisions du modèles ne seront pas erronés 
##On peut donc construire ce modèle et prédire les valeurs pour l'année 2023.

forecast_model_arima <- forecast(fit)


autoplot(forecast(fit))
autoplot(fit)

print(forecast(fit))
plot(forecast_model_arima)









##retourne les prévisions réalisé par les différentes méthodes
accuracy(fit)
##augmentation des taux interets -> investissement diminue car taux trop cher les entreprises font moins de crédit
##quand taux interets baisse, entreprises ivnestissent car pas cher -> dynamiser offre
##l'inflation elevé-> banque centrale augmente taux pour baisser les prix-> moins ivnestissement des entreprises-> moins offre-> ménage consomment moins et plus épargner car c cher l'inflation et taux plus avantageux
## -> baisse de la demande --> baisse offre/demande pression sur les prix ->> entreprises revoient leur offre en baissant leur prix pour refouler -> diminuer l'inflation
##affichage des prévisions
##mandas président démocrate --> relancer par l'investissement(biden)



