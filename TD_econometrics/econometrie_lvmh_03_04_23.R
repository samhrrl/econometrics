rm(list = ls())
setwd("C:/Users/PC/Documents/econometrics/financiere/")


lvmh_actions = read.csv("lvmh_2000_from_now_on.csv")
df_lvmh = data.frame(lvmh_actions)

View(df_lvmh)
dim(df_lvmh) ##258 lignes, 7 colonnes
typeof(df_lvmh)

##statistiques descriptives
summary(df_lvmh)
head(df_lvmh)
class(df_lvmh)

##convertion des types du dataframe pour l'étude
str(df_lvmh)

df_lvmh$Date <- as.Date(df_lvmh$Date) #convertion en date la colonne Date du dataframe
suppressWarnings(df_lvmh$Open <- as.numeric(df_lvmh$Open))
suppressWarnings(df_lvmh$High <- as.numeric(df_lvmh$High))
suppressWarnings(df_lvmh$Low <- as.numeric(df_lvmh$Low))
suppressWarnings(df_lvmh$Close <- as.numeric(df_lvmh$Close))
suppressWarnings(df_lvmh$Adj.Close <- as.numeric(df_lvmh$Adj.Close))
suppressWarnings(df_lvmh$Volume <- as.numeric(df_lvmh$Volume))

##on vérifie que le type de chacune des colonnes de notre dataframe est bien casté

str(df_lvmh)

View(df_lvmh)

length(df_lvmh$Close)
##ajout d'une nouvelle colonne

df_lvmh$rendementsV2 <- 0
View(df_lvmh)


##Xt+1/xt de la valeur des rendements à la fermeture

for(x in 1:5959){
  df_lvmh$rendementsV2[x] = (df_lvmh$Close[x+1]/df_lvmh$Close[x])-1
}

View(df_lvmh)

View(df_lvmh)
##on constate bien que le format est correct, on va pouvoir opérer sur nos variables de notre dataframe
## et créer une nouvelle colonne rendement

df_lvmh_diff <- diff(log(df_lvmh$Adj.Close))
View(df_lvmh_diff)

plot(df_lvmh_diff, type="o", col="red", main="rendements ajusté à la fermeture de LVMH(2000-2023)", xlab="index", ylab="rendements")

##Test t.test

##nous ferons un t test afin de vérifier si la différence entre nos rendements sont significatives ou non
test <- t.test(df_lvmh_diff , mu=0)
test
##on a deux hypothèses: l'hypothèse h0: moyenne nulle et h1: moyenne non nulle
##Prenons un seuil à 5%, on voit que la p-value est de 0.75 > 5%. De plus, t < 1.96. Ainsi, la p-value est donc elevé on ne peut pas rejetter l'hypothèse que la moyenne des rendements est statistiquement nulle,

##Nous ferons ensuite un box test pour étudier la corrélation entre les rendements journaliers
Box.test (df_lvmh_diff, lag = 1, type = "Ljung")

##on constate que la p-value = 0.45 qui est supérieur au seuil des 5%, donc la corrélation entre les rendements journaliers est faible

##CC 2

##vérification à l'aide du test de shapiro que la distribution ne suit pas une loi normale

#install.packages("dplyr")
library("dplyr")
library("car")

##test de shapiro
length(df_lvmh_diff)

n<- df_lvmh_diff[1:5000]
shapiro.test(n)

## On a à disposition deux hypothèses: h0 la loi suit une loi normale et h1 la distribution ne suit pas la loi normale
##on constate que p-value < 5% <=> 2.2E(-16)<5%, donc on rejette l'hypothèse h0 qui dit que la loi suit une loi normale car
##la p-value n'est pas siginificative au seuil des 5%. Ainsi, j'en conclus que la distribution ne suit pas une loi normale


##on peut donc constater ceci graphiquement pour voir que ça ne suit pas une loi normale
qqnorm(n)
hist(n)

##on peut aussi calculer la skewness et la kurtosis

#install.packages("e1071")
library(e1071)
skewness(n, na.rm=TRUE) ##skewness égale à 0.20

kurtosis(n, na.rm=TRUE) #kurtosis = 5 c'est bien différent de 3

##On en conclus, que comme la sknewness et la kurtosis sont différents de  0 et  3, ça ne suit donc pas une loi normale

## Nous allons maintenant vérifier que la corrélation entre le carré des rendements est strictement positive.
##Pour cela, nous utiliserons le Boxplot de Ljung et tester

##Nous ferons ensuite un box test pour étudier la corrélation entre les rendements journaliers

library("ggpubr")

res <- cor.test(df_lvmh_diff^2,df_lvmh_diff^2,
                method = "kendall")
res

##Graphiquement, on voit que le carré des corrélations est supérieur à 0

acf(df_lvmh_diff^2, na.action = na.pass)


##l'hypothèse h0 est définie tel que les rendements des carrées sont distribués de manière indépendante
##l'hypothèse alternative h1 est les rendements des carrées ne sont pas distribués de manière indépendante, ils présentent
##une corrélation sérielle

## Estimation d'un modèle ARCH
install.packages("fGarch")
install.packages("FinTS")
install.packages("rugarch")
install.packages("tseries")

library(fGarch)
library(FinTS)
library(rugarch)
library(tseries)
library(dynlm)

bydArchTest <- ArchTest(df_lvmh_diff, lags=1, demean=TRUE)
bydArchTest

##

#install.packages(c("rugarch", "rmgarch"))
library(rugarch)
library(rmgarch)

lvmh_garch = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,0,0)),
                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                        distribution.model="norm", fixed.pars=list(omega=0))
lvmh_garch

arch_n <- cbind(df_lvmh_diff, na.rm = TRUE)
arch_n

arch_n_omit <- na.omit(arch_n)
View(arch_n)

for(i in 1:length(arch_n_omit)) {
  if (is.na(arch_n_omit[i])) {
    print('Missing')
  }
}

model <- ugarchfit(spec = lvmh_garch, data = arch_n_omit, rm.na=TRUE)
model

##on constate d'après les résultats qu'on a un modèle sGARCH(1,1) qui a pour moyenne un ARFIMA(1,0,1) qui suit une
##distribution normale
## on a donc un modèle ARCH(1), il faut regarder la partie auto-régressive
##on voit que la t-value est significatif pour les paramètres optimaux eet les érreurs robustes standart
##car pour les paramètres optimaux: t-value = 5.85>1.96
##robust standard errors: t-value = 180.106 >1.96


## on va choisir une deuxième colonne de l'action de lvmh comme par exemple Open

df_lvmh_diff_open <- diff(log(df_lvmh$Open))
View(df_lvmh_diff_open)


##vérification à l'aide du test de shapiro que la distribution ne suit pas une loi normale


##test de shapiro
length(df_lvmh$rendementsV2)

n_open<- df_lvmh$rendementsV2[1:5000]
shapiro.test(n_open)

## On a à disposition deux hypothèses: h0 la loi suit une loi normale et h1 la distribution ne suit pas la loi normale
##on constate que p-value < 5% <=> 2.2E(-16)<5%, donc on rejette l'hypothèse h0 qui dit que la loi suit une loi normale car
##la p-value n'est pas siginificative au seuil des 5%. Ainsi, j'en conclus que la distribution ne suit pas une loi normale

#regardons si la corrélation entre les rendements sont
res <- cor.test(df_lvmh$rendementsV2^2,df_lvmh$rendementsV2^2,
                method = "kendall")
res
##Graphiquement, on voit que le carré des corrélations est supérieur à 0

acf(df_lvmh$rendementsV2^2, na.action = na.pass)

##on peut donc constater ceci graphiquement pour voir que ça ne suit pas une loi normale
qqnorm(n_open)
hist(n_open)

##les résultats sont qualitativement pareils.

##volatilité
##on affiche le modèle prédit de la volatilté modélisé par la variance
plot(model@fit[["var"]], type="l", main="volatilité des rendememnts d'un ARCH(3)", col="blue")
print(model@fit[["var"]])


plot(model@fit[["var"]]*model@fit[["var"]], type="l", main="volatilité ht² des rendememnts d'un ARCH(3)", col="blue")
