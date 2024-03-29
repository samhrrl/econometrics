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

##convertion des types du dataframe pour l'�tude
str(df_lvmh)

df_lvmh$Date <- as.Date(df_lvmh$Date) #convertion en date la colonne Date du dataframe
suppressWarnings(df_lvmh$Open <- as.numeric(df_lvmh$Open))
suppressWarnings(df_lvmh$High <- as.numeric(df_lvmh$High))
suppressWarnings(df_lvmh$Low <- as.numeric(df_lvmh$Low))
suppressWarnings(df_lvmh$Close <- as.numeric(df_lvmh$Close))
suppressWarnings(df_lvmh$Adj.Close <- as.numeric(df_lvmh$Adj.Close))
suppressWarnings(df_lvmh$Volume <- as.numeric(df_lvmh$Volume))

##on v�rifie que le type de chacune des colonnes de notre dataframe est bien cast�

str(df_lvmh)

View(df_lvmh)

length(df_lvmh$Close)
##ajout d'une nouvelle colonne

df_lvmh$rendementsV2 <- 0
View(df_lvmh)


##Xt+1/xt de la valeur des rendements � la fermeture

for(x in 1:5959){
  df_lvmh$rendementsV2[x] = (df_lvmh$Close[x+1]/df_lvmh$Close[x])-1
}

View(df_lvmh)

View(df_lvmh)
##on constate bien que le format est correct, on va pouvoir op�rer sur nos variables de notre dataframe
## et cr�er une nouvelle colonne rendement

df_lvmh_diff <- diff(log(df_lvmh$Adj.Close))
View(df_lvmh_diff)

plot(df_lvmh_diff, type="o", col="red", main="rendements ajust� � la fermeture de LVMH(2000-2023)", xlab="index", ylab="rendements")

##Test t.test

##nous ferons un t test afin de v�rifier si la diff�rence entre nos rendements sont significatives ou non
test <- t.test(df_lvmh_diff , mu=0)
test
##on a deux hypoth�ses: l'hypoth�se h0: moyenne nulle et h1: moyenne non nulle
##Prenons un seuil � 5%, on voit que la p-value est de 0.75 > 5%. De plus, t < 1.96. Ainsi, la p-value est donc elev� on ne peut pas rejetter l'hypoth�se que la moyenne des rendements est statistiquement nulle,

##Nous ferons ensuite un box test pour �tudier la corr�lation entre les rendements journaliers
Box.test (df_lvmh_diff, lag = 1, type = "Ljung")

##on constate que la p-value = 0.45 qui est sup�rieur au seuil des 5%, donc la corr�lation entre les rendements journaliers est faible

##CC 2

##v�rification � l'aide du test de shapiro que la distribution ne suit pas une loi normale

#install.packages("dplyr")
library("dplyr")
library("car")

##test de shapiro
length(df_lvmh_diff)

n<- df_lvmh_diff[1:5000]
shapiro.test(n)

## On a � disposition deux hypoth�ses: h0 la loi suit une loi normale et h1 la distribution ne suit pas la loi normale
##on constate que p-value < 5% <=> 2.2E(-16)<5%, donc on rejette l'hypoth�se h0 qui dit que la loi suit une loi normale car
##la p-value n'est pas siginificative au seuil des 5%. Ainsi, j'en conclus que la distribution ne suit pas une loi normale


##on peut donc constater ceci graphiquement pour voir que �a ne suit pas une loi normale
qqnorm(n)
hist(n)

##on peut aussi calculer la skewness et la kurtosis

#install.packages("e1071")
library(e1071)
skewness(n, na.rm=TRUE) ##skewness �gale � 0.20

kurtosis(n, na.rm=TRUE) #kurtosis = 5 c'est bien diff�rent de 3

##On en conclus, que comme la sknewness et la kurtosis sont diff�rents de  0 et  3, �a ne suit donc pas une loi normale

## Nous allons maintenant v�rifier que la corr�lation entre le carr� des rendements est strictement positive.
##Pour cela, nous utiliserons le Boxplot de Ljung et tester

##Nous ferons ensuite un box test pour �tudier la corr�lation entre les rendements journaliers

library("ggpubr")

res <- cor.test(df_lvmh_diff^2,df_lvmh_diff^2,
                method = "kendall")
res

##Graphiquement, on voit que le carr� des corr�lations est sup�rieur � 0

acf(df_lvmh_diff^2, na.action = na.pass)


##l'hypoth�se h0 est d�finie tel que les rendements des carr�es sont distribu�s de mani�re ind�pendante
##l'hypoth�se alternative h1 est les rendements des carr�es ne sont pas distribu�s de mani�re ind�pendante, ils pr�sentent
##une corr�lation s�rielle

## Estimation d'un mod�le ARCH
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

##on constate d'apr�s les r�sultats qu'on a un mod�le sGARCH(1,1) qui a pour moyenne un ARFIMA(1,0,1) qui suit une
##distribution normale
## on a donc un mod�le ARCH(1), il faut regarder la partie auto-r�gressive
##on voit que la t-value est significatif pour les param�tres optimaux eet les �rreurs robustes standart
##car pour les param�tres optimaux: t-value = 5.85>1.96
##robust standard errors: t-value = 180.106 >1.96


## on va choisir une deuxi�me colonne de l'action de lvmh comme par exemple Open

df_lvmh_diff_open <- diff(log(df_lvmh$Open))
View(df_lvmh_diff_open)


##v�rification � l'aide du test de shapiro que la distribution ne suit pas une loi normale


##test de shapiro
length(df_lvmh$rendementsV2)

n_open<- df_lvmh$rendementsV2[1:5000]
shapiro.test(n_open)

## On a � disposition deux hypoth�ses: h0 la loi suit une loi normale et h1 la distribution ne suit pas la loi normale
##on constate que p-value < 5% <=> 2.2E(-16)<5%, donc on rejette l'hypoth�se h0 qui dit que la loi suit une loi normale car
##la p-value n'est pas siginificative au seuil des 5%. Ainsi, j'en conclus que la distribution ne suit pas une loi normale

#regardons si la corr�lation entre les rendements sont
res <- cor.test(df_lvmh$rendementsV2^2,df_lvmh$rendementsV2^2,
                method = "kendall")
res
##Graphiquement, on voit que le carr� des corr�lations est sup�rieur � 0

acf(df_lvmh$rendementsV2^2, na.action = na.pass)

##on peut donc constater ceci graphiquement pour voir que �a ne suit pas une loi normale
qqnorm(n_open)
hist(n_open)

##les r�sultats sont qualitativement pareils.

##volatilit�
##on affiche le mod�le pr�dit de la volatilt� mod�lis� par la variance
plot(model@fit[["var"]], type="l", main="volatilit� des rendememnts d'un ARCH(3)", col="blue")
print(model@fit[["var"]])


plot(model@fit[["var"]]*model@fit[["var"]], type="l", main="volatilit� ht� des rendememnts d'un ARCH(3)", col="blue")
