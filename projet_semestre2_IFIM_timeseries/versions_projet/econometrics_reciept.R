rm(list = ls())
setwd("C:/Users/PC/Documents/econometrics/")


install.packages("readxl")
library("readxl")
gov_reciept <- read_excel("reciept.xlsx")
summary(gov_reciept)
View( gov_reciept)

serie = ts(data=gov_reciept[,2])

serie_after_war = 

print(serie)



library(zoo)
methods(class="zoo")


##Net government saving


'-----------------------------------------------------------------'

##Gross government investment: investissement brut du gouvernement

##graphique de base
gross_gov <- gov_reciept$`Gross government investment`
plot(serie, gross_gov, col="blue", main="gross government investement since 1947 to 2022 in billion dollars")

##graphique avec log
plot(serie, log(gross_gov), col="blue", main="gross government investement since 1947 to 2022 in billion dollars")

##graphique avec la différence première


diff_premiere = diff(log(gross_gov), differences=2)

plot(diff_premiere, type="l", col="blue", main="gross government investement since 1947 to 2022 in billion dollars")


##test fuuler

adf.test(diff_premiere, alternative="stationary") #test de statinnarité rejeté car p-value=0.01< 5%, donc l'hypothèse de stationnarité


##test de PP
PP.test(diff_premiere, lshort = TRUE)

#plot(diff_seconde, type="l", col="blue")



#test acf

acf(diff_premiere) #on constate avec l'acf un effet saisonier qui se repète. ça ne peut donc être un autoregressif p=3
pacf(diff_premiere)


summary(gov_reciept$`Gross government investment`)

##ajotut de la différence première dans un dataframe

df_premiere_gross <- data.frame(diff(log(gross_gov)))
View(df_premiere_gross)



##test de stationnarité
#install.packages("tseries")
library(tseries)

adf.test(diff(log(gross_gov)), alternative="stationary") #test de statinnarité rejeté car p-value=0.01< 5%, donc l'hypothèse de stationnarité
#alternative h0 est rejeté









'-----------------------------------------------------------------'

##PRESTATIONS SOCIALES
##graphique de base
prestations_social_gouv <- gov_reciept$`Government social benefits`
plot(serie,prestations_social_gouv, type='l', xlab="Année",ylab="prestations sociales du gouvernement en billions $", col="blue", main="Evolution des prestations sociales du gouvernement entre 1947 et 2022 en billion de dollars")

##avec log
plot(serie,log(prestations_social_gouv), type='l', xlab="Année",ylab="prestations sociales du gouvernement en billions $", col="blue", main="Evolution des prestations sociales du gouvernement entre 1947 et 2022 en billion de dollars")

###avec diff
plot(diff(log(prestations_social_gouv), differences=1), type='l', xlab="Année",ylab="prestations sociales du gouvernement en billions $", col="blue", main="Evolution des prestations sociales du gouvernement entre 1947 et 2022 en billion de dollars")


'-----------------------------------------------------------------'


#taxes on production and imports

taxes_prod_import <- gov_reciept$`Taxes on production and imports`

#graphique
plot(serie, taxes_prod_import, type="l", col="blue", main="taxes on production and imports from 1947 to 2022 in billion$")


##graphique avec log diff
plot(diff(log(taxes_prod_import)), type="l", col="blue", main="taxes on production and imports from 1947 to 2022 in billion$")

#test de stationnarité

#hypothèse rejeté
adf.test(diff(log(taxes_prod_import)), alternative="stationary") #test de statinnarité rejeté car p-value=0.01< 5%, donc l'hypothèse de stationnarité

'-----------------------------------------------------------------'
