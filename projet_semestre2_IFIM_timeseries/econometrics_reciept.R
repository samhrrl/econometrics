rm(list = ls())
setwd("C:/Users/PC/Documents/econometrics/")


install.packages("readxl")
library("readxl")
gov_reciept <- read_excel("reciept.xlsx")
summary(gov_reciept)

serie = ts(data=gov_reciept[,2])

serie

prestations_social_gouv <- gov_reciept$`Government social benefits`

library(zoo)
methods(class="zoo")
plot(serie,prestations_social_gouv, type='l', xlab="Année",ylab="prestations sociales du gouvernement en billions $", col="blue", main="Evolution des prestations sociales du gouvernement entre 1947 et 2022 en billion de dollars")

     