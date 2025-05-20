rm(list=ls())
if(!require("readxl")) {install.packages("readxl",dependencies=TRUE)} # ! - indicates logical negation (NOT).
if(!require("plm")) {install.packages("plm",dependencies=TRUE)} # 
library(readxl)
library(plm)

source("Funkcje_wlasne.R")

plik_xls = "koszt_banki.xlsx"
dane = read_excel(path=plik_xls, sheet = "dane", range = "A28:I1028")

dane = dane[order(dane$BANK, dane$t),] #zamiast order(dane[,1], dane[,2])

#przygotowanie danych 
yX_ = dane[,-c(1:2)]

#przygotowanie danych/zmiennych dla plm
indeks_it = dane[,c(1,2)] #tabela z indeksami banków i okresow

colnames(indeks_it) = c("BANK","t")
y = as.matrix(yX_[,1]); colnames(y) = 'lnC_w5'
colnames(y) = "y"
X = as.matrix(yX_[,-1])

#N, T i NT
N = nrow(as.matrix(unique(x=indeks_it[,1]))) #unikalne kody obiektow
T = nrow(as.matrix(unique(x=indeks_it[,2]))) #unikalne kody czasu
NT = N*T

#definicja dla plm zbioru danych zawierajacej id_firmy, okres, y, X-sy (bez jedynki) 
data_plm = cbind(indeks_it, y, X)
formula_ = y ~ X #y ~ x1 + x2

#-------------------------------------------------------------------------------------
# Model z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + beta*x_it + v_it

M1_FE = plm(formula = formula_, data = data_plm, effect = c("individual"),  
               model = c("within"), index = c("BANK","t"))
summary(M1_FE)

plik_csv = "M1_FE.csv"
Wydruk_csv(Model = M1_FE, Plik=plik_csv)

plik_txt = "M1_FE.txt"
sink(file = plik_txt, append=FALSE)

summary(M1_FE)
#closeAllConnections() 
sink()   # czasem nie dziala!

#---------------------------------------------------------------------------------------
# efekty losowe - inny sposob liczenia s2_alfa i s2_lambda

M2_RE_2e = plm(formula = formula_, data = data_plm, effect = c("individual"),  
               model = c("random"), index = c("BANK","t"))
summary(M2_RE_2e)

# Wydruk do pliku
plik_csv = "M2_RE_2e.csv"
Wydruk_csv(Model = M2_RE_2e, Plik = plik_csv)

plik_txt = "M2_RE_2e.txt"
sink(file = plik_txt, append=FALSE)
summary(M2_RE_2e)
sink()

#---------------------------------------------------------------------------------------
# TEST HAUSMANA
# phtest Hausman Test for Panel Models
#  H0: efekty losowe vs 
#  H1: model z efektami stalymi zamiast losowych

plik_txt = "test_hausmana.txt"
sink(file = plik_txt, append = TRUE)

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Model z ef. losowymi (H0) vs. Model z ef. stalymi H1", sep = '\n')
cat("Hausman Test for Panel Models", sep = '\n')
phtest(M1_FE, M2_RE_2e, method = c("chisq"))
testH_M2_M1 = phtest(M1_FE, M2_RE_2e, method = c("chisq"))
cat("", sep = '\n')
cat(c("Dla statystyki chi2 -> p.value = ", testH_M2_M1$p.value), sep = '\n')

if (testH_M2_M1$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze stalymi efektami indywidualnymi", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model z efektami losowymi", sep = '\n')
}
cat(" ", sep = '\n') 

sink()

#---------------------------------------------------------------------------------------
# bez efektow
M3_pooled = plm(formula = formula_, data = data_plm,  
                model = "pooling", index = c("BANK","t"))
summary(M3_pooled)
# Wydruk do pliku
plik_csv = "M3_pooled.csv"
Wydruk_csv(Model = M3_pooled, Plik = plik_csv)

plik_txt = "M3_pooled.txt"
sink(file = plik_txt, append = FALSE)
summary(M3_pooled)

sink()

#---------------------------------------------------------------------------------------
# plmtest Lagrange FF Multiplier Tests for Panel Models
# Description Test of individual and/or time effects for panel models.
# effect = c("individual", "time", "twoways"),
# type a character string indicating the test to be performed; bp" for Breusch and Pagan (1980),

plik_txt = "test_breuscha_pagana.txt"
sink(file = plik_txt, append = FALSE)

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models", sep = '\n')
cat("Breusch and Pagan (1980) ", sep = '\n')
cat("Model ze wspolnym wyraem wolnym (H0) vs. Model z ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_alfa=0 i sigma2_lambda=0", sep = '\n')

plmtest(x = M3_pooled, effect = c("twoway"), type = c("bp") )
cat("", sep = '\n')
cat("H0:  sigma2_alfa=0", sep = '\n')
plmtest(x = M3_pooled, effect = c("individual"), type = c("bp") )
cat("", sep = '\n')
cat("H0:  sigma2_lambda=0", sep = '\n')
plmtest(x = M3_pooled, effect = c("time"), type = c("bp") )

sink()

#---------------------------------------------------------------------------------

plik_txt = "test_F_na_efekty_indywidualne.txt"
sink(file = plik_txt, append = FALSE)

pFtest(M1_FE, M3_pooled)

sink()

#---------------------------------------------------------------------------------

#TU SKOŃCZYLIŚMY













cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
# efekty losowe - tylko indywidualne  oraz inny sposob liczenia s2_alfa i s2_lambda
M2_RE_ef_ind= plm( formula=formula_, data=data_plm, effect = c("individual"),  
                   model = c("random"), index=c("id_stanu","rok"), random.method="nerlove")
summary(M2_RE_ef_ind)
# Wydruk do pliku
plik_csv="M2_RE_ef_ind.csv"
Wydruk_csv(Model=M2_RE_ef_ind, Plik=plik_csv)


cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
# efekty losowe - tylko czasowe
M2_RE_ef_time= plm( formula=formula_, data=data_plm, effect = c("time"),  
                    model = c("random"), index=c("id_stanu","rok"), random.method="nerlove")
summary(M2_RE_ef_time)
# Wydruk do pliku
plik_csv="M2_RE_ef_time.csv"
Wydruk_csv(Model=M2_RE_ef_time, Plik=plik_csv)

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')

cat("Test BP gdy H1: model RE z efektem czasowym", sep = '\n')
plmtest(x=M2_RE_ef_time, effect = c("time"), type = c("bp") )

#closeAllConnections() 
sink()   # czasem nie dziala!