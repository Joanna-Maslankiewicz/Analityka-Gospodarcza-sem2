# Jerzy Marzec, UEK Krakow II 2025, Mikroekonometria
# CTRL +L - czyszczenie konsoli
rm(list=ls())

# require("nazwa_paczki") - sprawdza czy paczka jest zainstalowana
# require() - wczytanie do pamieci lub wymuszanie instalacji, gdy paczka nie zostala zainstalowana

if( !require("readxl")) {install.packages("readxl",dependencies=TRUE)} # ! - indicates logical negation (NOT).
if( !require("plm")) {install.packages("plm",dependencies=TRUE)} # 
library(readxl)
library(plm)

# katalog roboczy
setwd("d:\\Mikro")  #getwd()
#setwd("d:\\Mikro\\Murder_S2")
source("Funkcje_wlasne.R")  #Read R Code from a File

plik_xls="gr12_Murder_US.xlsx"
dane=read_excel(path=plik_xls, sheet = "murder", range = "A23:G176")
# sortowanie wg farm , gdybysmy zapomnieli to zrobic w Excelu! 
dane=dane[ order(dane$id, dane$rok), ] # zamiast order(dane[,1], dane[,2])
#dane=dane_ ; rm(dane_)

# przygotowanie danych 
yX_=dane[, -c(1:4)]

# przygotowanie danych/zmiennych dla plm
indeks_it=dane[,c(1,4)] #tabela z indeksami farm i okresow
#colnames(indeks_it)=c("id_farmy", "rok")
y=as.matrix(yX_[,1]) ; colnames(y)="y"
X=as.matrix(yX_[,-1])
# N, T i NT
N=nrow( as.matrix( unique(x=indeks_it[,1] ) ) )# unikalne kody obiektow
T=nrow( as.matrix( unique(x=indeks_it[,2] ) ) )# unikalne kody czasu
NT=N*T

# definicja dla plm zbioru danych zawierajacej id_firmy, okres, y, X-sy (bez jedynki) 
data_plm = cbind(indeks_it, y, X)
formula_=y~X # y~x1+x2

#-----------------------------------------------------------------------------------
# Model z indywidualnymi i czasowymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + lambda_t + beta*x_it + v_it
M1_FE_2e= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
            model = c("within"), index=c("nazwa_stanu","rok"))
summary(M1_FE_2e)
plik_csv="M1_FE_2e.csv"
Wydruk_csv(Model=M1_FE_2e, Plik=plik_csv)

plik_txt="M1_FE_2e.txt"
sink(file=plik_txt, append=FALSE)
summary(M1_FE_2e)
#closeAllConnections() 
sink()   # czasem nie dziala!

# efekty losowe - inny sposob liczenia s2_alfa i s2_lambda
M2_RE_2e= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
               model = c("random"), index=c("nazwa_stanu","rok"), random.method="nerlove")
summary(M2_RE_2e)
# Wydruk do pliku
plik_csv="M2_RE_2e.csv"
Wydruk_csv(Model=M2_RE_2e, Plik=plik_csv)

plik_txt="M2_RE_2e.txt"
sink(file=plik_txt, append=FALSE)
summary(M2_RE_2e)
sink()

# bez efektow
M3_pooled= plm( formula=formula_, data=data_plm,  
               model = "pooling", index=c("nazwa_stanu","rok"))
summary(M3_pooled)
# Wydruk do pliku
plik_csv="M3_pooled.csv"
Wydruk_csv(Model=M3_pooled, Plik=plik_csv)

plik_txt="M3_pooled.txt"
sink(file=plik_txt, append=FALSE)
summary(M3_pooled)

sink()


plik_txt="M2_RE_2e.txt"
sink(file=plik_txt, append=TRUE)

# phtest Hausman Test for Panel Models
#  H0: efekty losowe vs 
#  H1: model z efektami stalymi zamiast losowych
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Model z ef. losowymi (H0) vs. Model z ef. stalymi H1", sep = '\n')
cat("Hausman Test for Panel Models", sep = '\n')
phtest(M2_RE_2e, M1_FE_2e, method = c("chisq"))
testH_M2_M1=phtest(M2_RE_2e, M1_FE_2e, method = c("chisq"))
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

# plmtest Lagrange FF Multiplier Tests for Panel Models
# Description Test of individual and/or time effects for panel models.
# effect = c("individual", "time", "twoways"),
# type a character string indicating the test to be performed; bp" for Breusch and Pagan (1980),

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models", sep = '\n')
cat("Breusch and Pagan (1980) ", sep = '\n')
cat("Model ze wspolnym wyraem wolnym (H0) vs. Model z ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_alfa=0 i sigma2_lambda=0", sep = '\n')

plmtest(x=M2_RE_2e, effect = c("twoway"), type = c("bp") )
cat("", sep = '\n')
cat("H0:  sigma2_alfa=0", sep = '\n')
plmtest(x=M2_RE_2e, effect = c("individual"), type = c("bp") )
cat("", sep = '\n')
cat("H0:  sigma2_lambda=0", sep = '\n')
plmtest(x=M2_RE_2e, effect = c("time"), type = c("bp") )


cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
# efekty losowe - tylko indywidualne  oraz inny sposob liczenia s2_alfa i s2_lambda
M2_RE_ef_ind= plm( formula=formula_, data=data_plm, effect = c("individual"),  
               model = c("random"), index=c("nazwa_stanu","rok"), random.method="nerlove")
summary(M2_RE_ef_ind)
# Wydruk do pliku
plik_csv="M2_RE_ef_ind.csv"
Wydruk_csv(Model=M2_RE_ef_ind, Plik=plik_csv)


cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
# efekty losowe - tylko czasowe
M2_RE_ef_time= plm( formula=formula_, data=data_plm, effect = c("time"),  
                   model = c("random"), index=c("nazwa_stanu","rok"), random.method="nerlove")
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

