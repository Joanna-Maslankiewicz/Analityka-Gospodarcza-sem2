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
source("Funkcje_wlasne.R")  #Read R Code from a File

plik_xls="gr12_F_kosztu_banki_dane200firm.xlsx"
dane=read_excel(path=plik_xls, sheet = "dane", range = "A28:I1028")
# sortowanie wg farm , gdybysmy zapomnieli to zrobic w Excelu! 
dane=dane[ order(dane$BANK, dane$t), ] # zamiast order(dane[,1], dane[,2])
#dane=dane_ ; rm(dane_)

# przygotowanie danych 
yX_=dane[, -c(1:2)]

# przygotowanie danych/zmiennych dla plm
indeks_it=dane[,c(1,2)] #tabela z indeksami farm i okresow
#colnames(indeks_it)=c("id_farmy", "rok")
y=as.matrix(yX_[,1]) ; colnames(y)="lnC/w5"
X=as.matrix(yX_[,-1])
# N, T i NT
N=nrow( as.matrix( unique(x=indeks_it[,1] ) ) ) # unikalne kody obiektow
T=nrow( as.matrix( unique(x=indeks_it[,2] ) ) ) # unikalne kody czasu
NT=N*T

# definicja dla plm zbioru danych zawierajacej id_firmy, okres, y, X-sy (bez jedynki) 
data_plm = cbind(indeks_it, y, X)
formula_=y~X # y~x1+x2

#-----------------------------------------------------------------------------------
# Model z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + lambda_t + beta*x_it + v_it
M1_FE= plm( formula=formula_, data=data_plm, effect = c("individual"),  
            model = c("within"), index=c("BANK","t"))
summary(M1_FE)
plik_csv="M1_FE.csv"
Wydruk_csv(Model=M1_FE, Plik=plik_csv)

plik_txt="M1_FE.txt"
sink(file=plik_txt, append=FALSE)
summary(M1_FE)
#closeAllConnections() 
sink()   # czasem nie dziala!

# efekty losowe - inny sposob liczenia s2_alfa i s2_lambda
M2_RE= plm( formula=formula_, data=data_plm, effect = c("individual"),  
               model = c("random"), index=c("BANK","t"), random.method="swar")
summary(M2_RE)
# Wydruk do pliku
plik_csv="M2_RE.csv"
Wydruk_csv(Model=M2_RE, Plik=plik_csv)

plik_txt="M2_RE.txt"
sink(file=plik_txt, append=FALSE)
summary(M2_RE)


# phtest Hausman Test for Panel Models
#  H0: efekty losowe vs 
#  H1: model z efektami stalymi zamiast losowych
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Model z ef. losowymi (H0) vs. Model z ef. stalymi H1", sep = '\n')
cat("Hausman Test for Panel Models", sep = '\n')
phtest(M2_RE, M1_FE, method = c("chisq"))
testH_M2_M1=phtest(M2_RE, M1_FE, method = c("chisq"))
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

#================================================
# bez efektow = z wspolnym wyrazem wolnym
M3_pooled= plm( formula=formula_, data=data_plm,  
                model = "pooling", index=c("BANK","t"))
summary(M3_pooled)
# Wydruk do pliku
plik_csv="M3_pooled.csv"
Wydruk_csv(Model=M3_pooled, Plik=plik_csv)

plik_txt="M3_pooled.txt"
sink(file=plik_txt, append=FALSE)
summary(M3_pooled)
sink()


plik_txt="M2_RE.txt"
sink(file=plik_txt, append=TRUE)
# plmtest Lagrange FF Multiplier Tests for Panel Models
# Description Test of individual and/or time effects for panel models.
# effect = c("individual", "time", "twoways"),
# type a character string indicating the test to be performed; bp" for Breusch and Pagan (1980),
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models", sep = '\n')
cat("Breusch and Pagan (1980) ", sep = '\n')
cat("Model ze wspolnym wyrazem wolnym (H0) vs. Model z ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_alfa=0 ", sep = '\n')

plmtest(x=M2_RE, effect = c("individual"), type = c("bp") )
cat("", sep = '\n')
sink()



#F Test for Individual and/or Time Effects
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("TestF  na redukcje modelu", sep = '\n')
cat("H0: alfa(1)=..alfa(N-1) = ", sep = '\n')
pFtest(M1_FE, M3_pooled)

Test_M3_M1=pFtest(M1_FE, M3_pooled)

plik_txt="M1_FE.txt"
sink(file = plik_txt, append = TRUE)
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("F Test for Individual and/or Time Effects", sep = '\n')
cat(c("Test F H0: alfa(1)=...=alfa_(N-1)=0", Test_M3_M1$p.value), sep = '\n')
cat(c("Dla statystyki F -> p.value = ", Test_M3_M1$p.value), sep = '\n')
cat("", sep = '\n')
if (Test_M3_M1$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze indywidualnymi efektami stalymi", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze wspolnym wyrazem wolnym (a nie z efektami losowymi)", sep = '\n')
}

sink() # sink(file = NULL)
closeAllConnections() 


teta=M2_RE$ercomp$theta
teta
it_yX=cbind(indeks_it, yX_)
yX_mean=aggregate( it_yX[,-c(1,2)], by = list(it_yX[,1]), FUN="mean")
yX_mean=as.matrix(yX_mean[,-1]) # bez nr banku
bRE=as.matrix( M2_RE$coefficients )

#transformacja danych, czyli y , 1 oraz X-sy
Jeden5= matrix(1,nrow=T,ncol=1)
temp= yX_mean %x%  Jeden5  # iloczym Kronekera

y_transf=as.matrix( yX_[,1]-teta*temp[,1])
X_transf=yX_[,-1]-teta*temp[,-1]
stala_transf=matrix(1,nrow=T*N,ncol=1)-teta
X_transf=as.matrix( cbind (stala_transf , X_transf) )
b=solve(t(X_transf)%*%X_transf)%*%(t(X_transf)%*%y_transf)
b-bRE

reszty = y_transf - X_transf %*% bRE
colnames(reszty)= "reszty"
sum(reszty)

# srednia reszta dla kazdego banku
tmp=cbind(indeks_it[,1], reszty)

reszty_mean=aggregate( tmp[,2], by = list(tmp[,1]), FUN="mean")

# Wydruk do pliku
plik_csv="M2_reszty_mean.csv"
write.table(x=reszty_mean, file = plik_csv, append = FALSE, sep = ";", dec = ",", col.names = NA)
