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
source("Funkcje_wlasne.R")

plik_xls="gr11_Murder_US.xlsx"
dane=read_excel(path=plik_xls, sheet = "murder", range = "A23:G176")
# sortowanie wg farm , gdybysmy zapomnieli to zrobic w Excelu! 
dane=dane[ order(dane$id, dane$rok), ] # zamiast order(dane[,1], dane[,2])

# przygotowanie danych w kontekscie funkcji produkcji
yX_=dane[, -c(1:4)]

# przygotowanie danych/zmiennych dla plm
indeks_it=dane[,c(1,4)] #tabela z indeksami farm i okresow
colnames(indeks_it)=c("stan", "rok") # index w plm
y=as.matrix(yX_[,1]) ; colnames(y)="y"
X=as.matrix(yX_[,-1])

N=nrow(unique(indeks_it[,1]))  #Extract Unique Elements
T=nrow(unique(indeks_it[,2]))  #Extract Unique Elements
NT=N*T

# definicja dla plm zbioru danych zawierajacej id_firmy, okres, y, X-sy (bez jedynki) 
data_plm = cbind(indeks_it, y, X)
formula_=y~X

#-----------------------------------------------------------------------------------
# Model z indywidualnymi i czasowymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# Obliczenia z pomoca biblioteki plm
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + lambda_t+ beta*x_it + v_it
M1_FE_e2= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
            model = c("within"), index=c("stan","rok"))
summary(M1_FE_e2)

# Send R output to a file
wyniki_csv="M1_FE_e2.csv"
Wydruk_csv(Model=M1_FE_e2, Plik=wyniki_csv)

plik_txt="M1_FE_e2.txt"
sink(file = plik_txt, append = FALSE)
summary(M1_FE_e2)
sink() # sink(file = NULL)

# ----------------------------------
# model w dwoma efektami losowymi
#random.method method of estimation for the variance components in the random effects model,
#one of "swar" (default), "amemiya", "walhus", "nerlove"; for Hausman-Taylor
#estimation set to "ht" (see Details and Examples),

# y_it = beta0 ++ beta*x_it + alfa_i + lambda_t +v_it
M2_RE_e2= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
            model = c("random"), index=c("stan","rok"), random.method="nerlove")
summary(M2_RE_e2)

# Send R output to a file
wyniki_csv="M2_RE_e2.csv"
Wydruk_csv(Model=M2_RE_e2, Plik=wyniki_csv)

plik_txt="M2_RE_e2.txt"
sink(file = plik_txt, append = FALSE)
summary(M2_RE_e2)


# Test Hausmana
# phtest Hausman Test for Panel Models
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Model z ef. losowymi (H0) vs. Model z ef. stalymi (H1)", sep = '\n')
cat("Hausman Test for Panel Models", sep = '\n')
phtest(M1_FE_e2, M2_RE_e2)  # Model = model z indyw. ef. stalymi
cat("", sep = '\n')
testH_M2_M1=phtest(M1_FE_e2, M2_RE_e2)  
cat(c("p-value dla stytystyki chi2 = ", testH_M2_M1$p.value), sep = '\n')
if (testH_M2_M1$p.value<0.05) {  
  cat("Male p-value (<0.05), wiec odrzucamy H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze stalymi efektami indywidualnymi i czasowymi", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model z losowymi efektami indywidualnymi i czasowymi", sep = '\n')
}
cat(" ", sep = '\n') 
sink() # sink(file = NULL)



#-----------------------------------------------------------------------------------
# Model ze wspolnym wyrazem wolnym - pooling model, stosujemy MNK
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + beta*x_it + v_it
M3_pooled = plm( formula=formula_, data=data_plm,   
          model = c("pooling"), index=c("stan","rok"))
summary(M3_pooled)

wyniki_csv="M3_pooled.csv"
Wydruk_csv(Model=M3_pooled, Plik=wyniki_csv)

plik_txt="M3_pooled.txt"
sink(file = plik_txt, append = FALSE)
summary(M3_pooled)




# plmtest Lagrange FF Multiplier Tests for Panel Models
# Description Test of individual and/or time effects for panel models.
# type a character string indicating the test to be performed; bp" for Breusch and Pagan (1980),
# effect = c("individual", "time", "twoways"
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models, Breusch and Pagan (1980)", sep = '\n')
cat("Model ze wspolnym wyrazem wolnym (H0) vs. Model z DWOMA ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_alfa=0 i sigma2_lambda=0", sep = '\n')

plmtest(x=M3_pooled, effect = "twoways", type="bp")
Test_M3_M2=plmtest(x=M3_pooled, effect = "twoways", type="bp")

cat(c("Dla statystyki chi2 -> p.value = ", Test_M3_M2$p.value), sep = '\n')
cat("", sep = '\n')
if (Test_M3_M2$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze losowymi efektami", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze wspolnym wyrazem wolnym (a nie z efektami losowymi)", sep = '\n')
}

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models, Breusch and Pagan (1980)", sep = '\n')
cat("Model ze wspolnym wyrazem wolnym (H0) vs. Model z JEDNYM ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_alfa=0", sep = '\n')

plmtest(x=M3_pooled, effect = "individual", type="bp")
Test_M3_M2=plmtest(x=M3_pooled, effect = "individual", type="bp")

cat(c("Dla statystyki chi2 -> p.value = ", Test_M3_M2$p.value), sep = '\n')
cat("", sep = '\n')
if (Test_M3_M2$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze losowymi efektami", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze wspolnym wyrazem wolnym (a nie z efektami losowymi)", sep = '\n')
}

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models, Breusch and Pagan (1980)", sep = '\n')
cat("Model ze wspolnym wyrazem wolnym (H0) vs. Model z JEDNYM ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_lambda=0", sep = '\n')

plmtest(x=M3_pooled, effect = "time", type="bp")
Test_M3_M2=plmtest(x=M3_pooled, effect = "time", type="bp")

cat(c("Dla statystyki chi2 -> p.value = ", Test_M3_M2$p.value), sep = '\n')
cat("", sep = '\n')
if (Test_M3_M2$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze losowymi efektami", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze wspolnym wyrazem wolnym (a nie z efektami losowymi)", sep = '\n')
}

sink() # sink(file = NULL)




