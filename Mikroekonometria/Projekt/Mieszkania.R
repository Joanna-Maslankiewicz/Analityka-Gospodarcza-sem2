rm(list=ls())

if( !require("readxl")) {install.packages("readxl",dependencies=TRUE)} # ! - indicates logical negation (NOT).
if( !require("plm")) {install.packages("plm",dependencies=TRUE)} # library(readxl)
library(plm)

source("Funkcje_wlasne.R") 

plik_xls="Mieszkania.xlsx"
dane=read_excel(path=plik_xls, sheet = "Dane2", range = "A1:J225")
# sortowanie wg farm , gdybysmy zapomnieli to zrobic w Excelu! 
#dane=dane[ order(dane$Wojewodztwo, dane$Rok), ] # zamiast order(dane[,1], dane[,2])
#dane=dane_ ; rm(dane_)

# dodanie trendu  - alternatywnie mozna wykorzyta? kolumne "dane$YEARDUM"
trend_=c(1:14)         # 14 - liczba okresow  ) max(dane[,2]
ones=matrix(1,16,1)   # 16 - liczba wojewodztw    max(dane[,1])
trend = ones %x% trend_  #dim = 14*16 x 1 (iloczyn Kroneckera) - trzeba kontrolowa? A%x%B czy B%x%A
#trend-dane[,2]  # ma byc = 0 

# przygotowanie danych w kontekscie funkcji produkcji
yX_=dane[, -c(1,2)]
yX_=cbind(yX_, trend)

# przygotowanie danych/zmiennych dla plm
indeks_it=dane[,c(1:2)] #tabela z indeksami cen i okresow
colnames(indeks_it)=c("Wojewodztwo", "Rok")
y=as.matrix(yX_[,1]) ; colnames(y)="y"
X=as.matrix(yX_[,-1])
# N, T i NT
N=nrow( as.matrix( unique(x=indeks_it[,1] ) ) )# unikalne kody obiektow
T=nrow( as.matrix( unique(x=indeks_it[,2] ) ) )# unikalne kody czasu
NT=N*T

# definicja dla plm zbioru danych zawierajacej ceny, okres, y, X-sy (bez jedynki) 
data_plm = cbind(indeks_it, y, X)
formula_=y~X 

# Model z indywidualnymi i czasowymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# Obliczenia z pomoca biblioteki plm
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + lambda_t+ beta*x_it + v_it
M1_FE_e2= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
               model = c("within"), index=c("Wojewodztwo","Rok"))
summary(M1_FE_e2)

# Send R output to a file
wyniki_csv="M1_FE_e2.csv"
Wydruk_csv(Model=M1_FE_e2, Plik=wyniki_csv)

plik_txt="M1_FE_e2.txt"
sink(file = plik_txt, append = FALSE)
summary(M1_FE_e2)
sink() # sink(file = NULL)


#Test F na istotność efektów czasowych i indywidualnych
M1_FE= plm( formula=formula_, data=data_plm, effect = c("individual"),  
            model = c("within"), index=c("Wojewodztwo","Rok"))
summary(M1_FE)
M1_T= plm( formula=formula_, data=data_plm, effect = c("time"),  
           model = c("within"), index=c("Wojewodztwo","Rok"))
summary(M1_T)
pFtest(M1_FE_e2,M1_FE)
pFtest(M1_FE_e2,M1_T)

plik_testy_F <- "Testy_F_M1_FE_e2_vs_FE_T.txt"
sink(file = plik_testy_F)

cat("Test F: model dwuczynnikowy vs. model z efektami indywidualnymi\n")
print(pFtest(M1_FE_e2, M1_FE))

cat("\nTest F: model dwuczynnikowy vs. model z efektami czasowymi\n")
print(pFtest(M1_FE_e2, M1_T))

sink()  

# ----------------------------------
# model z dwoma efektami losowymi
#random.method method of estimation for the variance components in the random effects model,
#one of "swar" (default), "amemiya", "walhus", "nerlove"; for Hausman-Taylor
#estimation set to "ht" (see Details and Examples),

# y_it = beta0 ++ beta*x_it + alfa_i + lambda_t +v_it
M2_RE_e2= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
               model = c("random"), index=c("Wojewodztwo","Rok"), random.method="nerlove")
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
                 model = c("pooling"), index=c("Wojewodztwo","Rok"))
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

#Testy na modelu 
install.packages(c("plm", "lmtest", "sandwich", "tseries", "car", "urca"))

library(plm)
library(lmtest)
library(sandwich)
library(tseries)
library(car)
library(urca)

kor_err = pbgtest(M2_RE_e2)
hetero = bptest(M2_RE_e2)
jarque.bera.test(residuals(M2_RE_e2))

#Redukcja modelu
data= data_plm[, -c(8, 9, 10)]
X_2 = as.matrix(data[, 4:8])
formula_red = y ~ X_2
M2_RE_e2_reduced= plm( formula=formula_red, data=data, effect = c("twoways"),  
               model = c("random"), index=c("Wojewodztwo","Rok"), random.method="nerlove")

summary(M2_RE_e2_reduced)

# Send R output to a file
wyniki_csv="M2_RE_e2_reduced.csv"
Wydruk_csv(Model=M2_RE_e2_reduced, Plik=wyniki_csv)

plik_txt="M2_RE_e2_reduced"
sink(file = plik_txt, append = FALSE)
summary(M2_RE_e2_reduced)
sink() # sink(file = NULL)

kor_err = pbgtest(M2_RE_e2_reduced)
hetero = bptest(M2_RE_e2_reduced)
jarque.bera.test(residuals(M2_RE_e2_reduced))
plik_testy <- "diagnostyka_M2_RE_e2_reduced.txt"

sink(file = plik_testy, append = FALSE)
cat("=== Test autokorelacji (Breusch-Godfrey / Wooldridge) ===\n")
print(pbgtest(M2_RE_e2_reduced))
cat("\n=== Test heteroskedastyczności (Breusch-Pagan) ===\n")
print(bptest(M2_RE_e2_reduced))
cat("\n=== Test normalności reszt (Jarque-Bera) ===\n")
print(jarque.bera.test(residuals(M2_RE_e2_reduced)))
sink()
