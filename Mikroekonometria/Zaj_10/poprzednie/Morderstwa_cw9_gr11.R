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
setwd("d:\\Mikro\\Murder_S2")  #getwd()
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

# y_it = beta0 + + beta*x_it + alfa_i + lambda_t +v_it
M2_RE_e2= plm( formula=formula_, data=data_plm, effect = c("twoways"),  
            model = c("random"), index=c("stan","rok"), random.method="nerlove")
summary(M2_RE_e2)

# Send R output to a file
wyniki_csv="M2_RE_e2.csv"
Wydruk_csv(Model=M2_RE_e2, Plik=wyniki_csv)

plik_txt="M2_RE_e2.txt"
sink(file = plik_txt, append = FALSE)
summary(M2_RE_e2)
sink() # sink(file = NULL)

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
  cat("Zatem dane popieraja model ze stalymi efektami indywidualnymi", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model z losowymi efektami indywidualnymi", sep = '\n')
}
cat(" ", sep = '\n') 


# plmtest Lagrange FF Multiplier Tests for Panel Models
# Description Test of individual and/or time effects for panel models.
# type a character string indicating the test to be performed; bp" for Breusch and Pagan (1980),
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models, Breusch and Pagan (1980)", sep = '\n')
cat("Model ze wspolnym wyrazem wolnym (H0) vs. Model z DWOMA ef. losowymi (H1)", sep = '\n')
cat("H0:  sigma2_alfa=0 i sigma2_lambda=0", sep = '\n')

# do poprawy
plmtest(x=M3_pooled, effect = "individual", type="bp")

cat("", sep = '\n')
Test_???=plmtest(x=???, effect = "twoways",   type = "bp")
cat(c("Dla statystyki chi2 -> p.value = ", Test_???$p.value), sep = '\n')

if (Test_???$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze losowymi efektami", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze wspolnym wyrazem wolnym (a nie z efektami losowymi)", sep = '\n')
}




# ponizej kod z zadania Podukcja Ryzu











# Wyniki czastkowe
df_=Model$df.residual  # N*T-k-N 
RSS_=as.matrix( as.data.frame(Model$residuals) )  # reszty MNK z modelu
RSS_= crossprod(RSS_)  # suma kwadratow

beta=as.matrix(Model$coefficients)  # estym. MNK (bWG) dim = kx1
colnames(beta)="FE (bWG)"
beta_cov=as.matrix(Model$vcov)  # est.macierzy kowariancji V(bWG)  dim = kxk
beta_sd=as.matrix( diag(beta_cov))^0.5  # bledy szacunku # dim = kx1
colnames(beta_sd)="sd"
beta_iloraz_t=abs(beta/beta_sd)
colnames(beta_iloraz_t)="|iloraz_t|"
# uwaga beta_iloraz_t= |  |
p_value= 2*( 1-pt(q=beta_iloraz_t, df=df_, lower.tail = TRUE, log.p = FALSE) ) 
colnames(p_value)="p_value"
#pt(q=-1.65, df=df_, lower.tail = TRUE, log.p = FALSE)
#pt(q=1.65, df=df_, lower.tail = TRUE, log.p = FALSE)
#pnorm(q=-1.96, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
#pnorm(q=-1.65, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
# sigma2_it -> s2_v = estymator wariancji skladnika losowego v_it
s2_v = RSS_/df_

wyniki_csv="M1_IndywEfStale.csv"
druk=cbind(beta, beta_sd, beta_iloraz_t, p_value, beta_cov)
tytul_="Model z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)"
write(x=tytul_, file=wyniki_csv, append = FALSE)
write.table(x=druk, file=wyniki_csv, append = TRUE, sep=";", dec = ",", col.names = NA)
write(x=" ", file=wyniki_csv, append = TRUE)
write(c(x="Liczba stopni swobody = ", df_), file=wyniki_csv, append = TRUE)
write(c(x="Liczba obiektow = ", N), file=wyniki_csv, append = TRUE)
write(c(x="Liczba okresow = ", T), file=wyniki_csv, append = TRUE)
write(c(x="Liczba obserwacji N*T = ", NT), file=wyniki_csv, append = TRUE)
write(c(x="Estymator wariancji skladnika losowego v_it = s2_v", s2_v), file=wyniki_csv, append = TRUE)

#This function gives an overall intercept for within models and its accompanying standard error or a
#within model with the overall intercept 
beta0_=within_intercept(Model)
beta0=beta0_[[1]]         # ocena 
beta0_sd=attr(beta0_, which="se") # blad szacunku  beta0_sd=attr(beta0_, "se")  
beta0_=as.matrix(cbind(beta0, beta0_sd))

# Obliczenia wlasne bledu szacunku  dla beta0
x_mean=t(as.matrix(colMeans(X)))  # dim = 1x5  gdzie k=5
beta0_sd_ = as.numeric( sqrt(s2_v/NT + x_mean%*% beta_cov%*%t(x_mean) ) )
beta0_sd_

write(x=" ", file=wyniki_csv, append = TRUE)
write(x="ocena i blad wyrazu wolneogo", file=wyniki_csv, append = TRUE)
write.table(beta0_, file=wyniki_csv, append = TRUE, sep=";", dec = ",",row.names = FALSE)


# obliczanie stalych  efektow  indywidualnych
# Function to extract the fixed effects from a plm object and associated summary method
alfa=as.matrix(fixef(Model, effect = "individual", type = "level") ) # alfa_i
colnames(alfa)= "alfa"   
alfa_kontrast=as.matrix(fixef(Model, effect = "individual", type = "dmean") ) # odchylenie od wspol wyrazu wolnego
colnames(alfa_kontrast)= "alfa_kontrast"   
# inaczej 
alfa_kontrast_ = alfa - colMeans(alfa)
sum(alfa_kontrast_-alfa_kontrast)

write(x=c(" "),   file = wyniki_csv, append = TRUE)
write(x=c("Oceny stalych  efektow  indywidualnych i kontrastow"), file = wyniki_csv, append = TRUE)
write(x=c("alfa_kontrast = alfa - colMeans(alfa)"), file = wyniki_csv, append = TRUE)
write(x=c(" "),   file = wyniki_csv, append = TRUE)
druk=cbind(alfa, alfa_kontrast)
write.table(x=druk, file = wyniki_csv, append = TRUE, sep = ";", 
            dec = ",", col.names = NA)

# Testowanie 
# Dokonac testowania zasadnosi uwzglednienia efektow stalych
# (wzgledem modelu ze wspolnym wyrazem wolnym) za pomoca testu F 
# H0: y_it = beta0 + beta*x_it + v_it    (np. beta0 = alfa_N)
# H1: y_it = alfa_i + beta*x_it + v_it

#-----------------------------------------------------------------------------------
# Model ze wspolnym wyrazem wolnym - pooling model, stosujemy MNK
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = alfa_i + beta*x_it + v_it
M2 = plm( formula=formula_, data=data_plm,   
            model = c("pooling"), index=c("id_farmy","rok"))
summary(M2)

sink(file = plik_txt, append = TRUE)
cat("", sep = '\n')
cat("----------------------------------------", sep = '\n')
cat("", sep = '\n')
cat("Model z wspolnym wyrazem wolnym - estymator MNK ", sep = '\n')
cat("Estymacja parametrow modelu z pomoca PLM", sep = '\n')
cat("y_it = alfa_N + beta*x_it + v_it", sep = '\n')

summary(M2)
cat("", sep = '\n')

# Testowanie efektow stalych wzgledem modelu ze wspolnym wyrazem wolnym za pomoca testu F
# Test of individual and/or time effects based on the comparison of the within and the pooling model
# pFtest F Test for Individual and/or Time Effects
cat("Testowanie efektow stalych wzgledem modelu ze wspolnym wyrazem wolnym za pomoca testu F", sep = '\n')
pFtest(Model, M2 ) # Model H1 , Model H0
sink() # sink(file = NULL)

# obliczenia wlasne
M2_df_=M2$df.residual  # N*T-k-N 
M2_RSS_=as.matrix( as.data.frame(M2$residuals) )  # reszty MNK z modelu
M2_RSS_= crossprod(M2_RSS_)  # suma kwadratow
J = -( df_ - M2_df_)  # liczba restrykcji  N-1
F_emp=(M2_RSS_/RSS_-1)/(J/df_)
colnames(F_emp) =" Test F"
p_value=pf(q=F_emp, df1=J, df2=df_, lower.tail = FALSE, log.p = FALSE)

sink(file = plik_txt, append = TRUE)
cat("", sep = '\n')
cat("Test F - obliczenia wlasne", sep = '\n')
cat(paste("Liczba restrykcji= ", J), sep = '\n')
cat(paste("Liczba df w modelu dla H0 = ", M2_df_), sep = '\n')
cat(paste("Liczba df w modelu dla H1 = ", df_), sep = '\n')
cat(paste("SKR (SumKwadrReszt) w modelu H0 = ", M2_RSS_), sep = '\n')
cat(paste("SKR (SumKwadrReszt) w modelu H1 = ", RSS_), sep = '\n')
cat(paste("Statystyka F =  ", F_emp), sep = '\n')
cat(paste("p_value = ", p_value), sep = '\n')
sink() # sink(file = NULL)




# --------------- test -----------------------
# f) Dokonac testowania zasadnosci modelu H1 wzgledem modelu  zawierajacego tylko i wyacznie indywidualne efekty stale,  
# H0:  y_it = alfa_i +  v_it  vs.  H1:  y_it = alfa_i + beta*x_it + v_it
# H0:  beta1=beta2=... =beta5 = 0
# Estymacja modelu  H0:  y_it = alfa_i +  v_it

# dim (Z) = NT x N macierz odpowiadajaca stalym efektom 
Z=diag(1, nrow=N, ncol=N)%x%matrix(1, nrow=T, ncol=1)
colnames(Z) = paste0("farma_", 1:(ncol(Z))) # nazwy kolumn numerowane dynamicznie farma_1, farma_2
dim(Z)

formula_=y~-1+Z  # -1 = usuwamy kolumne jedynek 
data_plm_Z = cbind(data_plm[,c(1:3)], Z)
#-----------------------------------------------------------------------------------
# Model tylko z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# Estymacja modelu  H0:  y_it = alfa_i +  v_it
# musi by? model = c("pooling") i brak effect =""
M3= plm( formula=formula_, data=data_plm_Z, model = c("pooling"), index=c("id_farmy","rok"))

sink(file = plik_txt, append = TRUE)
cat("", sep = '\n')
cat("----------------------------------------------", sep = '\n')
cat("", sep = '\n')
cat("Model tylko z indywidualnymi efektami stalymi (ustalonymi), bez X-sow", sep = '\n')
summary(M3)
# rownowazne z factor(id_farmy)
M3b= plm( formula=y~-1+factor(id_farmy), data=data_plm,   
         model = c("pooling"), index=c("id_farmy","rok"))
summary(M3b)
# rownowazne z kontrastami 
cat("", sep = '\n')
cat("Model z kontrastami dla firm 2,3,4. .., zal. beta0=efekt_farmy_1", sep = '\n')
M3c= plm( formula=y~factor(id_farmy), data=data_plm,   
          model = c("pooling"), index=c("id_farmy","rok"))
summary(M3c)

cat("", sep = '\n')
cat("Testowanie beta1=...=beta5=0 wzgledem modelu beta<>0 za pomoca testu F", sep = '\n')
cat("H0:  y_it = alfa_i +  v_it  vs.  H1:  y_it = alfa_i + beta*x_it + v_it, sep" , sep = '\n')
cat("H0:  beta1=beta2=... =beta5 = 0" , sep = '\n')
pFtest(Model, M3) # Model H1 , Model H0
testM3_M=pFtest(Model, M3) # Model H1 , Model H0
sink() # sink(file = NULL)


# =====================================
# obliczyc miernik dopasowania R2 w modelu y_it = ALFA_i + x_it*beta + v_it
# R2_Within - ocena dopasowania wzgledem y_it = ALFA_i + v_it
R2_Within = r.squared(object = Model)
R2_Within_Adj = 1- (1-R2_Within) /(df_/ ((NT-1)) )# Adjusted R2 - korekta stopni swobody dla M1_R2_Within

# zwykly R2 - miernik  wzgledem y_it = beta0 + v_it
R2_LSDV= 1 - RSS_/sum( (y-mean(y))^2) 

# Zwykly R2 w modelu  y_it = beta0 + beta*x_it + v_it
M2_R2_pooled = r.squared(object = M2) 
# test F na redukcje - inny sposób obliczenai statystyki F
(R2_LSDV-M2_R2_pooled)/(1-R2_LSDV) /((M2_df_-df_)/df_)


sink(file = plik_txt, append = TRUE)
cat("", sep = '\n') 
cat("---------R2-----------", sep = '\n') 
cat("Ocena dopasowania y_it = ALFA_i + x_it*beta + v_it wzgledem y_it = alfa_i + v_it", sep = '\n') 
cat(paste("R2_Within =", R2_Within), sep = '\n') 
cat("Korekta R2_Within o stopnie swobody", sep = '\n') 
cat(paste("R2_Within_Adj =", R2_Within_Adj), sep = '\n') 
cat("Ocena dopasowania wzgledem y_it = beta0 + v_it", sep = '\n') 
cat(paste("R2_LSDV = ", R2_LSDV), sep = '\n') 
cat("Ocena dopasowania y_it = beta0 + beta*x_it + v_it wzgledem y_it = beta0 + v_it", sep = '\n') 
cat(paste("M2_R2_pooled = ", M2_R2_pooled), sep = '\n') 
cat("", sep = '\n') 
sink() # sink(file = NULL) 




# ==================================================================

# model z indydualnymi efektami losowymi

plik_txt="Fprod_IndywEfLosowe.txt"
wyniki_csv="Fprod_IndywEfLosowe.csv"

#---------------Efekty losowe-----------------------------------------------------------
# Model z efektami losowymi (indywidualnymi)
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"),
# y_it = BETA0 + beta*x_it + alfa_i + v_it

formula_=y~X  # Uwaga bez " "
# generalnie trzeba dodac index=c("id_farmy", "t") gdy dwie pierwsze kolumny "dane_plm" nie zawieraja tych indeksow 
M4_RE = plm(formula=formula_, data=data_plm, effect="individual", 
            model="random",index=c("id_farmy","rok"))

summary(M4_RE)

sink(file="RE_Wyniki.txt", append = TRUE)
summary(M4_RE)
sink()

M4_df_=M4_RE$df.residual
M4_beta=as.matrix(M4_RE$coefficients)  # estym. MNK (bWG) dim = kx1
colnames(M4_beta)="FE (bUMNK)"
M4_beta_cov=as.matrix(M4_RE$vcov)  # est.macierzy kowariancji V(bWG)  dim = kxk
M4_beta_sd=as.matrix( diag(M4_beta_cov))^0.5  # bledy szacunku # dim = kx1
colnames(M4_beta_sd)="sd"
M4_beta_iloraz_t=abs(M4_beta/M4_beta_sd)
colnames(M4_beta_iloraz_t)="|iloraz_t|"
# uwaga beta_iloraz_t= |  |
M4_p_value= 2*( 1-pt(q=M4_beta_iloraz_t, df=M4_df_, lower.tail = TRUE, log.p = FALSE) ) 
colnames(M4_p_value)="p_value"

druk=cbind(M4_beta, M4_beta_sd, M4_beta_iloraz_t, M4_p_value, M4_beta_cov)
tytul_="Model z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)"
write(x=tytul_, file=wyniki_csv, append = FALSE)
write.table(x=druk, file=wyniki_csv, append = TRUE, sep=";", dec = ",", col.names = NA)
write(x=" ", file=wyniki_csv, append = TRUE)
write(c(x="Liczba stopni swobody = ", M4_df_), file=wyniki_csv, append = TRUE)
write(c(x="Liczba obiektow = ", N), file=wyniki_csv, append = TRUE)
write(c(x="Liczba okresow = ", T), file=wyniki_csv, append = TRUE)
write(c(x="Liczba obserwacji N*T = ", NT), file=wyniki_csv, append = TRUE)

# wydruk wynikow z modelu do pliku csv
wyniki_csv="RE_Wyniki.csv"
Wydruk_csv(Model=M4_RE, Plik=wyniki_csv)


sink(file="RE_Wyniki.txt", append = TRUE)

# Test Walda H0: beta=0 
# pwaldtest Wald-style Chi-square Test and F Test  (test = c("Chisq", "F"))
cat("", sep = '\n') 
cat("---------------------------------------------------------", sep = '\n')
cat("Test na redukcje modelu poprzez usuniecie X-sow przy beta (test F lub Chi^2", sep = '\n')
cat("H0: beta1=...=beta_k=0", sep = '\n')
pwaldtest(x=M4_RE, test = c("Chisq"))
pwaldtest(x=M4_RE, test = c("F"))

# Test Hausmana
# phtest Hausman Test for Panel Models
cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Model z ef. losowymi (H0) vs. Model z ef. stalymi H1", sep = '\n')
cat("Hausman Test for Panel Models", sep = '\n')
phtest(Model, M4_RE)  # Model = model z indyw. ef. stalymi
testH_M4_M1=phtest(Model, M4_RE)  
cat(c("Dla statystyki chi2 -> p.value = ", TestM3_M1$p.value), sep = '\n')


if (TestM3_M1$p.value<0.05) {  
  cat("Niske p-value (<0.05) swiadczy o odrzuceniu H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model ze stalymi efektami indywidualnymi", sep = '\n')
} else {
  cat("Duze p-value (>0.05), wiec nie ma podstaw do odrzucenia H0 na rzecz H1", sep = '\n')
  cat("Zatem dane popieraja model z efektami losowymi", sep = '\n')
} cat("Zatem dane popieraja model ze stalymi efektami indywidualnymi", sep = '\n')
}
cat(" ", sep = '\n') 

# obliczenia wlasne  #Hausman=q'(V_q)^-1 *q
q=(beta - M4_beta[-1,])  # bez beta0
Hausman_test=t(q) %*% solve(beta_cov - M4_beta_cov[-1,-1]) %*% q
colnames(Hausman_test)="Test Hausmana"
rownames(Hausman_test)="Statysyka chi^2"
cat(paste("Hausman_test = ", Hausman_test), sep = '\n') 


# plmtest Lagrange FF Multiplier Tests for Panel Models
# Description Test of individual and/or time effects for panel models.
# type a character string indicating the test to be performed; bp" for Breusch and Pagan (1980),

cat("", sep = '\n')
cat("---------------------------------------------------------", sep = '\n')
cat("Lagrange FF Multiplier Tests for Panel Models", sep = '\n')
cat("Breusch and Pagan (1980) ", sep = '\n')
cat("Model ze wspolnym wyraem wolnym (H0) vs. Model z indywidualnymi ef. losowymi H1", sep = '\n')
plmtest(x=M2, effect = "individual", type="bp")

# obliczenia wlasne
# SKR = sum((M2$residuals)^2) = M2_RSS_
tmp_data=cbind(indeks_it[,1], M2$residuals )
licznik=aggregate(tmp_data[,2], by=list(tmp_data[,1]),FUN="sum")
licznik=sum( (licznik[,2])^2 )    # colSums(licznik)[2]
testM2_M4 = (NT/(2*(T-1))) *(1-licznik/M2_RSS_)^2
testM2_M4
cat(paste ("Statystyka Breusch and Pagan (1980) = ", testM2_M4), sep = '\n')

sink()
