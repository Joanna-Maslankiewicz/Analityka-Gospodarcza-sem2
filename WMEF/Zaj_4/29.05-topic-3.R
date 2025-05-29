# Zadanie 1

dane1 = read.csv2(file = "WIG20_2008_2016_ost.csv", dec=",")
dane2 = read.csv2(file = "DAX_2008_2016_ost.csv", dec=",")
dane3 = read.csv2(file = "DJI_2008_2016_ost.csv", dec=",")
acwi_us_poczatkowe = read.csv2(file = "acwi_us_d_2008_2016_ost.csv", dec=",")

dane_wstepne = merge(dane1, dane2, by = "Date", all = T, sort = T)
dane_wstepne = merge(dane_wstepne, dane3, by = "Date", all = T, sort = T)
dane_wstepne = merge(dane_wstepne, acwi_us_poczatkowe, by = "Date", all = T, sort = T)
dim(dane1)[1];dim(dane2)[1];dim(acwi_us_poczatkowe)[1];dim(dane_wstepne)[1]

#############
# albo eliminujemy dane puste (podać wtedy jaki % danych został usunięty), albo interpolacja liniowa (uzupełnienie danych w oparciu o sąsiadów) 

# pozbywamy się danych NA

names(dane_wstepne)=c("Date","WIG20","DAX","DJI","ACWI")
brakujace1=which(is.na(dane_wstepne[,2])==TRUE)
brakujace2=which(is.na(dane_wstepne[,3])==TRUE)
brakujace3=which(is.na(dane_wstepne[,4])==TRUE)
brakujace4=which(is.na(dane_wstepne[,5])==TRUE)
missing_data_ind_min=unique(c(brakujace1,brakujace2,brakujace3,brakujace4))
which(is.na(dane_wstepne[,5])==TRUE)
dane=dane_wstepne[-(missing_data_ind_min),]
dim(dane)
sum(is.na(dane))

# albo inaczej:
#names(dane_wstepne)=c("Date","WIG20","DAX","DJI","ACWI")
#dane = dane_wstepne[complete.cases(dane_wstepne), ]
#dim(dane)[1]

#####
write.table(dane,file="WIG20_DAX_DJI_ACWI_20082016_usunieteBraki.txt",col.names = c("Date","WIG20","DAX","DJI","ACWI"))
dane=read.table(file="WIG20_DAX_DJI_ACWI_20082016_usunieteBraki.txt",header = TRUE)

acwi_us=dane[,5]
y=matrix(0,(dim(dane)[1]-1),2)#-1 bo beda nas interesowaly stopy zwrotu

y[,1]=100*log(dane[-1,2]/dane[-(dim(dane)[1]),2])# jedna z metod definiowania stóp zwrotu
y[,2]=100*log(dane[-1,3]/dane[-(dim(dane)[1]),3])
#y to w 1 kolumnie WIG, w 2 kolumnie DAX

par(mfrow=c(2,1))
plot(y[,1], type="l"); abline(h=0)
plot(y[,2], type="l"); abline(h=0)

###################

# Zadanie 2

library(rmgarch)
#Najpierw oceniamy jak wygladaja warunkowe korelacje oszacowane za pomoca DCC
yspec1 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model="sstd")
yspec = multispec(replicate(2, yspec1))
#Dopasowywuje do
multf = multifit(yspec, data = y,cluster=NULL)
spec_dcc=dccspec(uspec = yspec, VAR = FALSE, lag =0, dccOrder = c(1,1), distribution = "mvnorm")

yfit_DCC = dccfit(spec_dcc, data = y, fit.control = list(eval.se=TRUE),fit = multf, cluster = NULL)

par(mfrow=c(1,1))
plot(rcor(yfit_DCC)[1,2,],type="l",ylim=c(-1,1))#Widac zmieniajaca sie w czasie korelacje
yfit_DCC

# dwa ostatnie parametry są charakterystyczne dla DCC. Jeśli przynajmniej jeden z nich jest istotny statystycznie, to warunkowe korelacje zmieniają się w czasie
# Wniosek: warunkowe korelacje zmieniają się w czasie

#Teraz dopasowanie modelu CCC-GARCH. Potem testujemy stałe warunkowe korelacje
uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                    distribution.model = "norm")
spec1 = cgarchspec(uspec = multispec( replicate(2, uspec) ), VAR = FALSE, robust = FALSE, lag = 0, lag.max = NULL,
                    lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                    robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500),
                    dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1],
                                                                                     method = c("Kendall", "ML")[2], time.varying = FALSE,#Je?li FALSE to CCC, w przeciwnym razie DCC
                                                                                     transformation = c("parametric", "empirical", "spd")[1]))
dim(y)
yfit_CCC = cgarchfit(spec1, data = y, cluster = NULL, fit.control = list(eval.se=FALSE))
#yfit_DCC
names(yfit_CCC@mfit)
(yfit_CCC)
#macierz
(yfit_CCC@mfit$Rt)

#w modelu CCC korelacja jest stała - ~0.61

####################

# Zadanie 3

par(mfrow=c(3,1), las=2)

plot(rcor(yfit_DCC)[1,2,],type="l", axes=FALSE,xlab="",ylab="")

abline(h=yfit_CCC@mfit$Rt[1,2],col="red")

skala=seq(from=1, to=(dim(dane)[1]-1),by=252)

axis(side=1,cex.axis=0.8,at=skala,padj=1,labels=dane$Date[skala+1])

axis(2)

#warunkowa wariancja dla WIG20:

plot(rcov(yfit_DCC)[1,1,],type="l", axes=FALSE,xlab="",ylab="")

skala=seq(from=1, to=(dim(dane)[1]-1),by=252)

axis(side=1,cex.axis=0.8,at=skala,padj=1,labels=dane$Date[skala+1])

axis(2)

#warunkowa wariancja dla DAX:

plot(rcov(yfit_DCC)[2,2,],type="l", axes=FALSE,xlab="",ylab="")

skala=seq(from=1, to=(dim(dane)[1]-1),by=252)

axis(side=1,cex.axis=0.8,at=skala,padj=1,labels=dane$Date[skala+1])

axis(2)

#wyższa zmienność -> wyższa współzależność
#efekt zarażania; chcemy wyeliminować czynnik globalny

############################

# Zadanie 4

# Test Engla i Shepharda 2001
DCCtest(y,garchOrder = c(1,1), n.lags = 1, solver = "solnp",solver.control = list(), cluster = NULL, Z = yfit_CCC@mfit$stdresid)
DCCtest(y,garchOrder = c(1,1), n.lags = 1, solver = "solnp",solver.control = list(), cluster = NULL, Z = NULL)

DCCtest(y,garchOrder = c(1,1), n.lags = 5, solver = "solnp",solver.control = list(), cluster = NULL, Z = yfit_CCC@mfit$stdresid)
DCCtest(y,garchOrder = c(1,1), n.lags = 5, solver = "solnp",solver.control = list(), cluster = NULL, Z = NULL)

#w każdym teście nie ma podstaw do odrzucenia H0 mówiącej że korelacja jest stała (?)

# Test Engla i Shepharda na 3000 ścieżkach:
simulationDCC=dccsim(yfit_DCC,n.sim=3000,n.start=1000,m.sim=1)
ysimDCC=simulationDCC@msim$simX[[1]][,1:2]
DCCtest(ysimDCC, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
#średnio działa bo za każdym razem wychodzi zupełnie inne p-value, więc lepiej chyba trzymać się parametrów

###############

# Eliminujemy czynnik globalny

acwi=100*log(acwi_us[-1]/acwi_us[-length(acwi_us)])
par(mfrow=c(1,1))
plot(acwi)

y_resztyACWI=matrix(0,length(acwi),2)

#pracujemy na resztach, bo chcemy wyeliminować czynnik globalny
pom1=lm(y[,1]~1+acwi)
y_resztyACWI[,1]=pom1$residuals

pom2=lm(y[,2]~1+acwi)
y_resztyACWI[,2]=pom2$residuals

#Warto sprawdzic jak wygladaja warunkowe korelacje uzyskane w oparciu o DCC
z_DCC_bezGlobalny = dccfit(spec_dcc, data = y_resztyACWI, fit.control = list(eval.se=TRUE),fit = multf, cluster = NULL)

z_CCC_bezGlobalny = cgarchfit(spec1, data = y_resztyACWI, cluster = NULL, fit.control = list(eval.se=FALSE))

par(mfrow=c(1,1))
plot(rcor(z_DCC_bezGlobalny)[1,2,],type="l",ylim=c(-1,1))
abline(h=z_CCC_bezGlobalny@mfit$Rt[1,2],col="red")
z_DCC_bezGlobalny@mfit$matcoef
#dccb1 jest istotny, więc korelacje zmieniają się  w czasie

DCCtest(y_resztyACWI,garchOrder = c(1,1), n.lags = 3, solver = "solnp",solver.control = list(), cluster = NULL, Z = z_CCC_bezGlobalny@mfit$stdresid)
DCCtest(y_resztyACWI,garchOrder = c(1,1), n.lags = 6, solver = "solnp",solver.control = list(), cluster = NULL, Z = NULL)
DCCtest(y_resztyACWI,garchOrder = c(3,3), n.lags = 6, solver = "nlminb",solver.control = list(), cluster = NULL, Z = NULL)

par(mfrow=c(3,1))
plot(rcor(z_DCC_bezGlobalny)[1,2,] , type="l", main="Conditional correlation", xlab="", ylab=""); abline(h=0) #korelacja WARUNKOWA
abline(h=z_CCC_bezGlobalny@mfit$Rt[1,2],col="red")
plot(rcov(z_DCC_bezGlobalny)[1,1,], type="l", main="Conditional variance for WIG20", xlab="", ylab="")
plot(rcov(z_DCC_bezGlobalny)[2,2,], type="l", main="Conditional variance for DAX", xlab="", ylab="")

par(mfrow=c(3,1))
plot(rcor(z_DCC_bezGlobalny)[1,2,] , type="l", main="Conditional correlation", xlab="", ylab=""); abline(h=0) #korelacja WARUNKOWA
abline(h=z_CCC_bezGlobalny@mfit$Rt[1,2],col="red")
plot(y[,1], type="l"); abline(h=0) #stopy zwrotu
plot(y[,2], type="l"); abline(h=0) #stopy zwrotu

# A moze zobaczymy na wykresach korelacji i surowych cen
par(mfrow=c(3,1))
plot(rcor(z_DCC_bezGlobalny)[1,2,], type="l", main="Conditional correlation", xlab="", ylab=""); abline(h=0) #korelacja WARUNKOWA
abline(h=z_CCC_bezGlobalny@mfit$Rt[1,2],col="red")
plot(dane[-1,2], type="l") #wykresy indeksów
plot(dane[-1,3], type="l") #wykresy indeksów

#zwykle jak na rynkach kapitałowych cena idzie w dół -> wzrost zmienności -> wzrost warunkowej korelacji; EFEKT ZARAŻANIA
#problem w tym że nie wiemy gdzie był pierwszy impuls, gdzie się zaczęło
#efekt zarażania może być rozumiany w sensie zarówno negatywnym jak i pozytywnym