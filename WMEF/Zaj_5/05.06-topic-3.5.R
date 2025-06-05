library(rmgarch)
dane1 = read.csv2(file = "pkn_20152021.csv", dec = ".", sep = ",")
dane2 = read.csv(file = "kgh_20152021.csv", sep = ",", dec = ".")
dane3 = read.csv(file = "pko_d.csv", dec = ".")

y = na.omit(merge(dane1, dane2, by = "Data"))
y = na.omit(merge(y, dane3, by = "Data"))
#?merge
dim(y)
y = y[,c(2,3,4)]

y_returns = y[-1,]
y_returns[,1] = diff(log(y[,1]))
y_returns[,2] = diff(log(y[,2]))
y_returns[,3] = diff(log(y[,3]))

nobs = dim(y_returns)[1]
plot(y_returns[,3], type = "l")

varx_fit_returns = varxfit(y_returns, p=2, postpad = "none")
varx_fit = varxfit(y, p = 2, postpad = "none")

names(varx_fit)
dim(varx_fit$xfitted)
varx_fit$Bcoef   # współczynniki dla opóźnień - l1, l2,... - opóźnienia, lagi
varx_fit$se
varx_fit$pstat
y[-c(1,2),]-(varx_fit$xfitted+varx_fit$xresiduals)   # mają być zera, bo = y - y teoretyczne - błędy

yspec1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "sstd")
yspec = multispec(replicate(3, yspec1))

spec_dcc = dccspec(uspec = yspec, VAR = TRUE, lag =2, dccOrder = c(1,1), distribution = "mvt")

yfit_DCC_returns = dccfit(spec_dcc, data = y_returns, fit.control = list(eval.se = TRUE), cluster = NULL)
yfit_DCC = dccfit(spec_dcc, data = y, fit.control = list(eval.se = TRUE), cluster = NULL)

par(mfrow=c(3,2))
plot(rcor(yfit_DCC)[1,1,],type="l")
plot(rcor(yfit_DCC)[1,2,],type="l")
plot(rcor(yfit_DCC)[1,3,],type="l")
plot(rcor(yfit_DCC)[2,2,],type="l")
plot(rcor(yfit_DCC)[2,3,],type="l")
plot(rcor(yfit_DCC)[3,3,],type="l")

DCCtest(y, garchOrder = c(1,1), n.lags = 1, solver = "solnp", solver.control = list(), cluster = NULL, Z = NULL)


varx_fit_returns = varxfit(y_returns, p = 2, postpad = "constant")
yspec1 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "sstd")
yspec = multispec(replicate(3, yspec1))
spec_dcc = dccspec(uspec = yspec, VAR = TRUE, lag = 2, dccOrder = c(1,1), distribution = "mvt")
multf = multifit(yspec, data = varx_fit_returns$xresiduals, cluster = NULL)
yfit_DCC_returns = dccfit(spec_dcc, data = y_returns, fit.control = list(eval.se=TRUE), VAR.fit = varx_fit_returns, fit = multf, cluster = NULL)

install.packages("MTS")
library(MTS)
install.packages("vars")
library(vars)
MarchTest(yfit_DCC_returns@mfit$stdresid, lag = 4)
arch.test(VAR(yfit_DCC_returns@mfit$stdresid))

K=3 #3 walory ryzykowne
H_t = matrix(0, nrow = K, ncol = K) #macierz war. kowariancji dla ust t
C_t = matrix(0, nrow = 1, ncol = 1) #stala normujaca dla chwili t
w_wagi = matrix(0, nrow = (nobs+1), ncol = K)
w_wagi_statyczny = matrix(0, nrow = (nobs+1), ncol = K)+c(1/3, 1/3, 1/3)
i_jedn = 1 + vector("integer", length = K)
alfa = 0.05 #poziom istotności
VaR_Long_returns = matrix(0, nrow = (nobs))
VaR_Long_portfolio = matrix(0, nrow = (nobs))
est_mi = matrix(0, nrow = nobs, ncol = K) #Prognozy warunkowych wartosci oczekiwanych; Uzyskujemy z dopasowania modelu VAR
est_mi = varx_fit_returns$xfitted #wartosci predyktywne = srednie =oszacowania modelu

V_assets = matrix(0, nrow = (nobs+1), ncol = K)#+1 bo warunek startowy uwzgledniam; Wartosci instrument tworzacych portfel
V_0_assets = as.matrix(y[1, c(1,2,3)])#Poczatkowe wartosci instrumentow; 
V_assets[1,] = V_0_assets#Poczatkowe wartosci instrumentow; tutaj arbitralnie przyjete
V_assets = as.matrix(y[,c(1,2,3)])

par(mfrow=c(3,1))
plot(V_assets[,1])
plot(V_assets[,2])
plot(V_assets[,3])


###############################

V_portfolio = vector("integer", length = (nobs+1)) #Values of the portfolio
V_portfolio[1] = 1000 #arbitralnie przyjeta wartość portfela
V_portfolio_statyczny = vector("integer", length = (nobs+1))#Values of the portfolio
V_portfolio_statyczny[1] = 1000#arbitralnie przyjeta wartosc portfela

w_wagi[1,] = c(1/3, 1/3, 1/3)

liczbaAkcji = matrix(0, nrow = (nobs+1), ncol = K)
liczbaAkcji[1,] = V_portfolio[1] * w_wagi[1,] / V_assets[1,] #liczba akcji odpowiadajaca momentowi startowemu 1
liczbaAkcji_statyczny = matrix(0, nrow = (nobs+1), ncol = K)
liczbaAkcji_statyczny[1,] = V_portfolio_statyczny[1] * w_wagi_statyczny[1,] / V_assets[1,]

alfa = 0.05

#wersja student:
for(i in 2:(nobs+1)){
  H_t = yfit_DCC_returns@mfit$H[,,i-1]
  C_t = (t(as.matrix(i_jedn)) %*% solve(H_t)) %*% as.matrix(i_jedn)
  w_wagi[i,] = solve(H_t) %*% as.matrix(i_jedn / C_t[1,1]) 
  VaR_Long_returns[i-1] = -sum(w_wagi[i-1,] * est_mi[i-1,]) - qnorm(alfa) * ((t(as.matrix(w_wagi[i-1,])) %*% H_t) %*% as.matrix(w_wagi[i-1,]))^0.5 #Wzór poprawny przy założeniu warunkowego rozkładu normalnego ale jak się okazuje działa
  V_portfolio[i] = sum(liczbaAkcji[i-1,] * (V_assets[i,])) #sum(w_wagi[i,]*(V_assets[i,]))
  V_portfolio_statyczny[i] = sum(liczbaAkcji_statyczny[i-1,] * (V_assets[i,]))
  liczbaAkcji[i,] = V_portfolio[i] * w_wagi[i,] / V_assets[i,]
  liczbaAkcji_statyczny[i,] = V_portfolio_statyczny[i] * w_wagi_statyczny[i,] / V_assets[i,]
  VaR_Long_portfolio[i-1] = V_portfolio[i-1] * (exp(sum(w_wagi[i-1,] * est_mi[i-1,]) + qnorm(alfa) * ((t(as.matrix(w_wagi[i-1,])) %*% H_t) %*% as.matrix(w_wagi[i-1,]))^0.5))
}

#Wykres wag dla kazdej ze skladowych portfela
par(mfrow = c(3,1))
plot(w_wagi[,1], type = "l")
plot(w_wagi[,2], type = "l")
plot(w_wagi[,3], type = "l")
w_wagi[1,]
w_wagi[2,]

par(mfrow=c(3,1))
plot(w_wagi_statyczny[,1], type = "l")
plot(w_wagi_statyczny[,2], type = "l")
plot(w_wagi_statyczny[,3], type = "l")

par(mfrow = c(2,1))
plot(V_portfolio, type = "l")
plot(V_portfolio_statyczny, type = "l")

portfolio_returns = diff(log(V_portfolio))
plot(portfolio_returns, type = "l")

par(mfrow = c(1,1))
matplot(portfolio_returns, type = "l")
matplot(-VaR_Long_returns, col = "red", add = T, type = "l")
matplot(VaR_Long_returns, col = "red", add = T, type = "l")

par(mfrow = c(1,1))
matplot(V_portfolio_statyczny, type = "l", col = "red", ylim = c(0,3000))
matplot(V_portfolio, type = "l", col = "green", add = T)

# test kupca - tylko dla VaR dla st?p zwrotu
library("rugarch")
print(VaRTest(alfa, portfolio_returns, -VaR_Long_returns))
print(VaRTest(alfa, V_portfolio[-1], VaR_Long_portfolio))  # nie narażamy ponad stan pieniędzy klienta
length(V_portfolio[-1])
length(VaR_Long_portfolio)

sum(portfolio_returns < (-VaR_Long_returns)) / length(portfolio_returns)   # ma być około 5%
alfa
#Test Kupca NIE odrzuca szacowanie VaR za pomocą rozważanego modelu








































set.seed(2016)
nobs = 1000; cut = 10000
a = c(0.003, 0.005, 0.001)
A = diag(c(0.02,0.03,0.015))
B = diag(c(0.02, 0.06, 0.08))
uncR = matrix(c(1.0, -0.4, 0.3, -0.4, 1.0, 0.12, 0.3, 0.12, 1.0),3,3)
1
dcc.para = c(0.01,0.98)
dcc.data.n = dcc.sim(nobs, a, A, B, uncR, dcc.para, model="diagonal")
V_assets=matrix(0,nrow=(nobs+1),ncol=K)
V_0_assets=c(10,12,9)
V_assets[1,]=V_0_assets
for(i in 2:(nobs+1)){
  V_assets[i,]=V_assets[i-1,]*exp(dcc.data.n$eps[i-1,])
}
w_wagi_statyczny=matrix(0,nrow=(nobs+1),ncol=K)+c(1/3,1/3,1/3)
V_portfolio_statyczny=vector("integer",length=(nobs+1))
V_portfolio_statyczny[1]=1000
liczbaAkcji_statyczny=matrix(0,nrow=(nobs+1),ncol=K)
liczbaAkcji_statyczny[1,]=
  for(i in 2:(nobs+1)){
    V_portfolio_statyczny[i]=
      liczbaAkcji_statyczny[i,]=
  }


qnorm(alfa)
solve(H_t)  #odwracanie macierzy
%*%   #operator mnożenia macierzy w programie R
  K=3
H_t=matrix(0,nrow=K,ncol=K)
C_t=matrix(0,nrow=1,ncol=1)
w_wagi=matrix(0,nrow=(nobs+1),ncol=K)
w_wagi[1,]=c(1/3,1/3,1/3)
liczbaAkcji=matrix(0,nrow=(nobs+1),ncol=K)
liczbaAkcji[1,]=
  for(i in 2:(nobs+1)){
    D_t=diag()
    H_t=
      C_t=
      w_wagi[i,]=
      VaR_Long_returns[i-1]<ñ
    V_portfolio[i]=
      liczbaAkcji[i,]=
      VaR_Long_portfolio[i-1]=
  }
VaRTest(alfa, ???,???)


