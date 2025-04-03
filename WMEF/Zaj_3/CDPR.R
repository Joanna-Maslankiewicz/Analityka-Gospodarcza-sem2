library(rugarch)
library(readxl)
library(TSA)
library(car)
library(MTS)
library(psych)

# Wczytanie danych

dane = read.csv("CDPR_close.csv", sep = ";")

dane$Date = as.Date(dane$Date, format="%d.%m.%Y")
dane$Close = as.numeric(gsub(",", ".", dane$Close))

head(dane)
str(dane)

y = ts(dane[,2])

# logarytmiczne stopy zwrotu
y_2 = y^2
y_ln = diff(log(y))
y_ln_2 = y_ln^2

t(describe(y))
t(describe(y_ln))
t(describe(y_ln_2))

par(mfrow = c(2,1))
plot(dane$Date, dane$Close, type = "l", xlab = "Data", ylab = "Cena zamknięcia", main = "Wykres cen CD Projekt")
plot(y_ln)
plot(y_ln_2)

###########################################################

# ACF
par(mfrow = c(2,1))
acf(y_ln, drop.lag.0 = TRUE, lag.max = 35, type = "correlation", plot = TRUE)
acf(y_ln_2, drop.lag.0 = TRUE, lag.max = 35, type = "correlation", plot = TRUE) 

# Test Ljung-Box
Box.test(y_ln, lag = 20, type = "Ljung-Box")
Box.test(y_ln_2, lag = 20, type = "Ljung-Box")

# Test McLeod-Li
McLeod.Li.test(y = y_ln)$p.values

# Test ARCH (MTS)
archTest(y_ln)

###########################################################

# modele GARCH - wybór
garch_norm = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  distribution.model = "norm",
  start.pars = list(omega = 0.00001, alpha1 = 0.4, beta1 = 0.55, gamma1 = 0.15, shape = 7)
)

garch_t = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  distribution.model = "std",
  start.pars = list(omega = 0.00001, alpha1 = 0.4, beta1 = 0.55, gamma1 = 0.15, shape = 7)
)

garch_gjr = ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  distribution.model = "sstd",
  start.pars = list(omega = 0.00001, alpha1 = 0.4, beta1 = 0.55, gamma1 = 0.15, shape = 7)
)

garch_norm_fit = ugarchfit(spec = garch_norm, data = y, solver = "hybrid")
garch_t_fit = ugarchfit(spec = garch_t, data = y, solver = "hybrid")
garch_gjr_fit = ugarchfit(spec = garch_gjr, data = y, solver = "hybrid")

info_criteria = cbind(
  infocriteria(garch_norm_fit),
  infocriteria(garch_t_fit),
  infocriteria(garch_gjr_fit)
)
rownames(info_criteria) = c("Akaike", "Bayes", "Shibata", "Hannan-Quinn")
colnames(info_criteria) = c("sGARCH Normal", "sGARCH Student", "GJR-GARCH")
print(info_criteria)

###########################################################

# modele GARCH - analiza estymatorów parametrów
print(garch_t_fit@fit$coef)
names(garch_t_fit@model)
names(garch_t_fit@fit)

###########################################################

# wykresy
par(mfrow = c(3,1))
plot(garch_t_fit@fit$residuals, type = "l", main = "Wykres surowych reszt")
plot(garch_t_fit@fit$sigma, type = "l", main = "Wykres wariancji warunkowych")
plot(sqrt(garch_t_fit@fit$sigma), type = "l", main = "Wykres odchylen warunkowych")

###########################################################

# diagnostyka reszt
res_n = garch_norm_fit@fit$residuals / garch_norm_fit@fit$sigma #garch residuals - normal
res_t = garch_t_fit@fit$residuals / garch_t_fit@fit$sigma #garch residuals - student
res_g = garch_gjr_fit@fit$residuals / garch_gjr_fit@fit$sigma #garch residuals - gjr

plot(res_n - res_t, main="Różnica między res_n a res_t", type="l")
plot(res_n - res_g, main="Różnica między res_n a res_g", type="l")
plot(res_t - res_g, main="Różnica między res_t a res_g", type="l")

df_t = garch_t_fit@fit$coef["shape"]
df_g = garch_gjr_fit@fit$coef["shape"]

qqPlot(res_n, dist="norm", main="QQ-Plot res_n (Normal)")
qqPlot(res_t, dist="t", df = df_t, main="QQ-Plot res_t (Student)")
qqPlot(res_g, dist="t", df = df_g, main="QQ-Plot res_g (GJR-GARCH)")

Box.test(garch_t_fit@fit$residuals / garch_t_fit@fit$sigma, lag = 20, type = "Ljung-Box")
Box.test((garch_t_fit@fit$residuals / garch_t_fit@fit$sigma)^2, lag = 20, type = "Ljung-Box")
shapiro.test(garch_t_fit@fit$residuals / garch_t_fit@fit$sigma)

###########################################################

#prognoza
model_fit = ugarchfit(data = y, spec = garch_t, out.sample = 10, solver = "hybrid")
forecast = ugarchforecast(model_fit, n.ahead = 10, n.roll = 10, out.sample = 10)
par(mfrow = c(1,1))
plot(forecast, which = "all")




