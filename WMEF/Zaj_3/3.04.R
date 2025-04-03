###################################
# zadanie 1
###################################

install.packages("rmgarch")
library(rmgarch)

data(dji30retw)

Dat = dji30retw[, 1:3]
y = Dat   #na tym wektorze pracujemy

specyfikacja = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model = "norm")
dcc_specyfikacja = dccspec(uspec = multispec(replicate(3, specyfikacja)), VAR = TRUE, lag = 1, dccOrder = c(1,1), distribution = "mvnorm")

fit_dcc = dccfit(spec = dcc_specyfikacja, data = y)   #[VAR GARCH DCC UncQ] : [12+9+2+3] <-- 9 bo 9 spółek; 2 bo dodatkowe dwa parametry b i c; 3 bo zostają 3 unikalne wartości na macierzy S (bo jest symetryczna więc reszta się powtarza)

names(fit_dcc@mfit)
fit_dcc@mfit$R
fit_dcc@model$residuals
class(fit_dcc)

slotNames(fit_dcc)
names(fit_dcc@model)
names(fit_dcc@mfit)

describe(fit_dcc@mfit$stdresid) # sd = 1, bo reszty są standaryzowane
describe(fit_dcc@model$residuals) # sd != 1

fit_dcc@mfit$coef # = coef(fit_dcc)
fit_dcc@mfit$matcoef

fit_dcc@model$varcoef

dcc.forecast = dccforecast(fit_dcc,n.ahead=2)

plot(dcc.forecast)

names(y)
par(mfrow = c(3,3))
plot(fit_dcc)

fit_dcc@mfit$R[2]

fit_dcc@mfit$stdresid
fit_dcc@model$residuals

plot(fit_dcc@mfit$stdresid)
plot(fit_dcc@model$residuals) 

fit_dcc@mfit$convergence # 0 <-- oznacza zbieżność, to dobrze

###################################
# zadanie 2
###################################

#fit_dcc@mfit$coef[13]=0.4
#fit_dcc@mfit$coef[14]=0.2
#fit_dcc@mfit$coef[1]=0 # zerujemy pierwszą omegę

?dccsim

simulation = dccsim(fit_dcc, n.sim = 3000, n.start = 1000, m.sim = 1, rseed = 1)

simulation@msim$simH
simulation@msim$simR

par(mfrow=c(3,1))

plot(simulation@msim$simR[[1]][1,1,],type="l")#simulated time series with DCC-GARCH process
plot(simulation@msim$simR[[1]][2,2,],type="l")
plot(simulation@msim$simR[[1]][3,3,],type="l")

par(mfrow=c(3,1))
plot(simulation@msim$simR[[1]][1,2,],type="l")#simulated time series with DCC-GARCH process
plot(simulation@msim$simR[[1]][1,3,],type="l")
plot(simulation@msim$simR[[1]][2,3,],type="l")


###################################
# zadanie 3
###################################

cluster=NULL
Dat = dji30retw[, 1:3, drop = FALSE]

uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = “sGARCH”), distribution.model = “norm”)
spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, robust = FALSE, lag = 0, lag.max = NULL, lag.criterion = c(“AIC”, “HQ”, “SC”, “FPE”), external.regressors = NULL, robust.control = list(“gamma” = 0.25, “delta” = 0.01, “nc” = 10, “ns” = 500), dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c(“mvnorm”, “mvt”)[1], method = c(“Kendall”, “ML”)[2], time.varying = FALSE, transformation = c(“parametric”, “empirical”, “spd”)[1]))
fit1 = cgarchfit(spec1, data = Dat, cluster = cluster, fit.control = list(eval.se=FALSE))
simulationCCC = cgarchsim(fit1, n.sim = 1000, m.sim = 1, startMethod = “sample”, cluster=cluster)

specyfikacja = ugarchspec(variance.model=list(model=“sGARCH”, garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model=“norm”)
dcc_specyfikacja=dccspec(uspec = multispec( replicate(3, specyfikacja)), VAR = TRUE, lag =0, dccOrder = c(1,1), distribution = “mvnorm”)
fit_dcc=dccfit(spec = dcc_specyfikacja, data = simulationCCC@msim$simX[[1]])
simulation=dccsim(fit_dcc,n.sim=?,n.start=?,m.sim=?, rseed=?)

abline(h=median(simulation@msim$simR[[1]][1,2,]),col=“red”)

###################################
# zadanie 4
###################################

simulationCCC = cgarchsim(fit1, n.sim = 3000, m.sim = 1, startMethod = “sample”, cluster=cluster)
y=simulationCCC@msim$simX[[1]][,1:3]
DCCtest(?, garchOrder = ?, n.lags = ?, solver = “solnp”, solver.control = list(), cluster = NULL, Z = NULL)

simulationDCC=dccsim(?)
y=simulationDCC@msim?
DCCtest(?, garchOrder = ?, n.lags = ?, solver = “solnp”, solver.control = list(), cluster = NULL, Z = NULL)

spec=ugarchspec(variance.model=list(model=“sGARCH”, garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, garchInMean = FALSE, inMeanType = 2), distribution.model=“sstd”, fixed.pars=list(mu=0.001,omega=0.00001, alpha1=0.05, beta1=0.90, shape=4,skew=2))
y=ugarchpath(spec, n.sim=3000, n.start=1000, m.sim=3)

