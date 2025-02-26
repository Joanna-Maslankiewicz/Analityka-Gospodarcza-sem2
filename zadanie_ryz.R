rm(list=ls())

install.packages("readxl")
library(readxl)
install.packages("plm") # <- pobrać pdf z instrukcją, bo będziemy dużo korzystać
library(plm)

dane = read_excel("ryz_dane.xls", sheet = "dane", range = cell_cols("B:H"))
# czemu nie działa?   dane = dane[order(dane[,2], dane[,1]), ]

yX_ = dane[, -c(1,2)]
yX_ = log(yX_)

### dodawanie trendu do danych

trend_ = c(1:8)
trend = trend_%x%matrix(1,43,1) # <- wymiar 8*43 x 1
#trend-dane[,1] --- powinno wyjść 0

yX_ = cbind(yX_, trend)

### dodawanie dwóch pozostałych wskaźników

indeks_it = dane[,c(1:2)] # <- tabela z indeksami okresu i farmy
colnames(indeks_it) = c("rok","id_farmy")
y = as.matrix(yX_[,1]); colnames(y) = "y"
X = as.matrix(yX_[,-1])

### przygotowanie zmiennych dla PLM

formula_ = y~X
data_plm = cbind(indeks_it, y, X)
#effect = c("individual", "time", "twoways", "nested")
#model = c("within", "random", "ht", "between", "pooling", "fd")

#stałe efekty indywidualne:
Model = plm(
  formula = formula_,
  data = data_plm,
  effect = "individual",
  model = "within",
  index = c("id_farmy","rok")
)

summary(Model)

