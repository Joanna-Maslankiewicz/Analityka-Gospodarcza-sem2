# Jerzy Marzec, UEK Krakow II 2025, Mikroekonometria
# CTRL +L - czyszczenie konsoli
rm(list=ls())

# require("nazwa_paczki") - sprawdza czy paczka jest zainstalowana
# require() - wczytanie do pamieci lub wymuszanie instalacji, gdy paczka nie zostala zainstalowana

if( !require("readxl")) {install.packages("readxl",dependencies=TRUE)} # ! - indicates logical negation (NOT).
if( !require("plm")) {install.packages("plm",dependencies=TRUE)} # 
#library(readxl)
#library(plm)

plik_xls = "daneR.xlsx"
dane = read_excel(path=plik_xls, sheet = "daneR", range = "A6:G350")
# sortowanie wg farm, gdybysmy zapomnieli to zrobic w Excelu:
dane = dane[order(dane$FMERCODE, dane$YEARDUM), ] # zamiast order(dane[,1], dane[,2])

# dodanie trendu  - alternatywnie mozna wykorzyta? kolumne "dane$YEARDUM"
trend_ = c(1:8)         # 8 - liczba okresow   max(dane[,2])
ones = matrix(1,43,1)   # 43 - liczba farm    max(dane[,1])
trend = ones %x% trend_  # dim = 8*43 x 1 (iloczyn Kroneckera) - trzeba kontrolować A%x%B czy B%x%A
#trend-dane[,2]  # ma byc = 0 

# przygotowanie danych w kontekscie funkcji produkcji
yX_ = dane[, -c(1,2)]
yX_ = log(yX_)
yX_ = cbind(yX_, trend) #trendu nie logarytmujemy

# przygotowanie danych/zmiennych dla plm
indeks_it = dane[,c(1:2)] #tabela z indeksami farm i okresow
colnames(indeks_it) = c("id_farmy", "rok")
y = as.matrix(yX_[,1]) ; colnames(y) = "y"
X = as.matrix(yX_[,-1])


# definicja dla plm zbioru danych zawierajacej id_firmy, okres, y, X-sy (bez jedynki) 
data_plm = cbind(indeks_it, y, X) # X - logarytm nakładów + trend; Y - logarytm produkcji
formula_ = y ~ X

#-----------------------------------------------------------------------------------
# Model z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)
# Obliczenia z pomoca biblioteki plm
# effect = c("individual", "time", "twoways", "nested"),
# model = c("within", "random", "ht", "between", "pooling", "fd"), #within - efekt stały
# y_it = alfa_i + beta*x_it + v_it
Model = plm(formula = formula_, data = data_plm, effect = c("individual"),  #efekt stały - "individual", w modelu - "within"
            model = c("within"), index = c("id_farmy","rok")) # index - (id obiektu, id okresu)

summary(Model) # N = n*T

# Send R output to a file
plik_txt = "Fprod_ryz_Wyniki.txt"
sink(file = plik_txt, append = FALSE)
summary(Model)
sink() # sink(file = NULL)

#wyniki estymacji
beta = as.matrix(Model$coefficients) #ocena estymatora MNK (na wykładzie - bWG)
colnames(beta) = "bWG"

beta_cov = Model$vcov #estymator macierzy kowariancji V(bWG)

beta_sd = as.matrix(diag(beta_cov)^0.5) #sd - standard deviation; diag - wyciąga elementy przekątniowe
colnames(beta_sd) = "sd"

iloraz_t = abs(beta/beta_sd)
colnames(iloraz_t) = "|iloraz_t|"

p_value = as.matrix(2 * pnorm(iloraz_t, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)) # LUB: p_value = as.matrix(2 * (1 - pnorm(iloraz_t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)))
colnames(p_value) = "p_value"

podsumowanie = cbind(beta, beta_sd, iloraz_t, p_value)

plik_csv = "wyniki_estymacji_model1.csv"
# write(x = podsumowanie, append = FALSE, file = plik_csv)

write.table(x = podsumowanie, file = plik_csv, append = FALSE, sep = ";", dec = ",", col.names = FALSE)


