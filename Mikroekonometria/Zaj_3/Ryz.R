rm(list=ls())

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
plik_txt = "Fprod_indyw_ef_stale.txt"
sink(file = plik_txt, append = FALSE)
summary(Model)
sink() # sink(file = NULL)

#wyniki czastkowe
df_ = Model$df.residual #N*T-k-N
RSS_ = as.matrix(as.data.frame(Model$residuals)) #reszty MNK
RSS_ = crossprod(RSS_) #suma kwadratów reszt


#WYNIKI ESTYMACJI
beta = as.matrix(Model$coefficients) #ocena estymatora MNK (na wykładzie - bWG)
colnames(beta) = "bWG"

beta_cov = Model$vcov #estymator macierzy kowariancji V(bWG)

beta_sd = as.matrix(diag(beta_cov)^0.5) #sd - standard deviation; diag - wyciąga elementy przekątniowe
colnames(beta_sd) = "sd"

iloraz_t = abs(beta/beta_sd)
colnames(iloraz_t) = "|iloraz_t|"

p_value = as.matrix(2 * (1-pt(q = iloraz_t, df = df_, lower.tail = TRUE, log.p = FALSE))) # LUB: p_value = as.matrix(2 * (1 - pnorm(iloraz_t, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)))
colnames(p_value) = "p_value"

podsumowanie = cbind(beta, beta_sd, iloraz_t, p_value, beta_cov)

tytul = "Model z indywidualnymi efektami stalymi (ustalonymi) - estymator MNK (FE estimator, Fixed Effect model)"

plik_csv = "wyniki_estymacji_model1_indyw_ef_stale.csv"
write(x = tytul, append = FALSE, file = plik_csv)
write(x = " ", append = TRUE, file = plik_csv)
write.table(x = podsumowanie, file = plik_csv, append = TRUE, sep = ";", dec = ",", col.names = NA)
write(x = c("Liczba stopni swobody = ",df_), file = plik_csv, append =  TRUE)

#wyciągnięcie wspólnego wyrazu wolnego
beta0_ = within_intercept(Model)
beta0 = beta0[[1]]
beta0_sd = attr(beta0_, "se")

write(x = " ", file = plik_csv, append = TRUE)
write(x = "ocena i blad wyrazu wolnego", file = plik_csv, append = TRUE)
write(c(beta0, beta0_sd), file = plik_csv, append = TRUE)

#obliczenie stałych efektów indywidualnych
alfa=as.matrix(fixef(Model, effect = "individual", type = "level") ) #alfa_i
colnames(alfa)= "alfa"   
alfa_kontrast=as.matrix(fixef(Model, effect = "individual", type = "dmean") ) #odchylenie od wspólnego wyrazu wolnego
colnames(alfa_kontrast)= "alfa_kontrast"
# inaczej:
alfa_kontrast_ = alfa - colMeans(alfa) #colMeans(alfa) = beta0
# sprawdzenie:
sum(alfa_kontrast_ - alfa_kontrast) # ma wyjść 0 lub prawie 0 (np. -4.773959e-15)

write(x=c("Oceny stalych  efektow  indywidualnych i kontrastow"), file = plik_csv, append = TRUE)
write(x=c("alfa_kontrast = alfa - colMeans(alfa)"), file = plik_csv, append = TRUE)
write(x=c(" "),   file = plik_csv, append = TRUE)
druk=cbind(alfa, alfa_kontrast)
write.table(x=druk, file = plik_csv, append = TRUE, sep = ";", dec = ",", col.names = NA) #jeśli mamy angielskiego excela, to  dec = "."



# TESTOWANIE
# Dokonać testowania zasadności uwzględnienia efektów stałych (względem modelu ze wspólnym wyrazem wolnym) za pomocą testu F
# H0: y_it = beta0 + beta*x_it + v_it (np. beta0 = alfa_N)
# H1: y_it = alfa_i + beta*x_it + v_it

#-----------------------------------------------------------------------------------
# Model ze wspólnym wyrazem wolnym - pooling model, stosujemy MNK
# model = c("within", "random", "ht", "between", "pooling", "fd"), #within - efekt stały
# y_it = alfa_i + beta*x_it + v_it
Model2 = plm(formula = formula_, data = data_plm, model = "pooling", index = c("id_farmy","rok")) # index - (id obiektu, id okresu)
summary(Model2) # N = n*T

M2_df_ = Model2$df.residual #N*T-k-N

M2_RSS_ = as.matrix(as.data.frame(Model2$residuals)) #reszty MNK
M2_RSS_ = crossprod(M2_RSS_) #suma kwadratów reszt #gorszy model ma wyższe RSS
J = M2_df_ -df_ #liczba restrykcji N-1

F_empiryczne = (M2_RSS_/RSS_ - 1) / (J/df_)
colnames(F_empiryczne) = "Test F"
p_value = pf(q = F_empiryczne, df1 = J, df2 = df_, lower.tail = FALSE, log.p = FALSE)

pFtest(Model, Model2) #panel F test; warunkujemy zawsze według tego samego y; ma być dodatnia - jak jest ujemna to trzeba zamienić argumenty miejscami (chyba się pisze najpierw H1 a potem H0)
p_value = pf(q = F_empiryczne, df1 = J, df2 = df_, lower.tail = FALSE, log.p = FALSE) #powinno wyjść takie samo p-value
#p-value < 0,05 --> efekty są istotne


