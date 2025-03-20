install.packages("MTS")
library(MTS)
install.packages("psych")
library(psych) #wykorzystaj describe() do obliczenia statystyk opisowych
install.packages("TSA")
library(TSA) #zmiana pakietu do rysowania acf

#ZADANIE 2

dane = read.csv2(file="Wig20_2000do2007.csv",dec=",")
dane_zamk = ts(dane[,4])

#lnrdane=diff(log(dane)) #ln(S_t/S_(t-1))

lnrdane = diff(log(dane_zamk)) #logarytmiczne stopy zwrotu
t(describe(dane_zamk))
t(describe(lnrdane))

#kurtosis  1.293533e+00 -- +3
#se        3.477265e-04 -- prawie 0, a w typowych finansowych szeregach czasowych zwykle jest ujemna (lewy ogon większy niż prawy) - bo cashofobia

#Dlaczego często modelujemy stopy zwrotu, a nie ceny? -- mniejsze zmienności; bo mają pełny przedział liczbowy (-inf, +inf), a zwykłe mają (-1, +inf), więc lepiej się modeluje

par(mfrow = c(2,1))
plot(dane_zamk)
plot(lnrdane)

dane_zamk_2 = dane_zamk^2
lnrdane_2 = lnrdane^2

#Autocorrelation Function
par(mfrow = c(2,1))
acf(lnrdane, drop.lag.0 = TRUE, lag.max = 35, type = "correlation", plot = TRUE)
acf(lnrdane_2, drop.lag.0 = TRUE, lag.max = 35, type = "correlation", plot = TRUE) 

#Box.test() #wersja Ljung; lag = 20 lub do dyskusji
#McLeod.Li.test()
#archTest()

# Test Ljung-Box
Box.test(lnrdane, lag = 20, type = "Ljung-Box")
Box.test(lnrdane^2, lag = 20, type = "Ljung-Box") # p<0.05 - występuje efekt ARCH

# Test McLeod-Li do wykrycia efektu ARCH
wyniki_mcleod_li = McLeod.Li.test(y = lnrdane)
print(wyniki_mcleod_li$p.values) #p<0.05 - zależność w danych (odrzucamy brak niezależności)

# Test ARCH (MTS)
archTest(lnrdane) #p<0.05 - występuje efekt ARCH


