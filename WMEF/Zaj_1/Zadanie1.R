install.packages("quantmod")
library(quantmod)

getSymbols("BTC-USD", src = "yahoo", from = "2016-03-05", to = "2021-03-05")

BTCUSD <- get("BTC-USD")

head(BTCUSD)

chartSeries(BTCUSD, theme = "white")

#BTC-USD.Open: Cena otwarcia Bitcoina w danym dniu.
#BTC-USD.High: Najwyższa cena Bitcoina w danym dniu.
#BTC-USD.Low: Najniższa cena Bitcoina w danym dniu.
#BTC-USD.Close: Cena zamknięcia Bitcoina w danym dniu.
#BTC-USD.Volume: Wolumen transakcji (liczba jednostek Bitcoina, które zmieniły właściciela w danym dniu).
#BTC-USD.Adjusted: Skorygowana cena zamknięcia (uwzględniająca ewentualne działania korporacyjne, takie jak split akcji, ale w przypadku Bitcoina zwykle jest tożsama z ceną zamknięcia).
