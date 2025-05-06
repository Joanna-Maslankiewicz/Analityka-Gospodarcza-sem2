# Jerzy Marzec, UEK Krakow, II 2025, Mikroekonometria
#Funkcje wlasne w R

# wydruk wynikow do file.csv
Wydruk_csv = function(Model, Plik) {
  b_=as.matrix(Model$coefficients)  # oceny dla parametrow beta
  b_cov=Model$vcov  # estymnator macierzy kowariancji dla parametrow beta
  b_sd = as.matrix( diag(b_cov)^0.5)  # bledy srednie szacunku dla ocen
  iloraz_t=abs(b_/b_sd)  # iloraz t
  pvalue=2*pt(iloraz_t, df=Model$df.residual, lower.tail = FALSE, log.p = FALSE )
  colnames(b_)="OcenaParametru"
  colnames(b_sd)="BladOceny"
  colnames(iloraz_t)="|ocena/blad|"
  colnames(pvalue)="Pr(>|t_emp|)  "
  druk1=cbind(b_, b_sd, iloraz_t, pvalue, b_cov)  

 #  browser() # call and place it, where I want my function to stop

   #  "within"=FE and "individual"
  if (Model$args[1]=="within" & Model$args[2] == "individual"){  # & = AND
    fixEf=as.matrix(fixef(Model, effect="individual", type = c("level")))
    fixEf_kontrasty=as.matrix(fixef(Model, effect="individual", type = c("dmean")))
    colnames(fixEf)="Efekty_indywidualne"
    colnames(fixEf_kontrasty)="Kontrasty"
    write(x="Wyniki estymacji parametrow modelu z indywidualnymi efektami stalymi (w tym V(b^))", 
          file = Plik, append = FALSE, sep = " ")
    druk2=cbind(fixEf, fixEf_kontrasty)    
  }

  #  "within"=FE and "individual" and "time"  
  if (Model$args[1]=="within" & Model$args[2] == "twoways"){  # & = AND
    fixEf_ind=as.matrix(fixef(Model, effect="individual", type = c("level")))
    fixEf_ind_kontrasty=as.matrix(fixef(Model, effect="individual", type = c("dmean")))
    fixEf_time=as.matrix(fixef(Model, effect="time", type = c("level")))
    fixEf_time_kontrasty=as.matrix(fixef(Model, effect="time", type = c("dmean")))
    
    colnames(fixEf_ind)="Efekty_indywidualne"
    colnames(fixEf_ind_kontrasty)="Kontrasty"
    colnames(fixEf_time)="Efekty_czasowe"
    colnames(fixEf_time_kontrasty)="Kontrasty"
    write(x="Wyniki estymacji parametrow modelu z dwoma efektami  stalymi (w tym V(b^))", 
          file = Plik, append = FALSE, sep = " ")
    druk2=cbind(fixEf_ind, fixEf_ind_kontrasty)  
    druk3=cbind(fixEf_time, fixEf_time_kontrasty)  
  }
  
  
  # pooling regression
  if (Model$args[1]=="pooling") {
    write(x="Wyniki estymacji parametrow modelu ze wspolnym wyrazem wolnym (pooling regression) (w tym V(b^))", 
          file = Plik, append = FALSE, sep = " ")
  }  
  # model z efektami losowymi (indywidualnymi)
  if (Model$args[1]=="random") {
    write(x="Wyniki estymacji parametrow modelu z indywidualnymi efektami losowymi (w tym V(b^))", 
          file = Plik, append = FALSE, sep = " ")
  }  
  
  write.table(x=druk1, file = Plik, append = TRUE, sep = ";", dec = ",", col.names = NA)
  write(x=" ", file = Plik, append = TRUE, sep = " ")

  if (Model$args[1]=="within" & Model$args[2] == "individual"){
    write(x=c("Nr_firmy Efekty(alfa_i) oraz kontrasty(odchylenia od beta0) ="), file = Plik, append = TRUE, sep = " ")
    write.table(x=druk2, file = Plik, append = TRUE, sep = ";", dec = ",", col.names = NA)
  }
  
  if (Model$args[1]=="within" & Model$args[2] == "twoways"){
    write(x=c("Nr_firmy Efekty(alfa_i) oraz kontrasty(odchylenia od beta0) ="), file = Plik, append = TRUE, sep = " ")
    write.table(x=druk2, file = Plik, append = TRUE, sep = ";", dec = ",", col.names = NA)
    write(x=" ", file = Plik, append = TRUE, sep = " ")
    write(x=c("Nr_okresu Efekty(lambda_t) oraz kontrasty(odchylenia od beta0) ="), file = Plik, append = TRUE, sep = " ")
    write.table(x=druk3, file = Plik, append = TRUE, sep = ";", dec = ",", col.names = NA)
  }

  if (Model$args[1]=="random") {
    s2_v=as.matrix(Model$ercomp$sigma2[[1]]); colnames(s2_v)="s2_v_FE"  # z modelu FE
    s2_alfa=as.matrix(Model$ercomp$sigma2[[2]]); colnames(s2_alfa)="s2_alfa"
    write(x=c("Wariancje skladnika losowego(z modelu FE) =s2_v (idios) i efektu indywidualnego=s2_alfa (id)"), file = Plik, append = TRUE, sep = " ")
    write.table(x=s2_v, file = Plik, append = TRUE, sep = ";", dec = ",", row.names = FALSE)
    write.table(x=s2_alfa, file = Plik, append = TRUE, sep = ";", dec = ",", row.names = FALSE)
    write(x=c("teta= 1 - (s2_v/(s2_v + T*s2_alfa))^0.5 "), file = Plik, append = TRUE, sep = " ")
    write.table(x=Model$ercomp[2], file = Plik, append = TRUE, sep = ";", dec = ",", row.names = FALSE)
  }  


}
