
#-------------------------------------------------------------------------------
#                    SYMULOWANE WYZARZANIE PROBLEM KOMIWOJAZERA
#-------------------------------------------------------------------------------


#CZYSZCZENIE
rm(list=ls())

#WCZYTANIE DANYCH
data=read.table("dane3.csv",sep=";",header = TRUE)
data1=read.table("dane3.csv",sep=";",header = TRUE)

dane<-dane1<-as.data.frame(data[,2:length(data[1,])],row.names = data[,1])

ilosc_miast<-length(dane[,1])
ilosc_miast1<-length(dane[,1])+1

#-------------------------------------------------------------------------------
#                    ROZWIAZANIE BAZOWE
#-------------------------------------------------------------------------------


#KOLEJNOSC ODWIEDZANYCH MIAST
kolejnosc_bazowa<-as.data.frame(data[,1])

#MUSI KONCZYC SIE TYM SAMYM MIASTEM W KTORYM JEST POCZATEK
kolejnosc_bazowa[ilosc_miast1,1]<-data[1,1]
#ODLEGLOSC MIEDZY PARAMI KOLEJNYCH MIAST
kolejnosc_bazowa[1,2]<-0
for(i in 1:ilosc_miast){
    wiersz<-kolejnosc_bazowa[i,1]
    kolumna<-kolejnosc_bazowa[i+1,1]
    kolejnosc_bazowa[i+1,2]<-dane[wiersz,kolumna]
}

droga_calkowita<-sum(kolejnosc_bazowa[,2])


#-------------------------------------------------------------------------------
#                    FUNKCJA LICZACA CALKOWITA DROGE
#-------------------------------------------------------------------------------

droga<-function(kol){
  kolejnosc<-as.data.frame(0)
  kolejnosc[1:ilosc_miast1,1]<-kol[1:ilosc_miast1,1]
  kolejnosc[1,2]<-0
  
  for(i in 1:ilosc_miast){
      
      wiersz<-kolejnosc[i,1]
      kolumna<-kolejnosc[i+1,1]
      kolejnosc[i+1,2]<-dane[wiersz,kolumna]
    
  }
  
  droga_calkowita<-sum(kolejnosc[,2])
  
  return(droga_calkowita)
}

#-------------------------------------------------------------------------------
#                    FUNKCJA 1 TWORZACA NOWE ROZWIAZANIE
#-------------------------------------------------------------------------------

nowe_roz1<-function(kolejnosc){
  
  kolejnosc_1<-as.data.frame(0)
  kolejnosc_1[1:ilosc_miast1,1]<-kolejnosc[1:ilosc_miast1,1]
  
  #miasto ktorego lokalizacje chce zmienic
  miasto1<-sample(1:ilosc_miast,1)
  #indeks na jaki miasto bedzie przeniesione
  miasto2<-sample(1:ilosc_miast,1)
  while(miasto2==miasto1){
    miasto1<-sample(1:length(dane1[,1]),1)
    miasto2<-sample(1:length(dane1[,1]),1)
  }
 
  kolejnosc_1[miasto1,]<-kolejnosc[miasto2,]
  kolejnosc_1[miasto2,]<-kolejnosc[miasto1,]
  #ostatnie miasto == pierwsze miasto
  kolejnosc_1[ilosc_miast1,1]<-kolejnosc_1[1,1]

  
  return(kolejnosc_1)
}
#-------------------------------------------------------------------------------
#                    FUNKCJA 2 TWORZACA NOWE ROZWIAZANIE
#-------------------------------------------------------------------------------

nowe_roz<-function(kolejnosc){
kolejnosc_1<-as.data.frame(0)
kolejnosc_1[1:ilosc_miast1,1]<-kolejnosc[1:ilosc_miast1,1]
  
  #miasto ktorego lokalizacje chce zmienic
  miasto<-sample(1:ilosc_miast,1)
  #indeks na jaki miasto bedzie przeniesione
  indeks<-sample(1:ilosc_miast,1)
 # indeks<-48
  kolejnosc_2<-as.data.frame(0)
  

  while(miasto==indeks || abs(miasto-indeks)<2 ){
    miasto<-sample(1:length(dane1[,1]),1)
    indeks<-sample(1:length(dane1[,1]),1)
  }
  if(indeks<miasto){
    
    #1 miasta przed indeksem
    kolejnosc_1[1:indeks-1,1]<-kolejnosc[1:indeks-1,1]
    #2 przeniesienie wylosowanego miasta na indeks
    
    kolejnosc_1[indeks,1]<-kolejnosc[miasto,1]
    row.names(kolejnosc_1)<-NULL
    #3 uzupelnienie nastepnego indeksu sasiadem
    i<-indeks+1
    
    kolejnosc_1[i,1]<-kolejnosc[indeks,1]
    #4 uzupelnienie miast od indeksu do indeksu wylosowanego miasta
    i<-indeks+2
    i1<-indeks+1
    m<-miasto
    m1<-miasto-1
    
    kolejnosc_1[i:m,1]<-kolejnosc[i1:m1,1]
    row.names(kolejnosc_1)<-NULL
    #5 ominiecie wylosowanego miasta i uzupelnienie reszty
    i<-length(kolejnosc[,1])
    m<-miasto+2
    m1<-miasto+1
    
    kolejnosc_1[m:i,1]<- kolejnosc[m:i,1]
    #ostatnie miasto == pierwsze miasto
    kolejnosc_1[i,1]<-kolejnosc_1[1,1]
    kolejnosc_2[1:ilosc_miast1,1]<-kolejnosc_1[1:ilosc_miast1,1]
    

  }else{
    #1 miasta przed miastem
    m<-miasto-1
    kolejnosc_1[1:m,1]<-kolejnosc[1:m,1]
    
    #2 ominiecie wylosowanego miasta i uzupelnienie do indeksu
    i<-indeks-1
    i1<-indeks
    
    m<-miasto
    m1<-miasto+1
    
    kolejnosc_1[m:i,1]<- kolejnosc[m1:i1,1]
    #3 przeniesienie wylosowanego miasta na indeks
    
    kolejnosc_1[indeks,1]<-kolejnosc[miasto,1]
    row.names(kolejnosc_1)<-NULL
    #3 uzupelnienie nastepnego indeksu sasiadem
    i<-indeks+1
    
    kolejnosc_1[i,1]<-kolejnosc[i,1]
    #4 uzupelnienie miast od indeksu do indeksu wylosowanego miasta
    i<-indeks+2
    i1<-indeks+1
    m<-length(kolejnosc[,1])
    kolejnosc_1[i:m,1]<-kolejnosc[i:m,1]
    row.names(kolejnosc_1)<-NULL

    #ostatnie miasto == pierwsze miasto
    kolejnosc_1[m,1]<-kolejnosc_1[1,1]
    kolejnosc_2[1:ilosc_miast1,1]<-kolejnosc_1[1:ilosc_miast1,1]
    
  }
  return(kolejnosc_2)
}


#-------------------------------------------------------------------------------
#                               ALGORYTM
#-------------------------------------------------------------------------------


algorytm<-function(kol,temp){
#kol<-kolejnosc2
  ndane<- kol
  i<- kol
  #parametr temperatury
  t_max<-temp
 # t_max<-0.95
  #koszt rozwiazania i
  f_i<-droga(i)
  
  #propozycja nowego rozwiazania
  
  j<-nowe_roz1(kol)


  #koszt rozwiazania j
  f_j<-droga(j)

  #porownanie kosztow i wybor nizszego
  if(f_j<f_i || f_j==f_i){
    kol[1:ilosc_miast1,1]<-j
  }else if(runif(1)<exp((f_i-f_j)/t_max)){
    kol[1:ilosc_miast1,1]<-j
  }else{
    kol[1:ilosc_miast1,1]<-i
  }
  return(kol)
}


#-------------------------------------------------------------------------------
#                           GLOWNA PETLA
#-------------------------------------------------------------------------------


      
#--------------------------------------------------------------------------------------------------
#ZMIANA TEMPERATURY PoczATKOWEJ

kolejnosc2<-as.data.frame(kolejnosc_bazowa[,1])
temp_pocz<-as.data.frame(kolejnosc_bazowa[,1])
wiersze<-ilosc_miast1+1

for(y in 1:10){
  
  temp=1-y*0.01
  
  for (i in 1:1000){
    wynik<-algorytm(kolejnosc2,temp)
    d<-droga(wynik)
    kolejnosc2<-wynik
    temp<-temp*0.9
    i=i+1
  }
  
  temp_pocz[1:ilosc_miast1,y]<-wynik
  temp_pocz[wiersze,y]<-d
  
}
write.csv(temp_pocz,file = "Wynik1.csv")
#--------------------------------------------------------------------------------------------------
#ZMIANA SZYBKOSCI ZMIENIANIA TEMPERATURY
kolejnosc2<-as.data.frame(kolejnosc_bazowa[,1])
temp_pocz<-as.data.frame(kolejnosc_bazowa[,1])
wiersze<-ilosc_miast1+1

for(y in 1:10){
  
  temp=0.95
  alpha<-(1-y*0.02)
  for (i in 1:1000){
    wynik<-algorytm(kolejnosc2,temp)
    d<-droga(wynik)
    kolejnosc2<-wynik
    temp<-temp*alpha
    i=i+1
  }
  
  temp_pocz[1:ilosc_miast1,y]<-wynik
  temp_pocz[wiersze,y]<-d
  
}
write.csv(temp_pocz,file = "Wynik2.csv")


#--------------------------------------------------------------------------------------------------
#ZMIANA ilosci iteracji
kolejnosc2<-as.data.frame(kolejnosc_bazowa[,1])
temp_pocz<-as.data.frame(kolejnosc_bazowa[,1])
wiersze<-ilosc_miast1+1

for(y in 1:10){
  
  temp=0.95
  iter=100*y
  for (i in 1:iter){
    wynik<-algorytm(kolejnosc2,temp)
    d<-droga(wynik)
    kolejnosc2<-wynik
    temp<-temp*alpha
    i=i+1
  }
  
  temp_pocz[1:ilosc_miast1,y]<-wynik
  temp_pocz[wiersze,y]<-d
  
}
write.csv(temp_pocz,file = "Wynik3.csv")








































